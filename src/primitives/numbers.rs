use super::types::NumType;

use crate::env::SharedEnv;
use crate::format_with_env::FormatWithEnv;
use crate::symbol::Sym;
use crate::values::Val;

use bigdecimal::{BigDecimal, FromPrimitive};
use num::{BigInt, BigRational};
use num_traits::cast::ToPrimitive;
use num_traits::pow::Pow;
use num_traits::sign::Signed;

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::hash::Hash;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Eq, PartialEq, Debug)]
pub enum NumErr {
	DivisionByZero,
	ExponentTooLarge,
	BaseTooLarge,
	PowerNotRepresentable,
}

impl fmt::Display for NumErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			NumErr::DivisionByZero => write!(f, "Division by zero"),
			NumErr::ExponentTooLarge => write!(f, "Exponent too large"),
			NumErr::BaseTooLarge => write!(f, "Base in non-integral exponentiation too large"),
			NumErr::PowerNotRepresentable => {
				write!(f, "Result of non-integral exponentiation not representable")
			}
		}
	}
}

/// Numeric Gynjo types.
#[derive(Clone, Debug)]
pub enum Num {
	Integer(BigInt),
	Rational(BigRational),
	Real(BigDecimal),
}

impl Num {
	/// Constructs a rational number from the given numerator and denominator.
	pub fn rational<T: Into<BigInt>>(numer: T, denom: T) -> Num {
		Num::Rational(BigRational::new(numer.into(), denom.into()))
	}

	/// Whether the number is equal to zero in its domain.
	pub fn is_zero(&self) -> bool {
		match self {
			Num::Integer(integer) => integer == &0.into(),
			Num::Rational(rational) => rational == &BigInt::from(0).into(),
			Num::Real(real) => real == &0.into(),
		}
	}

	/// Whether the number is greater than zero in its domain.
	pub fn is_positive(&self) -> bool {
		match self {
			Num::Integer(integer) => integer > &0.into(),
			Num::Rational(rational) => rational > &BigInt::from(0).into(),
			Num::Real(real) => real > &0.into(),
		}
	}

	/// Whether the number is less than zero in its domain.
	pub fn is_negative(&self) -> bool {
		match self {
			Num::Integer(integer) => integer < &0.into(),
			Num::Rational(rational) => rational < &BigInt::from(0).into(),
			Num::Real(real) => real < &0.into(),
		}
	}

	/// Converts this number to the smallest domain that can contain its value.
	pub fn shrink_domain(self) -> Num {
		match self {
			Num::Integer(integer) => Num::Integer(integer),
			Num::Rational(rational) => {
				if rational.is_integer() {
					Num::Integer(rational.to_integer())
				} else {
					Num::Rational(rational)
				}
			}
			Num::Real(real) => {
				if real.is_integer() {
					let (mut mantissa, exponent) = real.into_bigint_and_exponent();
					if exponent > 0 {
						for _ in 0..exponent {
							mantissa /= 10;
						}
					} else {
						for _ in 0..-exponent {
							mantissa *= 10;
						}
					}
					Num::Integer(mantissa)
				} else {
					Num::Real(real)
				}
			}
		}
	}

	/// Converts this number to the largest available domain, `Real`.
	pub fn expand_domain(self) -> Num {
		match self {
			Num::Integer(integer) => Num::Real(integer.into()),
			Num::Rational(rational) => {
				let (numer, denom) = rational.into();
				Num::Real(BigDecimal::from(numer) / BigDecimal::from(denom))
			}
			real @ Num::Real(_) => real,
		}
	}

	/// Computes `self` to the power of `other`.
	pub fn pow(&self, rhs: Self) -> Result<Num, NumErr> {
		if let Num::Integer(exponent) = rhs {
			match exponent.to_i64() {
				Some(mut exponent) => {
					let negative = exponent < 0;
					exponent = exponent.abs();
					let mut base = self.clone();
					let mut result = Num::from(1);
					while exponent > 0 {
						if exponent & 1 == 1 {
							result = result * base.clone();
						}
						base = base.clone() * base;
						exponent /= 2;
					}
					if negative {
						result = (Num::from(1) / result)?;
					}
					Ok(result)
				}
				None => Err(NumErr::ExponentTooLarge),
			}
		} else {
			let base = BigDecimal::from(self.clone())
				.to_f64()
				.ok_or(NumErr::BaseTooLarge)?;
			let exponent = BigDecimal::from(rhs)
				.to_f64()
				.ok_or(NumErr::ExponentTooLarge)?;
			Ok(Num::Real(
				BigDecimal::from_f64(base.pow(exponent)).ok_or(NumErr::PowerNotRepresentable)?,
			)
			.shrink_domain())
		}
	}

	/// Retrives the Gynjo number type of this number.
	pub fn get_type(&self) -> NumType {
		match self {
			Num::Integer(_) => NumType::Integer,
			Num::Rational(_) => NumType::Rational,
			Num::Real(_) => NumType::Real,
		}
	}

	/// Gets `self` as an `i64` if it's integral, otherwise returns `None`.
	pub fn as_i64(&self) -> Option<i64> {
		match self {
			Num::Integer(integer) => integer.to_i64(),
			_ => None,
		}
	}
}

impl PartialEq for Num {
	fn eq(&self, other: &Self) -> bool {
		match (self, other) {
			(Num::Real(lhs), rhs) => lhs == &BigDecimal::from(rhs.clone()),
			(lhs, Num::Real(rhs)) => &BigDecimal::from(lhs.clone()) == rhs,
			(Num::Rational(lhs), rhs) => lhs == &BigRational::from(rhs.clone()),
			(lhs, Num::Rational(rhs)) => &BigRational::from(lhs.clone()) == rhs,
			(Num::Integer(lhs), Num::Integer(rhs)) => lhs == rhs,
		}
	}
}

impl Eq for Num {}

impl Ord for Num {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match (self, other) {
			(Num::Real(lhs), rhs) => lhs.cmp(&BigDecimal::from(rhs.clone())),
			(lhs, Num::Real(rhs)) => BigDecimal::from(lhs.clone()).cmp(rhs),
			(Num::Rational(lhs), rhs) => lhs.cmp(&BigRational::from(rhs.clone())),
			(lhs, Num::Rational(rhs)) => BigRational::from(lhs.clone()).cmp(rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => lhs.cmp(rhs),
		}
	}
}

impl PartialOrd for Num {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		Some(self.cmp(other))
	}
}

impl Hash for Num {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		match self {
			Num::Real(n) => n.hash(state),
			Num::Rational(n) => n.hash(state),
			Num::Integer(n) => n.hash(state),
		}
	}
}

impl fmt::Display for Num {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Num::Integer(n) => n.fmt(f),
			Num::Rational(n) => n.fmt(f),
			Num::Real(n) => n.fmt(f),
		}
	}
}

impl FormatWithEnv for Num {
	fn format_with_env(&self, env: &SharedEnv) -> String {
		match self {
			Num::Integer(integer) => integer.to_string(),
			Num::Rational(rational) => {
				// Rational numbers are displayed both in rational and real form.
				let (numer, denom) = rational.clone().into();
				let real_string =
					Val::from(Num::Real(BigDecimal::from(numer) / BigDecimal::from(denom)))
						.format_with_env(env);
				// Rationals are displayed in proper form.
				let whole_part = rational.trunc();
				let fractional_part = rational.fract();
				if &whole_part == rational {
					// No fractional part.
					format!("{} ({})", whole_part, real_string)
				} else if &fractional_part == rational {
					// No whole part.
					format!("{} ({})", fractional_part, real_string)
				} else {
					// Whole and fractional parts. Ensure fractional part is displayed as positive.
					format!("{} {} ({})", whole_part, fractional_part.abs(), real_string)
				}
			}
			Num::Real(real) => {
				let precision = env
					.lock()
					.unwrap()
					// Look up precision setting.
					.get_var(&Sym {
						name: "precision".to_string(),
					})
					// Interpret as an integer.
					.and_then(|v| v.as_i64())
					// Interpret as a non-negative integer.
					.and_then(|v| if v < 1 { None } else { Some(v as u64) })
					// If something failed, use default precision setting.
					.unwrap_or(12);
				format!("{}", real.with_prec(precision).normalized())
			}
		}
	}
}

impl From<i64> for Num {
	fn from(n: i64) -> Num {
		Num::Integer(n.into())
	}
}

impl From<f64> for Num {
	fn from(n: f64) -> Num {
		BigDecimal::from_f64(n).map(Num::Real).unwrap()
	}
}

impl From<BigInt> for Num {
	fn from(n: BigInt) -> Num {
		Num::Integer(n)
	}
}

impl From<BigRational> for Num {
	fn from(n: BigRational) -> Num {
		Num::Rational(n)
	}
}

impl From<BigDecimal> for Num {
	fn from(n: BigDecimal) -> Num {
		Num::Real(n)
	}
}

impl Add for Num {
	type Output = Num;
	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs) => Num::Real(lhs + BigDecimal::from(rhs)),
			(lhs, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) + rhs),
			(Num::Rational(lhs), rhs) => Num::Rational(lhs + BigRational::from(rhs)),
			(lhs, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) + rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => Num::Integer(lhs + rhs),
		}
	}
}

impl Sub for Num {
	type Output = Num;
	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs) => Num::Real(lhs - BigDecimal::from(rhs)),
			(lhs, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) - rhs),
			(Num::Rational(lhs), rhs) => Num::Rational(lhs - BigRational::from(rhs)),
			(lhs, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) - rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => Num::Integer(lhs - rhs),
		}
	}
}

impl Mul for Num {
	type Output = Num;
	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs) => Num::Real(lhs * BigDecimal::from(rhs)).shrink_domain(),
			(lhs, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) * rhs).shrink_domain(),
			(Num::Rational(lhs), rhs) => {
				Num::Rational(lhs * BigRational::from(rhs)).shrink_domain()
			}
			(lhs, Num::Rational(rhs)) => {
				Num::Rational(BigRational::from(lhs) * rhs).shrink_domain()
			}
			(Num::Integer(lhs), Num::Integer(rhs)) => Num::Integer(lhs * rhs),
		}
	}
}

impl Div for Num {
	type Output = Result<Num, NumErr>;
	fn div(self, rhs: Self) -> Self::Output {
		if rhs.is_zero() {
			Err(NumErr::DivisionByZero)
		} else {
			Ok(match (self, rhs) {
				(Num::Real(a), b) => Num::Real(a / BigDecimal::from(b)).shrink_domain(),
				(a, Num::Real(b)) => Num::Real(BigDecimal::from(a) / b).shrink_domain(),
				(Num::Rational(lhs), rhs) => {
					Num::Rational(lhs / BigRational::from(rhs)).shrink_domain()
				}
				(lhs, Num::Rational(rhs)) => {
					Num::Rational(BigRational::from(lhs) / rhs).shrink_domain()
				}
				(Num::Integer(a), Num::Integer(b)) => Num::rational(a, b).shrink_domain(),
			})
		}
	}
}

impl Neg for Num {
	type Output = Num;
	fn neg(self) -> Self::Output {
		match self {
			Num::Integer(integer) => Num::Integer(-integer),
			Num::Rational(rational) => Num::Rational(-rational),
			Num::Real(real) => Num::Real(-real),
		}
	}
}

impl From<Num> for BigRational {
	fn from(number: Num) -> BigRational {
		match number {
			Num::Integer(integer) => integer.into(),
			Num::Rational(rational) => rational,
			Num::Real(_) => panic!("attempted to convert real number to rational"),
		}
	}
}

impl From<Num> for BigDecimal {
	fn from(number: Num) -> BigDecimal {
		match number {
			Num::Integer(integer) => integer.into(),
			Num::Rational(rational) => {
				let (numer, denom) = rational.into();
				BigDecimal::from(numer) / BigDecimal::from(denom)
			}
			Num::Real(real) => real,
		}
	}
}
