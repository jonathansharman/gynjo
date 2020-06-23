use super::env::SharedEnv;
use super::format_with_env::FormatWithEnv;
use super::symbol::Sym;
use super::types::NumType;
use super::values::Val;

use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;
use num_traits::pow::Pow;
use num_traits::sign::Signed;

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Eq, PartialEq, Debug)]
pub enum NumErr {
	DivisionByZero,
	ExponentTooLarge,
	BaseTooLarge,
}

impl fmt::Display for NumErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			NumErr::DivisionByZero => write!(f, "Division by zero"),
			NumErr::ExponentTooLarge => write!(f, "Exponent too large"),
			NumErr::BaseTooLarge => write!(f, "Base in non-integral exponentiation too large"),
		}
	}
}

/// Numeric Gynjo types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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
			},
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
					Ok(result.into())
				},
				None => Err(NumErr::ExponentTooLarge),
			}
		} else {
			let base = BigDecimal::from(self.clone()).to_f64().ok_or(NumErr::BaseTooLarge)?;
			let exponent = BigDecimal::from(rhs).to_f64().ok_or(NumErr::ExponentTooLarge)?;
			Ok(Num::Real(base.pow(exponent).into()).shrink_domain())
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
				let real_string = Val::from(Num::Real(BigDecimal::from(numer) / BigDecimal::from(denom))).format_with_env(env);
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
			},
			Num::Real(real) => {
				let precision = env.lock().unwrap()
					// Look up precision setting.
					.get_var(&Sym { name: "precision".to_string() })
					// Interpret as an integer.
					.and_then(|v| v.as_i64())
					// Interpret as a non-negative integer.
					.and_then(|v| if v < 1 { None } else { Some(v as u64) })
					// If something failed, use default precision setting.
					.unwrap_or(12);
				format!("{}", real.with_prec(precision))
			},
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
		Num::Real(n.into())
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

impl PartialOrd for Num {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self, other) {
			(Num::Real(lhs), rhs @ _) => lhs.partial_cmp(&BigDecimal::from(rhs.clone())),
			(lhs @ _, Num::Real(rhs)) => BigDecimal::from(lhs.clone()).partial_cmp(rhs),
			(Num::Rational(lhs), rhs @ _) => lhs.partial_cmp(&BigRational::from(rhs.clone())),
			(lhs @ _, Num::Rational(rhs)) => BigRational::from(lhs.clone()).partial_cmp(rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => lhs.partial_cmp(rhs),
		}
	}
}

impl Ord for Num {
	fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match (self, other) {
			(Num::Real(lhs), rhs @ _) => lhs.cmp(&BigDecimal::from(rhs.clone())),
			(lhs @ _, Num::Real(rhs)) => BigDecimal::from(lhs.clone()).cmp(rhs),
			(Num::Rational(lhs), rhs @ _) => lhs.cmp(&BigRational::from(rhs.clone())),
			(lhs @ _, Num::Rational(rhs)) => BigRational::from(lhs.clone()).cmp(rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => lhs.cmp(rhs),
		}
	}
}

impl Add for Num {
	type Output = Num;
	fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs @ _) => Num::Real(lhs + BigDecimal::from(rhs)),
			(lhs @ _, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) + rhs),
			(Num::Rational(lhs), rhs @ _) => Num::Rational(lhs + BigRational::from(rhs)),
			(lhs @ _, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) + rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => Num::Integer(lhs + rhs),
		}
	}
}

impl Sub for Num {
	type Output = Num;
	fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs @ _) => Num::Real(lhs - BigDecimal::from(rhs)),
			(lhs @ _, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) - rhs),
			(Num::Rational(lhs), rhs @ _) => Num::Rational(lhs - BigRational::from(rhs)),
			(lhs @ _, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) - rhs),
			(Num::Integer(lhs), Num::Integer(rhs)) => Num::Integer(lhs - rhs),
		}
	}
}

impl Mul for Num {
	type Output = Num;
	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Num::Real(lhs), rhs @ _) => Num::Real(lhs * BigDecimal::from(rhs)).shrink_domain(),
			(lhs @ _, Num::Real(rhs)) => Num::Real(BigDecimal::from(lhs) * rhs).shrink_domain(),
			(Num::Rational(lhs), rhs @ _) => Num::Rational(lhs * BigRational::from(rhs)).shrink_domain(),
			(lhs @ _, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) * rhs).shrink_domain(),
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
				(Num::Real(a), b @ _) => Num::Real(a / BigDecimal::from(b)).shrink_domain(),
				(a @ _, Num::Real(b)) => Num::Real(BigDecimal::from(a) / b).shrink_domain(),
				(Num::Rational(lhs), rhs @ _) => Num::Rational(lhs / BigRational::from(rhs)).shrink_domain(),
				(lhs @ _, Num::Rational(rhs)) => Num::Rational(BigRational::from(lhs) / rhs).shrink_domain(),
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
			},
			Num::Real(real) => real,
		}
	}
}
