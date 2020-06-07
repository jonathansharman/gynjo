use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::cast::ToPrimitive;

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

#[derive(Debug)]
pub enum NumError {
	DivisionByZero,
	ExponentTooLarge,
	NonIntegralExponentsNotSupported,
}

impl fmt::Display for NumError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
			NumError::DivisionByZero => write!(f, "Division by zero"),
			NumError::ExponentTooLarge => write!(f, "Exponent too large"),
			NumError::NonIntegralExponentsNotSupported => write!(f, "Non-integral exponentiation not yet supported"),
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
	/// Converts this number to the smallest domain that can contain its value.
	pub fn shrink_domain(self) -> Num {
		match self {
			Num::Integer(integer) => Num::Integer(integer),
			Num::Rational(rational) => {
				if rational.is_integer() {
					Num::Integer(rational.numer().clone())
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

	/// Constructs a rational number from the given numerator and denominator.
	pub fn rational<T: Into<BigInt>>(numer: T, denom: T) -> Num {
		Num::Rational(BigRational::new(numer.into(), denom.into()))
	}

	/// Computes `self` to the power of `other`.
	pub fn pow(self, other: Self) -> Result<Num, NumError> {
		match (self, other) {
			(base @ _, Num::Integer(exponent)) => {
				match exponent.to_i64() {
					Some(exponent) => {
						let negative = exponent < 0;
						let mut result = Num::from(1);
						for _ in 0..exponent.abs() {
							result = result * base.clone();
						}
						if negative {
							result = (Num::from(1) / result)?;
						}
						Ok(result.into())
					},
					None => Err(NumError::ExponentTooLarge),
				}
			},
			_ => Err(NumError::NonIntegralExponentsNotSupported),
		}
	}
}

impl fmt::Display for Num {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Num::Integer(n) => write!(f, "{}", n),
			Num::Rational(n) => write!(f, "{}", n),
			Num::Real(n) => write!(f, "{}", n),
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
	type Output = Result<Num, NumError>;
	fn div(self, rhs: Self) -> Self::Output {
		if BigDecimal::from(rhs.clone()) == BigDecimal::from(0) {
			Err(NumError::DivisionByZero)
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
			Num::Rational(rational) => BigDecimal::from(rational.numer().clone()) / BigDecimal::from(rational.denom().clone()),
			Num::Real(real) => real,
		}
	}
}
