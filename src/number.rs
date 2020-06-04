use bigdecimal::BigDecimal;
use num_bigint::BigInt;
use num_rational::BigRational;

use std::cmp::{Ord, PartialOrd};
use std::ops::Add;
use std::ops::Div;
use std::ops::Mul;
use std::ops::Sub;

/// Numeric Gynjo types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Number {
	Integer(BigInt),
	Rational(BigRational),
	Real(BigDecimal),
}

impl Number {
	/// Converts this number to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Number::Integer(n) => n.to_string(),
			Number::Rational(n) => n.to_string(),
			Number::Real(n) => n.to_string(),
		}
	}

	/// Converts this number to the smallest domain that can contain its value.
	pub fn shrink_domain(self) -> Number {
		match self {
			Number::Integer(integer) => Number::Integer(integer),
			Number::Rational(rational) => {
				if rational.is_integer() {
					Number::Integer(rational.numer().clone())
				} else {
					Number::Rational(rational)
				}
			}
			Number::Real(real) => {
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
					Number::Integer(mantissa)
				} else {
					Number::Real(real)
				}
			}
		}
	}

	/// Constructs a rational number from the given numerator and denominator.
	pub fn rational<T: Into<BigInt>>(numer: T, denom: T) -> Number {
		Number::Rational(BigRational::new(numer.into(), denom.into()))
	}
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		match (self, other) {
			(Number::Real(lhs), rhs @ _) => lhs.partial_cmp(&BigDecimal::from(rhs.clone())),
			(lhs @ _, Number::Real(rhs)) => BigDecimal::from(lhs.clone()).partial_cmp(rhs),
			(Number::Rational(lhs), rhs @ _) => lhs.partial_cmp(&BigRational::from(rhs.clone())),
			(lhs @ _, Number::Rational(rhs)) => BigRational::from(lhs.clone()).partial_cmp(rhs),
			(Number::Integer(lhs), Number::Integer(rhs)) => lhs.partial_cmp(rhs),
		}
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
		match (self, other) {
			(Number::Real(lhs), rhs @ _) => lhs.cmp(&BigDecimal::from(rhs.clone())),
			(lhs @ _, Number::Real(rhs)) => BigDecimal::from(lhs.clone()).cmp(rhs),
			(Number::Rational(lhs), rhs @ _) => lhs.cmp(&BigRational::from(rhs.clone())),
			(lhs @ _, Number::Rational(rhs)) => BigRational::from(lhs.clone()).cmp(rhs),
			(Number::Integer(lhs), Number::Integer(rhs)) => lhs.cmp(rhs),
		}
    }
}

impl Add for Number {
    type Output = Number;
    fn add(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Number::Real(lhs), rhs @ _) => Number::Real(lhs + BigDecimal::from(rhs)),
			(lhs @ _, Number::Real(rhs)) => Number::Real(BigDecimal::from(lhs) + rhs),
			(Number::Rational(lhs), rhs @ _) => Number::Rational(lhs + BigRational::from(rhs)),
			(lhs @ _, Number::Rational(rhs)) => Number::Rational(BigRational::from(lhs) + rhs),
			(Number::Integer(lhs), Number::Integer(rhs)) => Number::Integer(lhs + rhs),
		}
    }
}

impl Sub for Number {
    type Output = Number;
    fn sub(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Number::Real(lhs), rhs @ _) => Number::Real(lhs - BigDecimal::from(rhs)),
			(lhs @ _, Number::Real(rhs)) => Number::Real(BigDecimal::from(lhs) - rhs),
			(Number::Rational(lhs), rhs @ _) => Number::Rational(lhs - BigRational::from(rhs)),
			(lhs @ _, Number::Rational(rhs)) => Number::Rational(BigRational::from(lhs) - rhs),
			(Number::Integer(lhs), Number::Integer(rhs)) => Number::Integer(lhs - rhs),
		}
    }
}

impl Mul for Number {
	type Output = Number;
	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(Number::Real(lhs), rhs @ _) => Number::Real(lhs * BigDecimal::from(rhs)).shrink_domain(),
			(lhs @ _, Number::Real(rhs)) => Number::Real(BigDecimal::from(lhs) * rhs).shrink_domain(),
			(Number::Rational(lhs), rhs @ _) => Number::Rational(lhs * BigRational::from(rhs)).shrink_domain(),
			(lhs @ _, Number::Rational(rhs)) => Number::Rational(BigRational::from(lhs) * rhs).shrink_domain(),
			(Number::Integer(lhs), Number::Integer(rhs)) => Number::Integer(lhs * rhs),
		}
	}
}

impl Div for Number {
	type Output = Option<Number>;
	fn div(self, rhs: Self) -> Self::Output {
		if BigDecimal::from(rhs.clone()) == BigDecimal::from(0) {
			None
		} else {
			Some(match (self, rhs) {
				(Number::Real(a), b @ _) => Number::Real(a / BigDecimal::from(b)).shrink_domain(),
				(a @ _, Number::Real(b)) => Number::Real(BigDecimal::from(a) / b).shrink_domain(),
				(Number::Rational(lhs), rhs @ _) => Number::Rational(lhs / BigRational::from(rhs)).shrink_domain(),
				(lhs @ _, Number::Rational(rhs)) => Number::Rational(BigRational::from(lhs) / rhs).shrink_domain(),
				(Number::Integer(a), Number::Integer(b)) => Number::rational(a, b).shrink_domain(),
			})
		}
	}
}

impl From<Number> for BigRational {
	fn from(number: Number) -> BigRational {
		match number {
			Number::Integer(integer) => integer.into(),
			Number::Rational(rational) => rational,
			Number::Real(_) => panic!("attempted to convert real number to rational"),
		}
	}
}

impl From<Number> for BigDecimal {
	fn from(number: Number) -> BigDecimal {
		match number {
			Number::Integer(integer) => integer.into(),
			Number::Rational(rational) => BigDecimal::from(rational.numer().clone()) / BigDecimal::from(rational.denom().clone()),
			Number::Real(real) => real,
		}
	}
}
