use bigdecimal::BigDecimal;
use num_bigint::BigInt;

/// Boolean Gynjo type.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Boolean { True, False }

impl Boolean {
	pub fn to_string(&self) -> String {
		match self {
			Boolean::True => "true", 
			Boolean::False => "false",
		}.to_string()
	}
}

/// Gynjo boolean to Rust boolean.
impl From<Boolean> for bool {
	fn from(boolean: Boolean) -> Self {
		match boolean {
			Boolean::True => true,
			Boolean::False => false,
		}
	}
}

/// Rust boolean to Gynjo boolean.
impl From<bool> for Boolean {
	fn from(boolean: bool) -> Self {
		if boolean { Boolean::True } else { Boolean::False }
	}
}

/// Numeric Gynjo types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Number {
	Integer(BigInt),
	Real(BigDecimal),
}

impl Number {
	/// Converts this number to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Number::Integer(n) => n.to_string(),
			Number::Real(n) => n.to_string(),
		}
	}

	/// Converts this number to a `Number::Integer` if its value is integral.
	pub fn to_integer_if_integral(self) -> Number {
		match self {
			Number::Integer(integer) => Number::Integer(integer),
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
}

impl From<Number> for BigDecimal {
	fn from(number: Number) -> BigDecimal {
		match number {
			Number::Integer(integer) => integer.into(),
			Number::Real(real) => real,
		}
	}
}

/// Primitive Gynjo value types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Primitive {
	Boolean(Boolean),
	Number(Number),
	String(String),
}

impl Primitive {
	/// Converts this primitive to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Primitive::Boolean(b) => b.to_string(),
			Primitive::Number(n) => n.to_string(),
			Primitive::String(s) => format!("\"{}\"", s),
		}
	}
}

impl From<bool> for Primitive {
	fn from(b: bool) -> Primitive {
		Primitive::Boolean(Boolean::from(b))
	}
}

impl From<i64> for Primitive {
	fn from(n: i64) -> Primitive {
		Primitive::Number(Number::Integer(n.into()))
	}
}

impl From<f64> for Primitive {
	fn from(n: f64) -> Primitive {
		Primitive::Number(Number::Real(n.into()))
	}
}

impl From<String> for Primitive {
	fn from(s: String) -> Primitive {
		Primitive::String(s)
	}
}
