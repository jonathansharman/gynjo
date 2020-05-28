use bigdecimal::BigDecimal;

/// Boolean Gynjo value.
#[derive(Copy, Clone, Eq, PartialEq, Hash)]
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

/// Sum type of Gynjo primitive values.
#[derive(Clone, Eq, PartialEq, Hash)]
pub enum Primitive {
	Boolean(Boolean),
	Number(BigDecimal),
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
