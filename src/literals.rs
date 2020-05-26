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

/// Sum type of Gynjo literal values.
#[derive(Clone, Eq, PartialEq)]
pub enum Literal {
	Boolean(Boolean),
	Number(BigDecimal),
	String(String),
}

impl Literal {
	/// Converts this literal to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Literal::Boolean(b) => b.to_string(),
			Literal::Number(n) => n.to_string(),
			Literal::String(s) => format!("\"{}\"", s),
		}
	}
}
