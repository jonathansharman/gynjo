use super::number::Number;

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
		Primitive::Number(n.into())
	}
}

impl From<f64> for Primitive {
	fn from(n: f64) -> Primitive {
		Primitive::Number(n.into())
	}
}

impl From<String> for Primitive {
	fn from(s: String) -> Primitive {
		Primitive::String(s)
	}
}
