use super::number::Num;

/// Boolean Gynjo type.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Bool { True, False }

impl Bool {
	pub fn to_string(&self) -> String {
		match self {
			Bool::True => "true", 
			Bool::False => "false",
		}.to_string()
	}
}

/// Gynjo boolean to Rust boolean.
impl From<Bool> for bool {
	fn from(boolean: Bool) -> Self {
		match boolean {
			Bool::True => true,
			Bool::False => false,
		}
	}
}

/// Rust boolean to Gynjo boolean.
impl From<bool> for Bool {
	fn from(boolean: bool) -> Self {
		if boolean { Bool::True } else { Bool::False }
	}
}

/// Primitive Gynjo value types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Prim {
	Bool(Bool),
	Num(Num),
	String(String),
}

impl Prim {
	/// Converts this primitive to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Prim::Bool(b) => b.to_string(),
			Prim::Num(n) => n.to_string(),
			Prim::String(s) => format!("\"{}\"", s),
		}
	}
}

impl From<bool> for Prim {
	fn from(b: bool) -> Prim {
		Prim::Bool(Bool::from(b))
	}
}

impl From<i64> for Prim {
	fn from(n: i64) -> Prim {
		Prim::Num(n.into())
	}
}

impl From<f64> for Prim {
	fn from(n: f64) -> Prim {
		Prim::Num(n.into())
	}
}

impl From<String> for Prim {
	fn from(s: String) -> Prim {
		Prim::String(s)
	}
}
