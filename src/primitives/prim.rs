use super::Text;
use super::Type;
use super::Num;

use std::fmt;

/// Boolean Gynjo type.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Bool { True, False }

impl fmt::Display for Bool {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Bool::True => write!(f, "true"), 
			Bool::False => write!(f, "false"),
		}
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
	Text(Text),
	Type(Type),
}

impl fmt::Display for Prim {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Prim::Bool(b) => b.fmt(f),
			Prim::Num(n) => n.fmt(f),
			Prim::Text(t) => t.fmt(f),
			Prim::Type(t) => t.fmt(f),
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
		Prim::Text(s.into())
	}
}
