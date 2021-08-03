use super::PrimType;

use std::fmt;

/// Gynjo types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
	Prim(PrimType),
	Text,
	Tuple,
	List,
	Range,
	Closure,
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use Type::*;
		match self {
			Prim(prim) => prim.fmt(f),
			Text => write!(f, "text"),
			Tuple => write!(f, "tuple"),
			List => write!(f, "list"),
			Range => write!(f, "range"),
			Closure => write!(f, "closure"),
		}
	}
}
