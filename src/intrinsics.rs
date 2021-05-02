use std::fmt;

/// Represents an intrinsic Gynjo function or its body, depending on context.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Intrinsic {
	// Fundamental list operations
	Pop,
}

impl fmt::Display for Intrinsic {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Intrinsic::Pop => write!(f, "pop"),
		}
	}
}
