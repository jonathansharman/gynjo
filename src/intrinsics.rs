use std::fmt;

/// Represents an intrinsic Gynjo function or its body, depending on context.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Intrinsic {
	// Fundamental list operations
	Top,
	Pop,
	Push,
	// I/O
	Print,
	Read,
	// Type operations
	GetType,
	ToReal,
}

impl fmt::Display for Intrinsic {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Intrinsic::Top => write!(f, "top"),
			Intrinsic::Pop => write!(f, "pop"),
			Intrinsic::Push => write!(f, "push"),
			Intrinsic::Print => write!(f, "print"),
			Intrinsic::Read => write!(f, "read"),
			Intrinsic::GetType => write!(f, "get_type"),
			Intrinsic::ToReal => write!(f, "real"),
		}
    }
}
