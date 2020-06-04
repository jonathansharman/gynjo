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
	// Type conversions
	ToReal,
}

impl Intrinsic {
	/// The intrinsic's user-readable name.
	pub fn name(&self) -> String {
		match self {
			Intrinsic::Top => "top".to_string(),
			Intrinsic::Pop => "pop".to_string(),
			Intrinsic::Push => "push".to_string(),
			Intrinsic::Print => "print".to_string(),
			Intrinsic::Read => "read".to_string(),
			Intrinsic::ToReal => "real".to_string(),
		}
	}
}
