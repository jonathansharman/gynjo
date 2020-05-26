/// Represents an intrinsic Gynjo function or its body, depending on context.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Intrinsic {
	// Fundamental list operations
	Top,
	Pop,
	Push,
	// I/O
	Print,
	Read,
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
		}
	}
}
