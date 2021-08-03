use std::fmt;

/// Gynjo numeric types.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum NumType {
	Integer,
	Rational,
	Real,
}

impl fmt::Display for NumType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			NumType::Integer => write!(f, "integer"),
			NumType::Rational => write!(f, "rational"),
			NumType::Real => write!(f, "real"),
		}
	}
}

/// Primitive Gynjo types.
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum PrimType {
	Any,
	Type,
	Boolean,
	Quant(NumType),
	Text,
	Break,
	Return,
}

impl PrimType {
	/// A vector containing the possible quantity types.
	pub fn quant_types() -> Vec<PrimType> {
		vec![
			PrimType::Quant(NumType::Integer),
			PrimType::Quant(NumType::Rational),
			PrimType::Quant(NumType::Real),
		]
	}
}

impl fmt::Display for PrimType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		use PrimType::*;
		match self {
			Any => write!(f, "any"),
			Type => write!(f, "type"),
			Boolean => write!(f, "boolean"),
			Quant(num_type) => num_type.fmt(f),
			Text => write!(f, "text"),
			Tuple => write!(f, "tuple"),
			List => write!(f, "list"),
			Range => write!(f, "range"),
			Closure => write!(f, "closure"),
			Break => write!(f, "break_value"),
			Return => write!(f, "return_value"),
		}
	}
}
