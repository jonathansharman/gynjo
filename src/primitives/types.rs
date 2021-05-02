use std::fmt;

/// Gynjo numeric types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
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

/// Gynjo type values.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
	Type,
	Boolean,
	Quant(NumType),
	Text,
	Tuple,
	List,
	Range,
	Closure,
	Break,
	Return,
}

impl Type {
	/// A vector containing the possible quantity types.
	pub fn quant_types() -> Vec<Type> {
		vec![
			Type::Quant(NumType::Integer),
			Type::Quant(NumType::Rational),
			Type::Quant(NumType::Real),
		]
	}
}

impl fmt::Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::Type => write!(f, "type"),
			Type::Boolean => write!(f, "boolean"),
			Type::Quant(num_type) => num_type.fmt(f),
			Type::Text => write!(f, "text"),
			Type::Tuple => write!(f, "tuple"),
			Type::List => write!(f, "list"),
			Type::Range => write!(f, "range"),
			Type::Closure => write!(f, "closure"),
			Type::Break => write!(f, "break_value"),
			Type::Return => write!(f, "return_value"),
		}
	}
}
