use std::fmt;

/// Binary Gynjo operators.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BinOp {
	As,
	In,
	And,
	Or,
	Eq,
	Neq,
	Approx,
	Lt,
	Leq,
	Gt,
	Geq,
	Add,
	Sub,
	Concat,
}

impl fmt::Display for BinOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			BinOp::As => write!(f, "as"),
			BinOp::In => write!(f, "in"),
			BinOp::And => write!(f, "and"),
			BinOp::Or => write!(f, "or"),
			BinOp::Eq => write!(f, "="),
			BinOp::Neq => write!(f, "!="),
			BinOp::Approx => write!(f, "~"),
			BinOp::Lt => write!(f, "<"),
			BinOp::Leq => write!(f, "<="),
			BinOp::Gt => write!(f, ">"),
			BinOp::Geq => write!(f, ">="),
			BinOp::Add => write!(f, "+"),
			BinOp::Sub => write!(f, "-"),
			BinOp::Concat => write!(f, "|"),
		}
	}
}
