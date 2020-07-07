use super::{BinOp, Expr};

use std::fmt;

/// Binary Gynjo expressions.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct BinExpr {
	pub op: BinOp,
	pub left: Box<Expr>,
	pub right: Box<Expr>,
}

impl fmt::Display for BinExpr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "({} {} {})", self.left, self.op, self.right)
	}
}
