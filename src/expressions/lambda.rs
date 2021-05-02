use super::Expr;

use crate::intrinsics::Intrinsic;
use crate::symbol::Sym;

use itertools::Itertools;

use std::fmt;

/// Lambda bodies can be user-defined expressions or intrinsic function bodies.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum LambdaBody {
	UserDefined(Box<Expr>),
	Intrinsic(Intrinsic),
}

/// A function expression.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Lambda {
	pub params: Vec<Sym>,
	pub body: LambdaBody,
}

impl fmt::Display for Lambda {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self.body {
			LambdaBody::UserDefined(body) => write!(
				f,
				"(({}) -> {})",
				self.params.iter().map(|s| s.name.clone()).join(", "),
				body
			),
			LambdaBody::Intrinsic(intrinsic) => intrinsic.fmt(f),
		}
	}
}
