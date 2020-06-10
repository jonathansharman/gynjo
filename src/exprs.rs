use super::intrinsics::Intrinsic;
use super::primitives::Prim;
use super::symbol::Sym;

use itertools::Itertools;

use std::boxed::Box;
use std::collections::VecDeque;
use std::fmt;

/// Binary Gynjo operators.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum BinOp {
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
}

impl fmt::Display for BinOp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
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
		}
    }
}

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

/// The way in which a cluster item is attached to the preceding element of the cluster.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ClusterConnector {
	/// Item is the first in the cluster.
	None,
	/// Adjacent value enclosed in parentheses
	AdjParen,
	/// Adjacent value not enclosed in parentheses
	AdjNonparen,
	/// Explicit multiplication
	Mul,
	/// Explicit division
	Div,
	/// Explicit exponentiation
	Exp,
}

/// A single item in an expression `Cluster`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ClusterItem {
	/// This item's expression.
	pub expr: Box<Expr>,
	/// Determines whether this item is negated.
	pub negated: bool,
	/// How this item is connected to the previous item.
	pub connector: ClusterConnector,
}

impl fmt::Display for ClusterItem {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}{}{}",
			if self.negated { "-" } else { "" },
			match self.connector {
				ClusterConnector::None => "",
				ClusterConnector::AdjParen => " (",
				ClusterConnector::AdjNonparen => " ",
				ClusterConnector::Mul => " * ",
				ClusterConnector::Div => " / ",
				ClusterConnector::Exp => " ^ ",
			},
			if let ClusterConnector::AdjParen = self.connector { ")" } else { "" },
			self.expr
		)
    }
}

/// A cluster of function calls, exponentiations, (possibly implicit) multiplications, and/or divisions.
///
/// This large grouping of operations is as fine-grained as possible in the parsing stage. Breaking this down
/// into specific operations requires additional parsing in the evaluation stage since determining the order of
/// operations requires type info.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cluster {
	pub negated: bool,
	pub items: Vec<ClusterItem>,
}

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
			LambdaBody::UserDefined(body) => write!(f, "(({}) -> {})", self.params.iter().map(|s| s.name.clone()).join(", "), body),
			LambdaBody::Intrinsic(intrinsic) => intrinsic.fmt(f),
		}
    }
}

/// Gynjo expressions.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
	Block { exprs: Box<Vec<Expr>> },
	BinExpr(BinExpr),
	Not { expr: Box<Expr> },
	Cluster(Cluster),
	Lambda(Lambda),
	TupleExpr(Box<Vec<Expr>>),
	ListExpr(Box<VecDeque<Expr>>),
	Sym(Sym),
	Prim(Prim),
	Import { filename: String },
	Assign {
		lhs: Sym,
		rhs: Box<Expr>,
	},
	Branch {
		test: Box<Expr>,
		then_expr: Box<Expr>,
		else_expr: Box<Expr>,
	},
	WhileLoop {
		test: Box<Expr>,
		body: Box<Expr>,
	},
	ForLoop {
		loop_var: Sym,
		range: Box<Expr>,
		body: Box<Expr>,
	},
	Return { result: Box<Expr> },
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expr::Block { exprs } => write!(f, "{{ {} }}", exprs.iter().map(Expr::to_string).join("; ")),
			Expr::BinExpr(binary_expr) => binary_expr.fmt(f),
			Expr::Not { expr } => write!(f, "(not {})", expr),
			Expr::Cluster(cluster) => write!(f, "({})", cluster.items.iter().map(ClusterItem::to_string).join(" ")),
			Expr::Lambda(lambda) => lambda.fmt(f),
			Expr::TupleExpr(exprs) => write!(f, "({})", exprs.iter().map(Expr::to_string).join(", ")),
			Expr::ListExpr(exprs) => write!(f, "[{}]", exprs.iter().map(Expr::to_string).join(", ")),
			Expr::Sym(symbol) => symbol.fmt(f),
			Expr::Prim(primitive) => primitive.fmt(f),
			Expr::Import { filename } => write!(f, "import \"{}\"", filename),
			Expr::Assign { lhs, rhs } => write!(f, "let {} = {}", lhs.name, rhs),
			Expr::Branch { test, then_expr, else_expr } => {
				write!(f, "if {} then {} else {}", test, then_expr, else_expr)
			},
			Expr::WhileLoop { test, body } => write!(f, "while {} do {}", test, body),
			Expr::ForLoop { loop_var, range, body } => {
				write!(f, "for {} in {} do {}", loop_var.name, range, body)
			},
			Expr::Return { result } => write!(f, "return {}", result),
		}
    }
}
