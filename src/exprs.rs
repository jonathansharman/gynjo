use super::intrinsics::Intrinsic;
use super::literals::Literal;
use super::stmts::Stmt;
use super::symbol::Symbol;

use itertools::Itertools;

use std::boxed::Box;

/// Sum type of binary Gynjo operators.
#[derive(Copy, Clone, Eq, PartialEq)]
pub enum BinaryOp {
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

impl BinaryOp {
	pub fn to_string(&self) -> String {
		match self {
			BinaryOp::And => "and",
			BinaryOp::Or => "or",
			BinaryOp::Eq => "=",
			BinaryOp::Neq => "!=",
			BinaryOp::Approx => "~",
			BinaryOp::Lt => "<",
			BinaryOp::Leq => "<=",
			BinaryOp::Gt => ">",
			BinaryOp::Geq => ">=",
			BinaryOp::Add => "+",
			BinaryOp::Sub => "-",
		}.to_string()
	}
}

/// Sum type of binary Gynjo expressions.
#[derive(Clone, Eq, PartialEq)]
pub struct BinaryExpr {
	pub op: BinaryOp,
	pub left: Box<Expr>,
	pub right: Box<Expr>,
}

impl BinaryExpr {
	pub fn to_string(&self) -> String {
		format!("({} {} {})", self.left.to_string(), self.op.to_string(), self.right.to_string())
	}
}

/// The way in which a cluster item is attached to the preceding element of the cluster.
#[derive(Copy, Clone, Eq, PartialEq)]
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
#[derive(Clone, Eq, PartialEq)]
pub struct ClusterItem {
	/// This item's expression.
	pub expr: Box<Expr>,
	/// Determines whether this item is negated.
	pub negated: bool,
	/// How this item is connected to the previous item.
	pub connector: ClusterConnector,
}

impl ClusterItem {
	fn to_string(&self) -> String {
		format!(
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
			self.expr.to_string()
		)
	}
}

/// A cluster of function calls, exponentiations, (possibly implicit) multiplications, and/or divisions.
///
/// This large grouping of operations is as fine-grained as possible in the parsing stage. Breaking this down
/// into specific operations requires additional parsing in the evaluation stage since determining the order of
/// operations requires type info.
#[derive(Clone, Eq, PartialEq)]
pub struct Cluster {
	pub items: Vec<ClusterItem>,
}

/// Lambda bodies can be user-defined expressions or intrinsic function bodies.
#[derive(Clone, Eq, PartialEq)]
pub enum LambdaBody {
	UserDefined(Box<Expr>),
	Intrinsic(Intrinsic),
}

/// A function expression.
#[derive(Clone, Eq, PartialEq)]
pub struct Lambda {
	pub params: Vec<Symbol>,
	pub body: LambdaBody,
}

impl Lambda {
	pub fn to_string(&self) -> String {
		match self.body {
			LambdaBody::UserDefined(body) => format!("(({}) -> {})", self.params.iter().map(|s| s.name.clone()).join(", "), body.to_string()),
			LambdaBody::Intrinsic(f) => f.name(),
		}
	}
}

/// Sum type of all Gynjo expressions.
#[derive(Clone, Eq, PartialEq)]
pub enum Expr {
	Cond {
		test: Box<Expr>,
		then_expr: Box<Expr>,
		else_expr: Box<Expr>,
	},
	Block { stmts: Box<Vec<Stmt>> },
	BinaryExpr(BinaryExpr),
	Not { expr: Box<Expr> },
	Cluster(Cluster),
	Lambda(Lambda),
	TupleExpr(Box<Vec<Expr>>),
	ListExpr(Box<Vec<Expr>>),
	Symbol(Symbol),
	Literal(Literal),
}

impl Expr {
	/// Converts this expression to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Expr::Cond { test, then_expr, else_expr } =>
				format!("({} ? {} : {})", test.to_string(), then_expr.to_string(), else_expr.to_string()),
			Expr::Block { stmts } => format!("{{ {} }}", stmts.iter().map(Stmt::to_string).join("; ")),
			Expr::BinaryExpr(binary_expr) => binary_expr.to_string(),
			Expr::Not { expr } => format!("(not {})", expr.to_string()),
			Expr::Cluster(cluster) => format!("({})", cluster.items.iter().map(ClusterItem::to_string).join(" ")),
			Expr::Lambda(f) => f.to_string(),
			Expr::TupleExpr(exprs) => format!("({})", exprs.iter().map(Expr::to_string).join(", ")),
			Expr::ListExpr(exprs) => format!("[{}]", exprs.iter().map(Expr::to_string).join(", ")),
			Expr::Symbol(symbol) => symbol.to_string(),
			Expr::Literal(literal) => literal.to_string(),
		}
	}
}