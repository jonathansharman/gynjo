use super::BinExpr;
use super::Cluster;
use super::Lambda;

use crate::primitives::Prim;
use crate::symbol::Sym;

use itertools::Itertools;

use std::boxed::Box;
use std::collections::VecDeque;
use std::fmt;

/// Gynjo expressions.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Expr {
	Block(Box<Vec<Expr>>),
	BinExpr(BinExpr),
	Not(Box<Expr>),
	Cluster(Cluster),
	Lambda(Lambda),
	TupleExpr(Box<Vec<Expr>>),
	ListExpr(Box<VecDeque<Expr>>),
	RangeExpr(Box<(Option<Expr>, Option<Expr>, Option<Expr>)>),
	Sym(Sym),
	Unit(String),
	Prim(Prim),
	Import(Box<Expr>),
	Assign {
		lhs: Sym,
		rhs: Box<Expr>,
	},
	DeclUnit {
		unit_name: String,
		value_expr: Box<Expr>,
	},
	Basic(Box<Expr>),
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
	Break,
	Return(Box<Expr>),
	Read,
	Write(Box<Expr>),
	GetType(Box<Expr>),
}

impl fmt::Display for Expr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Expr::Block(exprs) => {
				write!(f, "{{ {} }}", exprs.iter().map(Expr::to_string).join("; "))
			}
			Expr::BinExpr(binary_expr) => binary_expr.fmt(f),
			Expr::Not(expr) => write!(f, "(not {})", expr),
			Expr::Cluster(cluster) => cluster.fmt(f),
			Expr::Lambda(lambda) => lambda.fmt(f),
			Expr::TupleExpr(exprs) => {
				write!(f, "({})", exprs.iter().map(Expr::to_string).join(", "))
			}
			Expr::ListExpr(exprs) => {
				write!(f, "[{}]", exprs.iter().map(Expr::to_string).join(", "))
			}
			Expr::RangeExpr(exprs) => {
				let start = (*exprs)
					.0
					.as_ref()
					.map_or(String::new(), |start| format!("{}", start));
				let end = (*exprs)
					.0
					.as_ref()
					.map_or(String::new(), |end| format!("{}", end));
				let stride = (*exprs)
					.0
					.as_ref()
					.map_or(String::new(), |stride| format!("{}", stride));
				write!(f, "({}..{} by {})", start, end, stride)
			}
			Expr::Sym(symbol) => symbol.fmt(f),
			Expr::Unit(unit) => unit.fmt(f),
			Expr::Prim(primitive) => primitive.fmt(f),
			Expr::Import(target) => write!(f, "import \"{}\"", target),
			Expr::Assign { lhs, rhs } => write!(f, "let {} = {}", lhs.name, rhs),
			Expr::DeclUnit {
				unit_name,
				value_expr,
			} => write!(f, "let {} = {}", unit_name, value_expr),
			Expr::Basic(expr) => write!(f, "basic {}", expr),
			Expr::Branch {
				test,
				then_expr,
				else_expr,
			} => {
				write!(f, "if {} then {} else {}", test, then_expr, else_expr)
			}
			Expr::WhileLoop { test, body } => write!(f, "while {} do {}", test, body),
			Expr::ForLoop {
				loop_var,
				range,
				body,
			} => {
				write!(f, "for {} in {} do {}", loop_var.name, range, body)
			}
			Expr::Break => write!(f, "break"),
			Expr::Return(result) => write!(f, "return {}", result),
			Expr::Read => write!(f, "read"),
			Expr::Write(output) => write!(f, "write {}", output),
			Expr::GetType(expr) => write!(f, "get_type {}", expr),
		}
	}
}
