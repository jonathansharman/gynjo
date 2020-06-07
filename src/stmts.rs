use super::exprs::Expr;
use super::symbol::Sym;

use std::boxed::Box;
use std::fmt;

/// Gynjo statements.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Stmt {
	Nop,
	Import { filename: String },
	Assign {
		lhs: Sym,
		rhs: Box<Expr>,
	},
	Branch {
		test: Box<Expr>,
		then_stmt: Box<Stmt>,
		else_stmt: Box<Stmt>,
	},
	WhileLoop {
		test: Box<Expr>,
		body: Box<Stmt>,
	},
	ForLoop {
		loop_var: Sym,
		range: Box<Expr>,
		body: Box<Stmt>,
	},
	Return { result: Box<Expr> },
	ExprStmt(Box<Expr>),
}

impl fmt::Display for Stmt {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Stmt::Nop => write!(f, "no-op"),
			Stmt::Import { filename } => write!(f, "import {}", filename),
			Stmt::Assign { lhs, rhs } => write!(f, "let {} = {}", lhs.name, rhs),
			Stmt::Branch { test, then_stmt, else_stmt } => {
				write!(f, "if {} then {} else {}", test, then_stmt, else_stmt)
			},
			Stmt::WhileLoop { test, body } => write!(f, "while {} do {}", test, body),
			Stmt::ForLoop { loop_var, range, body } => {
				write!(f, "for {} in {} do {}", loop_var.name, range, body)
			},
			Stmt::Return { result } => write!(f, "return {}", result),
			Stmt::ExprStmt(expr) => write!(f, "{};", expr),
		}
    }
}
