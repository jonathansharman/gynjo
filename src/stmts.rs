use super::exprs::Expr;
use super::symbol::Sym;

use std::boxed::Box;

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

impl Stmt {
	/// Converts this statement to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Stmt::Nop => "no-op".to_string(),
			Stmt::Import { filename } => format!("import {}", filename),
			Stmt::Assign { lhs, rhs } => format!("let {} = {}", lhs.name, rhs.to_string()),
			Stmt::Branch { test, then_stmt, else_stmt } =>
				format!("if {} then {} else {}", test.to_string(), then_stmt.to_string(), else_stmt.to_string()),
			Stmt::WhileLoop { test, body } => format!("while {} do {}", test.to_string(), body.to_string()),
			Stmt::ForLoop { loop_var, range, body } => 
				format!("for {} in {} do {}", loop_var.name, range.to_string(), body.to_string()),
			Stmt::Return { result } => format!("return {}", result.to_string()),
			Stmt::ExprStmt(expr) => format!("{};", expr.to_string()),
		}
	}
}
