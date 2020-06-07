use super::env::Env;
use super::error::{Error, RuntimeError};
use super::exprs::{Expr, BinOp, Cluster, ClusterConnector, LambdaBody};
use super::intrinsics::Intrinsic;
use super::lexer::lex;
use super::number::Num;
use super::primitives::{Prim, Bool};
use super::parser::{parse_expr_with_all, parse_stmt};
use super::stmts::Stmt;
use super::types::Type;
use super::values::{Closure, Tuple, List, Val};

use bigdecimal::BigDecimal;

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Val, RuntimeError>;

/// Result of executing a Gynjo statement.
type ExecResult = Result<(), RuntimeError>;

/// If possible, computes the value of `expr` in the context of `env`.
pub fn eval_expr(mut env: &mut Rc<RefCell<Env>>, expr: Expr) -> EvalResult {
	match expr {
		Expr::Cond { test, then_expr, else_expr } => {
			eval_expr(&mut env, *test).and_then(|test_value| {
				match test_value {
					Val::Prim(Prim::Bool(boolean)) => eval_expr(&mut env, if boolean.into() { *then_expr } else { *else_expr }),
					_ => Err(RuntimeError::UnaryTypeMismatch {
						context: "conditional test".into(),
						expected: Type::Boolean,
						actual: test_value.get_type()
					}),
				}
			})
		},
		Expr::Block { stmts } => {
			for stmt in stmts.into_iter() {
				// A return statement exits the block early and produces a value.
				if let Stmt::Return { result } = stmt {
					return eval_expr(&mut env, *result);
				}
				// Otherwise, just execute the statement.
				exec_stmt(&mut env, stmt)?;
			}
			// Return nothing if there was no return statement.
			Ok(Val::Tuple(Tuple::empty()))
		},
		Expr::BinaryExpr(bin_expr) => match bin_expr.op {
			BinOp::And => {
				match eval_expr(&mut env, *bin_expr.left)? {
					Val::Prim(Prim::Bool(left)) => if left.into() {
						match eval_expr(&mut env, *bin_expr.right)? {
							Val::Prim(Prim::Bool(right)) => Ok(right.into()),
							invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
								context: "logical conjunction",
								expected: Type::Boolean,
								actual: invalid.get_type(),
							}),
						}
					} else {
						// Short-circuit to false.
						Ok(Bool::False.into())
					},
					invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "logical conjunction",
						expected: Type::Boolean,
						actual: invalid.get_type(),
					}),
				}
			},
			BinOp::Or => {
				match eval_expr(env, *bin_expr.left)? {
					Val::Prim(Prim::Bool(left)) => if left.into() {
						// Short-circuit to true.
						Ok(Bool::True.into())
					} else {
						match eval_expr(env, *bin_expr.right)? {
							Val::Prim(Prim::Bool(right)) => Ok(right.into()),
							invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
								context: "logical disjunction",
								expected: Type::Boolean,
								actual: invalid.get_type(),
							}),
						}
					},
					invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "logical disjunction",
						expected: Type::Boolean,
						actual: invalid.get_type(),
					}),
				}
			},
			BinOp::Eq => Ok(Bool::from(eval_expr(env, *bin_expr.left)? == eval_expr(env, *bin_expr.right)?).into()),
			BinOp::Neq => Ok(Bool::from(eval_expr(env, *bin_expr.left)? != eval_expr(env, *bin_expr.right)?).into()),
			BinOp::Approx => {
				let left = eval_expr(env, *bin_expr.left)?;
				let right = eval_expr(env, *bin_expr.right)?;
				Ok(Val::from(match (left, right) {
					(Val::Prim(Prim::Num(left)), Val::Prim(Prim::Num(right))) => {
						let left = Val::from(Num::Real(BigDecimal::from(left))).to_string(&mut env);
						let right = Val::from(Num::Real(BigDecimal::from(right))).to_string(&mut env);
						left == right
					},
					(left @ _, right @ _) => left == right,
				}))
			},
			BinOp::Lt => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Val::from(a < b)))
			},
			BinOp::Leq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Val::from(a <= b)))
			},
			BinOp::Gt => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Val::from(a > b)))
			},
			BinOp::Geq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Val::from(a >= b)))
			},
			BinOp::Add => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "addition", |a, b| Ok(Val::from(a + b)))
			},
			BinOp::Sub => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "subtraction", |a, b| Ok(Val::from(a - b)))
			},
		},
		Expr::Not { expr } => {
			match eval_expr(env, *expr)? {
				Val::Prim(Prim::Bool(b)) => Ok(Bool::from(!bool::from(b)).into()),
				invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
					context: "logical negation",
					expected: Type::Boolean,
					actual: invalid.get_type(),
				}),
			}
		},
		Expr::Cluster(cluster) => eval_cluster(env, cluster),
		Expr::Lambda(f) => Ok(Val::Closure(Closure { f: f, env: env.clone() })),
		Expr::TupleExpr(expr_elems) => {
			let mut elems = Box::new(Vec::with_capacity(expr_elems.len()));
			for value in expr_elems.into_iter().map(|elem| eval_expr(&mut env, elem)) {
				elems.push(value?);
			}
			Ok(Val::Tuple(Tuple { elems: elems }))
		},
		Expr::ListExpr(expr_elems) => {
			let mut list = List::Empty;
			for expr_elem in expr_elems.into_iter() {
				list = List::Cons { head: Box::new(eval_expr(&mut env, expr_elem)?), tail: Rc::new(list) };
			}
			Ok(Val::List(list))
		},
		Expr::Sym(symbol) => env.borrow().lookup(&symbol)
			.map(|v| v.clone())
			.ok_or(RuntimeError::Undefined(symbol.name)),
		Expr::Prim(primitive) => Ok(Val::Prim(match primitive {
			Prim::Num(number) => Prim::Num(number.shrink_domain()),
			primitive @ _ => primitive
		})),
	}
}

/// If possible, computes the value of the expression contained in `input` in the context of `env`.
pub fn eval(env: &mut Rc<RefCell<Env>>, input: &str) -> Result<Val, Error> {
	// Lex.
	let tokens = lex(input).map_err(Error::lex)?;
	// Parse.
	let expr = parse_expr_with_all(&tokens[..]).map_err(Error::parse)?;
	// Evaluate.
	eval_expr(env, expr).map_err(Error::runtime)
}

/// If possible, executes `stmt` in the context of `env`.
fn exec_stmt(mut env: &mut Rc<RefCell<Env>>, stmt: Stmt) -> ExecResult {
	match stmt {
		Stmt::Nop => Ok(()),
		Stmt::Import { filename } => {
			let lib_text = std::fs::read_to_string(&filename)
				.map_err(|err| RuntimeError::FileError {
					filename: filename.clone(),
					file_error: err.to_string(),
				})?;
			exec(&mut env, &lib_text).map_err(|err| RuntimeError::LibError {
				lib_name: filename.clone(),
				nested_error: Box::new(err),
			})
		},
		Stmt::Assign { lhs, rhs } => {
			let rhs_value = eval_expr(env, *rhs)?;
			// Perform the assignment, possibly overwriting the existing value.
			env.borrow_mut().assign(lhs, rhs_value);
			Ok(())
		},
		Stmt::Branch { test, then_stmt, else_stmt } => {
			let test_value = eval_expr(env, *test)?;
			match test_value {
				Val::Prim(Prim::Bool(b)) => exec_stmt(env, if b.into() { *then_stmt } else { *else_stmt }),
				_ => Err(RuntimeError::UnaryTypeMismatch {
					context: "conditional test",
					expected: Type::Boolean,
					actual: test_value.get_type(),
				}),
			}
		},
		Stmt::WhileLoop { test, body } => {
			loop {
				// Evaluate the test condition.
				let test_value = eval_expr(env, (*test).clone())?;
				match test_value {
					Val::Prim(Prim::Bool(b)) => if b.into() {
						// Execute next iteration.
						exec_stmt(env, (*body).clone())?;
					} else {
						// End of loop.
						return Ok(());
					},
					_ => print!("while-loop test value must be boolean, found {}", test_value.to_string(&env)),
				}
			}
		},
		Stmt::ForLoop { loop_var, range, body } => {
			let range_value = eval_expr(&mut env, *range)?;
			match range_value {
				Val::List(range_list) => {
					for value in range_list.iter() {
						// Assign the loop variable to the current value in the range list.
						env.borrow_mut().assign(loop_var.clone(), value.clone());
						// Execute the loop body in this context.
						exec_stmt(env, (*body).clone())?;
					}
					Ok(())
				},
				_ => Err(RuntimeError::UnaryTypeMismatch {
					context: "for-loop range".into(),
					expected: Type::List,
					actual: range_value.get_type()
				}),
			}
		},
		Stmt::Return { .. } => Err(RuntimeError::ReturnOutsideBlock),
		Stmt::ExprStmt(expr) => {
			let value = eval_expr(env, *expr)?;
			// Expression statements must evaluate to ().
			match value {
				Val::Tuple(Tuple { elems }) if (elems.is_empty()) => Ok(()),
				_ => Err(RuntimeError::UnusedResult(value.to_string(env))),
			}
		}
	}
}

/// If possible, executes the statements contained in `input` in the context of `env`.
pub fn exec(env: &mut Rc<RefCell<Env>>, input: &str) -> Result<(), Error> {
	// Lex.
	let tokens = lex(input).map_err(Error::lex)?;
	let mut token_slice = &tokens[..];
	// While there is still input left, parse and execute.
	while !token_slice.is_empty() {
		// Parse.
		let (remaining_tokens, stmt) = parse_stmt(token_slice).map_err(Error::parse)?;
		token_slice = remaining_tokens;
		// Execute.
		exec_stmt(env, stmt).map_err(Error::runtime)?;
	}
	Ok(())
}

/// Evaluates a numerical operation on a list and a number.
fn list_num_op(env: &Rc<RefCell<Env>>, list: List, number: Num, number_on_left: bool, op_name: &'static str, op: fn(Num, Num) -> EvalResult) -> Result<List, RuntimeError> {
	match list {
		List::Empty => Ok(List::Empty),
		List::Cons { head, tail } => Ok(List::Cons {
			head: if number_on_left {
				Box::new(bin_num_op(env, Val::Prim(Prim::Num(number.clone())), *head, op_name, op)?)
			} else {
				Box::new(bin_num_op(env, *head, Val::Prim(Prim::Num(number.clone())), op_name, op)?)
			},
			tail: Rc::new(list_num_op(env, (*tail).clone(), number, number_on_left, op_name, op)?),
		}),
	}
}

/// Evaluates a numerical operation on two values.
fn bin_num_op(env: &Rc<RefCell<Env>>, left: Val, right: Val, op_name: &'static str, op: fn(Num, Num) -> EvalResult) -> EvalResult {
	match (left, right) {
		// Number op Number
		(Val::Prim(Prim::Num(left)), Val::Prim(Prim::Num(right))) => Ok(op(left, right)?),
		// List op Number
		(Val::List(list), Val::Prim(Prim::Num(number))) => {
			Ok(Val::List(list_num_op(env, list, number, false, op_name, op)?))
		},
		// Number op List
		(Val::Prim(Prim::Num(number)), Val::List(list)) => {
			Ok(Val::List(list_num_op(env, list, number, true, op_name, op)?))
		},
		// Invalid numeric operation
		(left @ _, right @ _) => Err(RuntimeError::BinaryTypeMismatch {
			context: op_name,
			left: left.get_type(),
			right: right.get_type(),
		}),
	}
}

/// A cluster item with its expression resolved to a value.
struct EvaluatedClusterItem {
	/// This item's value.
	value: Val,
	/// How this item is connected to the previous item.
	connector: ClusterConnector,
}

fn eval_evaluated_cluster(env: &mut Rc<RefCell<Env>>, mut cluster: Vec<EvaluatedClusterItem>) -> EvalResult {
	if cluster.len() > 1 {
		// Parenthesized applications
		for idx in 0..cluster.len() - 1 {
			if let Val::Closure(closure) = &cluster[idx].value {
				if cluster[idx + 1].connector == ClusterConnector::AdjParen {
					cluster[idx].value = eval_application(closure.clone(), cluster[idx + 1].value.clone())?;
					cluster.remove(idx + 1);
					return eval_evaluated_cluster(env, cluster);
				}
			}
		}
		// Exponentiations
		for idx in (0..cluster.len() - 1).rev() {
			if cluster[idx + 1].connector == ClusterConnector::Exp {
				let left = cluster[idx].value.clone();
				let right = cluster[idx + 1].value.clone();
				cluster[idx].value = bin_num_op(env, left, right, "exponentiation", |a, b| {
					Ok(Val::from(a.pow(b).map_err(RuntimeError::numeric)?))
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			}
		}
		// Non-parenthesized applications
		for idx in 0..cluster.len() - 1 {
			if let Val::Closure(closure) = &cluster[idx].value {
				if cluster[idx + 1].connector == ClusterConnector::AdjNonparen {
					cluster[idx].value = eval_application(closure.clone(), cluster[idx + 1].value.clone())?;
					cluster.remove(idx + 1);
					return eval_evaluated_cluster(env, cluster);
				}
			}
		}
		// Multiplications and Divisions
		for idx in 0..cluster.len() - 1 {
			match &cluster[idx + 1].connector {
				ClusterConnector::AdjParen | ClusterConnector::AdjNonparen | ClusterConnector::Mul => {
					let left = cluster[idx].value.clone();
					let right = cluster[idx + 1].value.clone();
					cluster[idx].value = bin_num_op(env, left, right, "multiplication", |a, b| {
						Ok(Val::from(a * b))
					})?;
					cluster.remove(idx + 1);
					return eval_evaluated_cluster(env, cluster);
				},
				ClusterConnector::Div => {
					let left = cluster[idx].value.clone();
					let right = cluster[idx + 1].value.clone();
					cluster[idx].value = bin_num_op(env, left, right, "division", |a, b| {
						Ok(Val::from((a / b).map_err(RuntimeError::numeric)?))
					})?;
					cluster.remove(idx + 1);
					return eval_evaluated_cluster(env, cluster);
				},
				_ => ()
			}
		}
		unreachable!("previous cases are exhaustive")
	} else {
		Ok(cluster.remove(0).value)
	}
}

/// Evaluates a cluster expression.
fn eval_cluster(env: &mut Rc<RefCell<Env>>, cluster: Cluster) -> EvalResult {
	// First, evaluate the cluster items.
	let mut evaluated_cluster = Vec::with_capacity(cluster.items.len());
	for item in cluster.items {
		let value = eval_expr(env, *item.expr)?;
		evaluated_cluster.push(EvaluatedClusterItem {
			value: if item.negated { eval_negation(env, value)? } else { value },
			connector: item.connector,
		});
	}
	// Now that the items' types are known, evaluate down to a single value and negate if necessary.
	let non_negated_result = eval_evaluated_cluster(env, evaluated_cluster)?;
	if cluster.negated {
		eval_negation(env, non_negated_result)
	} else {
		Ok(non_negated_result)
	}
}

/// Evaluates a closure application.
fn eval_application(c: Closure, args: Val) -> EvalResult {
	// Extract arguments into a vector.
	let args = match args {
		Val::Tuple(tuple) => {
			*tuple.elems
		},
		_ => vec!(args),
	};
	// Ensure correct number of arguments.
	if args.len() != c.f.params.len() {
		return Err(RuntimeError::ArgCountMismatch {
			required: c.f.params.len(),
			received: args.len(),
		});
	}
	// Assign arguments to parameters within a copy of the closure's environment.
	let mut local_env = Env::new(Some(c.env.clone()));
	for (variable, value) in c.f.params.into_iter().zip(args.into_iter()) {
		local_env.borrow_mut().assign(variable, value);
	}
	// Evaluate function body within the application environment.
	match c.f.body {
		LambdaBody::UserDefined(body) => eval_expr(&mut local_env, *body),
		LambdaBody::Intrinsic(body) => {
			match body {
				Intrinsic::Top => match local_env.borrow().lookup(&"list".into()).unwrap() {
					Val::List(List::Cons { head, .. }) => Ok((*head).clone()),
					arg @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "top()",
						expected: Type::Cons,
						actual: arg.get_type()
					}),
				},
				Intrinsic::Pop => match local_env.borrow().lookup(&"list".into()).unwrap() {
					Val::List(List::Cons { tail, .. }) => Ok(Val::List((*tail).clone())),
					arg @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "pop()",
						expected: Type::Cons,
						actual: arg.get_type()
					}),
				},
				Intrinsic::Push => match local_env.borrow().lookup(&"list".into()).unwrap() {
					Val::List(list) => {
						let value = local_env.borrow().lookup(&"value".into()).unwrap().clone();
						Ok(Val::List(List::Cons { head: Box::new(value), tail: Rc::new(list.clone()) }))
					},
					arg @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "push()",
						expected: Type::List,
						actual: arg.get_type()
					}),
				},
				Intrinsic::Print => {
					print!("{}", local_env.borrow().lookup(&"value".into()).unwrap().to_string(&local_env));
					Ok(Val::Tuple(Tuple::empty()))
				},
				Intrinsic::Read => {
					let mut input = String::new();
					io::stdin().read_line(&mut input).unwrap();
					Ok(Val::from(input.trim().to_string()))
				},
				Intrinsic::ToReal => match local_env.borrow().lookup(&"value".into()).unwrap() {
					Val::Prim(Prim::Num(number)) => Ok(Val::from(Num::Real(number.into()))),
					arg @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "real()",
						expected: Type::Real,
						actual: arg.get_type()
					}),
				},
			}
		}
	}
}

fn negate_list(env: &Rc<RefCell<Env>>, list: List) -> Result<List, RuntimeError> {
	match list {
		List::Empty => Ok(List::Empty),
		List::Cons { head, tail } => Ok(List::Cons {
			head: Box::new(eval_negation(env, *head)?),
			tail: Rc::new(negate_list(env, (*tail).clone())?),
		}),
	}
}

fn eval_negation(env: &Rc<RefCell<Env>>, value: Val) -> EvalResult {
	match value {
		Val::Prim(Prim::Num(Num::Integer(integer))) => Ok(Val::Prim(Prim::Num(Num::Integer(-integer)))),
		Val::Prim(Prim::Num(Num::Real(real))) => Ok(Val::Prim(Prim::Num(Num::Real(-real)))),
		Val::List(list) => Ok(Val::List(negate_list(env, list)?)),
		_ => Err(RuntimeError::UnaryTypeMismatch {
			context: "negation",
			expected: Type::Boolean,
			actual: value.get_type(),
		}),
	}
}

#[cfg(test)]
mod tests {
	use crate::env::Env;
	use crate::error::Error;
	use crate::interpreter::{eval, exec};
	use crate::number::Num;
	use crate::primitives::Prim;
	use crate::values::{Val, Tuple, List};

	use bigdecimal::BigDecimal;

	use std::cell::RefCell;
	use std::rc::Rc;
	use std::str::FromStr;

	#[test]
	fn execution_of_empty_statement_returns_nothing() -> Result<(), Error> {
		assert_eq!((), exec(&mut Env::new(None), "")?);
		Ok(())
	}

	mod logical_operators {
		use super::*;
		#[test]
		fn and() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false and false")?);
			assert_eq!(Val::from(false), eval(&mut env, "false and true")?);
			assert_eq!(Val::from(false), eval(&mut env, "true and false")?);
			assert_eq!(Val::from(true), eval(&mut env, "true and true")?);
			Ok(())
		}
		#[test]
		fn or() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false or false")?);
			assert_eq!(Val::from(true), eval(&mut env, "false or true")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or false")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or true")?);
			Ok(())
		}
		#[test]
		fn not() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "not true")?);
			assert_eq!(Val::from(true), eval(&mut env, "not false")?);
			assert_eq!(Val::from(true), eval(&mut env, "not false and false")?);
			Ok(())
		}
		#[test]
		fn short_circuiting() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false and 1/0")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or 1/0")?);
			Ok(())
		}
		#[test]
		fn and_precedes_or() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "true or true and false")?);
			assert_eq!(Val::from(true), eval(&mut env, "false and true or true")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "(false) and true")?);
			assert_eq!(Val::from(true), eval(&mut env, "false or ((true))")?);
			assert_eq!(Val::from(true), eval(&mut env, "not (false)")?);
			Ok(())
		}
	}

	mod comparisons {
		use super::*;
		#[test]
		fn eq() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 2")?);
			Ok(())
		}
		#[test]
		fn neq() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 != 2")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 != 1")?);
			Ok(())
		}
		#[test]
		fn approx() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "true ~ true")?);
			assert_eq!(Val::from(true), eval(&mut env, "real(1/3) ~ 0.333333333333")?);
			assert_eq!(Val::from(true), eval(&mut env, "1/3 ~ 0.333333333333")?);
			assert_eq!(Val::from(false), eval(&mut env, "real(1/3) ~ 0.333")?);
			Ok(())
		}
		#[test]
		fn lt() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 < 2.0")?);
			assert_eq!(Val::from(false), eval(&mut env, "1.0 < 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "2 < 1")?);
			Ok(())
		}
		#[test]
		fn leq() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 <= 2.0")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.0 <= 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "2 <= 1")?);
			Ok(())
		}
		#[test]
		fn gt() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "1 > 2.0")?);
			assert_eq!(Val::from(false), eval(&mut env, "1.0 > 1")?);
			assert_eq!(Val::from(true), eval(&mut env, "2 > 1")?);
			Ok(())
		}
		#[test]
		fn geq() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "1 >= 2.0")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.0 >= 1")?);
			assert_eq!(Val::from(true), eval(&mut env, "2 >= 1")?);
			Ok(())
		}
		#[test]
		fn comparisons_and_logical_operators() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 1 and 2 = 2")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 1 and 2 = 3")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 2 or 3 = 3")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 2 or 3 = 4")?);
			Ok(())
		}
		#[test]
		fn non_numbers_are_equal_checkable_but_not_comparable() -> Result<(), Error> {
			let mut env = Env::new(None);
			// Booleans and tuples can be equality-checked.
			assert_eq!(Val::from(true), eval(&mut env, "true = true")?);
			assert_eq!(Val::from(false), eval(&mut env, "true = false")?);
			assert_eq!(Val::from(false), eval(&mut env, "true != true")?);
			assert_eq!(Val::from(true), eval(&mut env, "true != false")?);
			assert_eq!(Val::from(true), eval(&mut env, "(1, 2, 3) = (1, 2, 3)")?);
			assert_eq!(Val::from(false), eval(&mut env, "(1, 2, 3) = (3, 2, 1)")?);
			assert_eq!(Val::from(false), eval(&mut env, "(1, 2, 3) != (1, 2, 3)")?);
			assert_eq!(Val::from(true), eval(&mut env, "(1, 2, 3) != (3, 2, 1)")?);
			// Cannot be compared.
			assert!(eval(&mut env, "false < true").is_err());
			assert!(eval(&mut env, "(1, 2, 3) < (3, 2, 1)").is_err());
			Ok(())
		}
		#[test]
		fn different_types_compare_inequal() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "[1, 2, 3] = (1, 2, 3)")?);
			assert_eq!(Val::from(true), eval(&mut env, "(x -> x) != false")?);
			Ok(())
		}
		#[test]
		fn different_types_cannot_be_order_compared() -> Result<(), Error> {
			assert!(eval(&mut Env::new(None), "(x -> x) < false").is_err());
			Ok(())
		}
		#[test]
		fn comparison_precedes_equality() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 < 2 = 2 < 3")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 > 2 != 2 < 3")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "(1) = 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 != (1)")?);
			assert_eq!(Val::from(true), eval(&mut env, "((1)) < 2")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 <= ((1))")?);
			assert_eq!(Val::from(false), eval(&mut env, "(1) > (1)")?);
			assert_eq!(Val::from(false), eval(&mut env, "((1)) >= (2)")?);
			Ok(())
		}
	}

	mod conditional_expressions {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "false ? 1/0 : 1")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "true ? 1 : 1/0")?);
			Ok(())
		}
	}

	#[test]
	fn subtraction_and_negation() -> Result<(), Error> {
		assert_eq!(Val::from(3), eval(&mut Env::new(None), "-1+-2*-2")?);
		Ok(())
	}

	#[test]
	fn simple_compound_expression_with_parentheses() -> Result<(), Error> {
		assert_eq!(Val::from(5), eval(&mut Env::new(None), "-5 *(1 +  -2)")?);
		Ok(())
	}

	mod mixed_domain_math {
		use super::*;
		#[test]
		fn real_literals_decay_to_integer() -> Result<(), Error> {
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "3.0")?);
			assert_eq!(Val::from(4_000_000), eval(&mut Env::new(None), "4000000.0")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), Error> {
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "1 + 2")?);
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "1.0 + 2")?);
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "1 + 2.0")?);
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "1.0 + 2.0")?);
			assert_eq!(Val::from(3.5), eval(&mut Env::new(None), "1 + 2.5")?);
			assert_eq!(Val::from(3.5), eval(&mut Env::new(None), "1.5 + 2")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "2 - 1")?);
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "2.0 - 1")?);
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "2 - 1.0")?);
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "2.0 - 1.0")?);
			assert_eq!(Val::from(1.5), eval(&mut Env::new(None), "2.5 - 1")?);
			assert_eq!(Val::from(0.5), eval(&mut Env::new(None), "2 - 1.5")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), Error> {
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2 * 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2.0 * 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2 * 1.0")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2.0 * 1.0")?);
			assert_eq!(Val::from(2.5), eval(&mut Env::new(None), "2.5 * 1")?);
			assert_eq!(Val::from(3), eval(&mut Env::new(None), "2 * 1.5")?);
			assert_eq!(Val::from(3.75), eval(&mut Env::new(None), "2.5 * 1.5")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), Error> {
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2 / 1")?);
			assert_eq!(Val::from(Num::rational(1, 2)), eval(&mut Env::new(None), "1 / 2")?);
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "1 / 2 * 2")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2.0 / 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "3 / 1.5")?);
			assert_eq!(Val::from(2.5), eval(&mut Env::new(None), "2.5 / 1.0")?);
			assert_eq!(Val::from(2.5), eval(&mut Env::new(None), "2.5 / 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "3 / 1.5")?);
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "1.5 / 1.5")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), Error> {
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2 ^ 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2.0 ^ 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2 ^ 1.0")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "2.0 ^ 1.0")?);
			assert_eq!(Val::from(2.5), eval(&mut Env::new(None), "2.5 ^ 1")?);
			assert_eq!(Val::from(2), eval(&mut Env::new(None), "4 ^ 0.5")?);
			Ok(())
		}
	}

	#[test]
	fn exponentiation_is_right_associative() -> Result<(), Error> {
		assert_eq!(Val::from(262144), eval(&mut Env::new(None), "4^3^2")?);
		Ok(())
	}

	#[test]
	fn basic_assignment() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, "let x = 42")?;
		assert_eq!(Val::from(42), eval(&mut env, "x")?);
		Ok(())
	}

	mod tuples {
		use super::*;
		#[test]
		fn singleton_collapses_into_contained_value() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "(1)")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers() -> Result<(), Error> {
			let expected = make_tuple!(Val::from(1), make_tuple!(Val::from(2), Val::from(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1, (2, 3))")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers_and_booleans() -> Result<(), Error> {
			let expected = make_tuple!(Val::from(true), make_tuple!(Val::from(2), Val::from(false)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1 < 2, (2, false))")?);
			Ok(())
		}
	}

	mod list_construction {
		use super::*;
		#[test]
		fn singleton_list() -> Result<(), Error> {
			assert_eq!(Val::List(make_list!(Val::from(1))), eval(&mut Env::new(None), "[1]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers() -> Result<(), Error> {
			let expected = Val::List(make_list!(Val::from(1), Val::List(make_list!(Val::from(2), Val::from(3)))));
			assert_eq!(expected, eval(&mut Env::new(None), "[1, [2, 3]]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers_and_booleans() -> Result<(), Error> {
			let expected = Val::List(make_list!(Val::from(true), Val::List(make_list!(Val::from(2), Val::from(false)))));
			assert_eq!(expected, eval(&mut Env::new(None), "[1 < 2, [2, false]]")?);
			Ok(())
		}
	}

	#[test]
	fn list_destruction_does_not_cause_stack_overflow() {
		let result = exec(&mut Env::new(None), r"(
			let i = 0
			let l = []
			while i < 1000 do {
				let l = push(l, i)
				let i = i + 1
			};
			)");
		assert!(result.is_ok());
	}

	mod math_operations_on_lists {
		use super::*;
		#[test]
		fn negation() -> Result<(), Error> {
			let expected = Val::List(make_list!(Val::from(-1), Val::from(-2), Val::from(-3)));
			assert_eq!(expected, eval(&mut Env::new(None), "-[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = Val::List(make_list!(Val::from(2), Val::from(3), Val::from(4)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3] + 1")?);
			assert_eq!(expected, eval(&mut env, "1 + [1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = Val::List(make_list!(Val::from(1), Val::from(2), Val::from(3)));
			assert_eq!(expected, eval(&mut env, "[2, 3, 4]-1")?);
			assert_eq!(expected, eval(&mut env, "4-[3, 2, 1]")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = Val::List(make_list!(Val::from(2), Val::from(4), Val::from(6)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3]2")?);
			assert_eq!(expected, eval(&mut env, "2[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = Val::List(make_list!(Val::from(1), Val::from(2), Val::from(3)));
			assert_eq!(expected, eval(&mut env, "[2, 4, 6]/2")?);
			assert_eq!(expected, eval(&mut env, "6/[6, 3, 2]")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = Val::List(make_list!(Val::from(1), Val::from(4), Val::from(16)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 4]^2")?);
			assert_eq!(expected, eval(&mut env, "2^[0, 2, 4]")?);
			Ok(())
		}
	}

	#[test]
	fn simple_function_application() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, "let f = () -> 42")?;
		assert_eq!(Val::from(42), eval(&mut env, "f()")?);
		Ok(())
	}

	mod order_of_operations {
		use super::*;
		fn env_with_inc() -> Result<Rc<RefCell<Env>>, Error> {
			let mut env = Env::new(None);
			exec(&mut env, "let inc = a -> a + 1")?;
			Ok(env)
		}
		#[test]
		fn parenthesized_function_call_before_exponentiation() -> Result<(), Error> {
			assert_eq!(Val::from(36), eval(&mut env_with_inc()?, "4inc(2)^2")?);
			Ok(())
		}
		#[test]
		fn exponentiation_before_non_parenthesized_function_call() -> Result<(), Error> {
			assert_eq!(Val::from(20), eval(&mut env_with_inc()?, "4inc 2^2")?);
			Ok(())
		}
		#[test]
		fn exponentiation_before_negation() -> Result<(), Error> {
			assert_eq!(Val::from(-9), eval(&mut Env::new(None), "-3^2")?);
			Ok(())
		}
	}

	#[test]
	fn higher_order_functions() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, "let apply = (f, a) -> f(a)")?;
		assert_eq!(Val::from(42), eval(&mut env, "apply(a -> a, 42)")?);
		Ok(())
	}

	#[test]
	fn curried_function() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, "let sum = a -> b -> a + b")?;
		assert_eq!(Val::from(3), eval(&mut env, "sum 1 2")?);
		Ok(())
	}

	#[test]
	fn environment_does_not_persist_between_function_chains() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let sum = a -> b -> a + b
			let get_a = () -> a
		")?;
		// "a" should be undefined.
		assert!(eval(&mut env, "sum (1) (2) get_a ()").is_err());
		Ok(())
	}

	#[test]
	fn chained_application_with_and_without_parentheses() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let sum = a -> b -> a + b
			let inc = a -> a + 1
		")?;
		assert_eq!(Val::from(3), eval(&mut env, "sum (1) 2")?);
		Ok(())
	}

	#[test]
	fn chained_application_does_not_pollute_applications_higher_in_the_call_chain() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let sum = a -> b -> a + b
			let inc = b -> b + 1
		")?;
		assert_eq!(Val::from(8), eval(&mut env, "sum (inc 5) 2")?);
		Ok(())
	}

	#[test]
	fn importing_core_constants() -> Result<(), Error> {
		let mut env = Env::new(None);
		let pi_str = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
		let expected = Val::Prim(Prim::Num(Num::Real(BigDecimal::from_str(pi_str).unwrap())));
		exec(&mut env, "import \"core/constants.gynj\"")?;
		assert_eq!(expected, eval(&mut env, "PI")?);
		Ok(())
	}

	#[test]
	fn blocks() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, "{}")?;
		eval(&mut env, "{ let a = 0 }")?;
		eval(&mut env, "{ let b = { let a = a + 1 return a } }")?;
		assert_eq!(Val::from(1), eval(&mut env, "b")?);
		Ok(())
	}

	mod branch_statements {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), Error> {
			let mut env = Env::new(None);
			exec(&mut env, "if false then let a = 1/0 else let a = 1")?;
			assert_eq!(Val::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), Error> {
			let mut env = Env::new(None);
			exec(&mut env, "if true then let a = 1 else let a = 1/0")?;
			assert_eq!(Val::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn no_else_statement() -> Result<(), Error> {
			assert_eq!((), exec(&mut Env::new(None), "if false then let a = 1/0")?);
			Ok(())
		}
	}

	#[test]
	fn while_loops() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let a = 0
			while a < 3 do let a = a + 1
		")?;
		assert_eq!(Val::from(3), eval(&mut env, "a")?);
		Ok(())
	}

	#[test]
	fn for_loops() -> Result<(), Error> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let a = 0
			for x in [1, 2, 3] do let a = a + x
			for x in [] do let a = 10
		")?;
		assert_eq!(Val::from(6), eval(&mut env, "a")?);
		Ok(())
	}

	mod intrinsics {
		use super::*;
		#[test]
		fn top() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(1), eval(&mut env, "top([1])")?);
			assert!(eval(&mut env, "top([])").is_err());
			Ok(())
		}
		#[test]
		fn pop() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::List(make_list!()), eval(&mut env, "pop([1])")?);
			assert!(eval(&mut env, "pop([])").is_err());
			Ok(())
		}
		#[test]
		fn push() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::List(make_list!(Val::from(1))), eval(&mut env, "push([], 1)")?);
			assert_eq!(Val::List(make_list!(Val::from(2), Val::from(1))), eval(&mut env, "push([1], 2)")?);
			Ok(())
		}
		#[test]
		fn to_real() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(1.0), eval(&mut env, "real(1)")?);
			assert_eq!(Val::from(0.5), eval(&mut env, "real(1/2)")?);
			Ok(())
		}
	}
}
