use super::env::{Env, SharedEnv};
use super::errors::{Error, RuntimeError};
use super::exprs::{Expr, BinExpr, BinOp, Cluster, ClusterConnector, LambdaBody};
use super::intrinsics::Intrinsic;
use super::lexer::lex;
use super::list::List;
use super::numbers::Num;
use super::primitives::{Prim, Bool};
use super::parser::parse;
use super::symbol::Sym;
use super::tuple::Tuple;
use super::types::Type;
use super::values::{Closure, Val};

use bigdecimal::BigDecimal;

use std::collections::VecDeque;
use std::io;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Val, RuntimeError>;

/// If possible, computes the value of `expr` in the context of `env`.
pub fn eval_expr(mut env: &mut SharedEnv, expr: Expr) -> EvalResult {
	match expr {
		Expr::Block { exprs } => eval_block(&mut env, exprs),
		Expr::BinExpr(bin_expr) => eval_bin_expr(&mut env, bin_expr),
		Expr::Not { expr } => eval_not(&mut env, expr),
		Expr::Cluster(cluster) => eval_cluster(env, cluster),
		Expr::Lambda(f) => Ok(Val::Closure(Closure { f: f, env: env.clone() })),
		Expr::TupleExpr(expr_elems) => eval_tuple_expr(&mut env, expr_elems),
		Expr::ListExpr(expr_elems) => eval_list_expr(&mut env, expr_elems),
		Expr::Sym(symbol) => eval_symbol(&mut env, symbol),
		Expr::Prim(primitive) => Ok(Val::Prim(match primitive {
			Prim::Num(number) => Prim::Num(number.shrink_domain()),
			primitive @ _ => primitive
		})),
		Expr::Import { target } => eval_import(&mut env, target),
		Expr::Assign { lhs, rhs } => eval_assignment(&mut env, lhs, rhs),
		Expr::Branch { test, then_expr, else_expr } => eval_branch(&mut env, test, then_expr, else_expr),
		Expr::WhileLoop { test, body } => eval_while_loop(env, test, body),
		Expr::ForLoop { loop_var, range, body } => eval_for_loop(env, loop_var, range, body),
		Expr::Return { result } => Ok(Val::Returned { result: Box::new(eval_expr(env, *result)?) }),
	}
}

/// If possible, computes the value of the expression contained in `input` in the context of `env`.
pub fn eval(env: &mut SharedEnv, input: &str) -> Result<Val, Error> {
	// Lex.
	let tokens = lex(input).map_err(Error::lex)?;
	// Parse.
	let expr = parse(&tokens[..]).map_err(Error::parse)?;
	// Evaluate.
	eval_expr(env, expr).map_err(Error::runtime)
}

fn eval_block(env: &mut SharedEnv, mut exprs: Box<Vec<Expr>>) -> EvalResult {
	let last = exprs.pop();
	if let Some(last) = last {
		for expr in exprs.into_iter() {
			match eval_expr(env, expr)? {
				Val::Tuple(Tuple { elems }) if elems.is_empty() => {
					// Non-final empty results are okay.
				},
				Val::Returned { result } => {
					// Return non-final result.
					return Ok(*result)
				},
				unused @ _ => {
					// Non-final, non-returned, non-empty results are errors.
					return Err(RuntimeError::UnusedResult(unused.to_string(env)));
				},
			}
		}
		// No early return. Return final value.
		Ok(eval_expr(env, last)?)
	} else {
		Ok(Val::empty())
	}
}

fn eval_bin_expr(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match bin_expr.op {
		BinOp::As => {
			match eval_expr(env, *bin_expr.right)? {
				Val::Prim(Prim::Type(to)) => {
					match (eval_expr(env, *bin_expr.left)?, to) {
						// T -> T
						(value @ _, to @ _) if value.get_type() == to => Ok(value),
						// integer -> rational
					    (Val::Prim(Prim::Num(Num::Integer(integer))), Type::Rational) => {
							Ok(Val::from(Num::Rational(integer.into())))
						},
						// integer | rational | real -> real
					    (Val::Prim(Prim::Num(number)), Type::Real) => {
							Ok(Val::from(Num::Real(number.into())))
						},
						// tuple -> list
						(Val::Tuple(tuple), Type::List) => {
							let mut list = List::empty();
							for value in tuple.elems.iter().rev() {
								list = list.push(value.clone());
							}
							Ok(Val::List(list))
						},
						// list -> tuple
					    (Val::List(list), Type::Tuple) => {
							let mut elems = Box::new(Vec::new());
							for value in list.iter() {
								elems.push(value.clone());
							}
							Ok(Val::Tuple(Tuple { elems }))
						},
						// T -> string
						(value @ _, Type::String) => Ok(Val::Prim(Prim::String(value.to_string(env)))),
						// Invalid conversion
						(value @ _, to @ _) => Err(RuntimeError::InvalidTypeCast { from: value.get_type(), to }),
					}
				},
				invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
					context: "type cast",
					expected: Type::Type,
					actual: invalid.get_type(),
				})
			}
		},
		BinOp::And => {
			match eval_expr(env, *bin_expr.left)? {
				Val::Prim(Prim::Bool(left)) => if left.into() {
					match eval_expr(env, *bin_expr.right)? {
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
					let left = Val::from(Num::Real(BigDecimal::from(left))).to_string(env);
					let right = Val::from(Num::Real(BigDecimal::from(right))).to_string(env);
					left == right
				},
				(left @ _, right @ _) => left == right,
			}))
		},
		BinOp::Lt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "comparison", |a, b| Ok(Val::from(a < b)))
		},
		BinOp::Leq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "comparison", |a, b| Ok(Val::from(a <= b)))
		},
		BinOp::Gt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "comparison", |a, b| Ok(Val::from(a > b)))
		},
		BinOp::Geq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "comparison", |a, b| Ok(Val::from(a >= b)))
		},
		BinOp::Add => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "addition", |a, b| Ok(Val::from(a + b)))
		},
		BinOp::Sub => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			bin_num_op(env, left, right, "subtraction", |a, b| Ok(Val::from(a - b)))
		},
		BinOp::Concat => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			match (left, right) {
				// String concatenation
				(Val::Prim(Prim::String(left)), Val::Prim(Prim::String(right))) => {
					Ok(Val::from(format!("{}{}", left, right)))
				},
				(Val::Prim(Prim::String(left)), right @ _) => {
					Ok(Val::from(format!("{}{}", left, right.to_string(&env))))
				},
				(left @ _, Val::Prim(Prim::String(right))) => {
					Ok(Val::from(format!("{}{}", left.to_string(&env), right)))
				},
				// List concatenation
				(Val::List(left), Val::List(right)) => Ok(Val::List(left.concat(right))),
				// Invalid concatenation
				(left @ _, right @ _) => Err(RuntimeError::BinaryTypeMismatch {
					context: "concatenation",
					left: left.get_type(),
					right: right.get_type(),
				})
			}
		},
	}
}

fn eval_not(env: &mut SharedEnv, expr: Box<Expr>) -> EvalResult {
	match eval_expr(env, *expr)? {
		Val::Prim(Prim::Bool(b)) => Ok(Bool::from(!bool::from(b)).into()),
		invalid @ _ => Err(RuntimeError::UnaryTypeMismatch {
			context: "logical negation",
			expected: Type::Boolean,
			actual: invalid.get_type(),
		}),
	}
}

fn eval_tuple_expr(env: &mut SharedEnv, expr_elems: Box<Vec<Expr>>) -> EvalResult {
	let mut elems = Box::new(Vec::with_capacity(expr_elems.len()));
	for value in expr_elems.into_iter().map(|elem| eval_expr(env, elem)) {
		elems.push(value?);
	}
	Ok(Val::Tuple(Tuple { elems: elems }))
}

fn eval_list_expr(env: &mut SharedEnv, expr_elems: Box<VecDeque<Expr>>) -> EvalResult {
	let mut list = List::empty();
	for expr_elem in expr_elems.into_iter() {
		list = list.push(eval_expr(env, expr_elem)?);
	}
	Ok(Val::List(list))
}

fn eval_symbol(env: &mut SharedEnv, symbol: Sym) -> EvalResult {
	env.lock().unwrap().lookup(&symbol)
		.map(|v| v.clone())
		.ok_or(RuntimeError::Undefined(symbol.name))
}

fn eval_import(env: &mut SharedEnv, target: Box<Expr>) -> EvalResult {
	match eval_expr(env, *target)? {
    Val::Prim(Prim::String(filename)) => {
		let lib_text = std::fs::read_to_string(&filename)
			.map_err(|err| RuntimeError::CouldNotOpenFile {
				filename: filename.clone(),
				file_error: err.to_string(),
			})?;
		eval(env, &lib_text).map_err(|err| RuntimeError::LibError {
			lib_name: filename.clone(),
			nested_error: Box::new(err),
		})
	}
    invalid @ _ => {
		Err(RuntimeError::UnaryTypeMismatch {
			context: "import",
			expected: Type::String,
			actual: invalid.get_type(),
		})
	}
}
}

fn eval_assignment(env: &mut SharedEnv, lhs: Sym, rhs: Box<Expr>) -> EvalResult {
	let rhs_value = eval_expr(env, *rhs)?;
	// Perform the assignment, possibly overwriting the existing value.
	env.lock().unwrap().assign(lhs, rhs_value);
	Ok(Val::empty())
}

fn eval_branch(env: &mut SharedEnv, test: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr>) -> EvalResult {
	let test_value = eval_expr(env, *test)?;
	match test_value {
		Val::Prim(Prim::Bool(b)) => eval_expr(env, if b.into() { *then_expr } else { *else_expr }),
		_ => Err(RuntimeError::UnaryTypeMismatch {
			context: "conditional test",
			expected: Type::Boolean,
			actual: test_value.get_type(),
		}),
	}
}

fn eval_while_loop(env: &mut SharedEnv, test: Box<Expr>, body: Box<Expr>) -> EvalResult {
	loop {
		// Evaluate the test condition.
		let test_value = eval_expr(env, (*test).clone())?;
		match test_value {
			Val::Prim(Prim::Bool(b)) => if b.into() {
				// Evaluate next iteration.
				match eval_expr(env, (*body).clone())? {
					Val::Tuple(Tuple { elems }) if elems.is_empty() => {
						// Non-final empty results are okay.
					},
					Val::Returned { result } => {
						// Break out of loop via return.
						return Ok(*result)
					},
					unused @ _ => {
						// Non-final, non-returned, non-empty results are errors.
						return Err(RuntimeError::UnusedResult(unused.to_string(env)));
					},
				}
			} else {
				// End of loop.
				return Ok(Val::empty());
			},
			_ => print!("while-loop test value must be boolean, found {}", test_value.to_string(&env)),
		}
	}
}

fn eval_for_loop(env: &mut SharedEnv, loop_var: Sym, range: Box<Expr>, body: Box<Expr>) -> EvalResult {
	let range_value = eval_expr(env, *range)?;
	match range_value {
		Val::List(range_list) => {
			for value in range_list.iter() {
				// Assign the loop variable to the current value in the range list.
				env.lock().unwrap().assign(loop_var.clone(), value.clone());
				// Evaluate the loop body in this context.
				match eval_expr(env, (*body).clone())? {
					Val::Tuple(Tuple { elems }) if elems.is_empty() => {
						// Non-final empty results are okay.
					},
					Val::Returned { result } => {
						// Break out of loop via return.
						return Ok(*result)
					},
					unused @ _ => {
						// Non-final, non-returned, non-empty results are errors.
						return Err(RuntimeError::UnusedResult(unused.to_string(env)));
					},
				}

			}
			Ok(Val::empty())
		},
		_ => Err(RuntimeError::UnaryTypeMismatch {
			context: "for-loop range".into(),
			expected: Type::List,
			actual: range_value.get_type()
		}),
	}
}

/// Evaluates a numerical operation on two values.
fn bin_num_op(env: &mut SharedEnv, left: Val, right: Val, op_name: &'static str, op: fn(Num, Num) -> EvalResult) -> EvalResult {
	match (left, right) {
		// Number op Number
		(Val::Prim(Prim::Num(left)), Val::Prim(Prim::Num(right))) => Ok(op(left, right)?),
		// List op Number
		(Val::List(list), Val::Prim(Prim::Num(number))) => {
			Ok(Val::List(list.map(|elem| bin_num_op(env, elem.clone(), Val::Prim(Prim::Num(number.clone())), op_name, op))?))
		},
		// Number op List
		(Val::Prim(Prim::Num(number)), Val::List(list)) => {
			Ok(Val::List(list.map(|elem| bin_num_op(env, Val::Prim(Prim::Num(number.clone())), elem.clone(), op_name, op))?))
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

/// Tries to extract an idex from `list`, mod `n`.
fn get_idx_from_list(env: &SharedEnv, list: List, n: usize) -> Result<usize, RuntimeError> {
	// Check for exactly one element that fits in an i64.
	if let (Some(value), Some(true)) = (list.head(), list.tail().as_ref().map(List::is_empty)) {
		if let Some(signed_idx) = value.as_i64() {
			let signed_len = n as i64;
			let signed_idx = ((signed_idx % signed_len) + signed_len) % signed_len;
			return Ok(signed_idx as usize);
		}
	}
	Err(RuntimeError::InvalidIndex { idx: list.to_string(&env) })
}

fn eval_evaluated_cluster(env: &mut SharedEnv, mut cluster: Vec<EvaluatedClusterItem>) -> EvalResult {
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
		// List/string indexing
		for idx in 0..cluster.len() - 1 {
			if let ClusterConnector::AdjNonparen = &cluster[idx + 1].connector {
				cluster[idx].value = match (&cluster[idx].value, &cluster[idx + 1].value) {
					// List index
					(Val::List(left), Val::List(right)) => {
						if left.is_empty() {
							Err(RuntimeError::OutOfBounds)
						} else {
							// Can safely unwrap because idx is guaranteed to be in bounds.
							Ok(left.nth(get_idx_from_list(&env, right.clone(), left.len())?).unwrap())
						}
					},
					// String index
					(Val::Prim(Prim::String(left)), Val::List(right)) => {
						if left.is_empty() {
							Err(RuntimeError::OutOfBounds)
						} else {
							// Can safely unwrap because idx is guaranteed to be in bounds.
							Ok(Val::from(left.chars().nth(get_idx_from_list(&env, right.clone(), left.len())?).unwrap().to_string()))
						}
					},
					_ => continue,
				}?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
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
		// Multiplications and divisions
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
fn eval_cluster(env: &mut SharedEnv, cluster: Cluster) -> EvalResult {
	// First, evaluate the cluster items.
	let mut evaluated_cluster = Vec::with_capacity(cluster.items.len());
	for item in cluster.items {
		let value = eval_expr(env, *item.expr)?;
		evaluated_cluster.push(EvaluatedClusterItem {
			value: if item.negated { eval_numeric_negation(env, value)? } else { value },
			connector: item.connector,
		});
	}
	// Now that the items' types are known, evaluate down to a single value and negate if necessary.
	let non_negated_result = eval_evaluated_cluster(env, evaluated_cluster)?;
	if cluster.negated {
		eval_numeric_negation(env, non_negated_result)
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
		local_env.lock().unwrap().assign(variable, value);
	}
	// Evaluate function body within the application environment.
	match c.f.body {
		LambdaBody::UserDefined(body) => eval_expr(&mut local_env, *body),
		LambdaBody::Intrinsic(body) => {
			match body {
				Intrinsic::Pop => match local_env.lock().unwrap().lookup(&"list".into()).unwrap() {
					Val::List(list) => Ok(Val::List(list.tail().ok_or(RuntimeError::OutOfBounds)?)),
					arg @ _ => Err(RuntimeError::UnaryTypeMismatch {
						context: "pop()",
						expected: Type::List,
						actual: arg.get_type()
					}),
				},
				Intrinsic::Print => {
					println!("{}", local_env.lock().unwrap().lookup(&"value".into()).unwrap().to_string(&local_env));
					Ok(Val::empty())
				},
				Intrinsic::Read => {
					let mut input = String::new();
					io::stdin().read_line(&mut input).unwrap();
					Ok(Val::from(input.trim().to_string()))
				},
				Intrinsic::GetType => Ok(Val::Prim(Prim::Type(local_env.lock().unwrap().lookup(&"value".into()).unwrap().get_type()))),
			}
		}
	}
}

fn eval_numeric_negation(env: &mut SharedEnv, value: Val) -> EvalResult {
	match value {
		Val::Prim(Prim::Num(Num::Integer(integer))) => Ok(Val::Prim(Prim::Num(Num::Integer(-integer)))),
		Val::Prim(Prim::Num(Num::Rational(rational))) => Ok(Val::Prim(Prim::Num(Num::Rational(-rational)))),
		Val::Prim(Prim::Num(Num::Real(real))) => Ok(Val::Prim(Prim::Num(Num::Real(-real)))),
		Val::List(list) => Ok(Val::List(list.map(|elem| eval_numeric_negation(env, elem.clone()))?)),
		_ => Err(RuntimeError::UnaryTypeMismatch {
			context: "negation",
			expected: Type::Boolean,
			actual: value.get_type(),
		}),
	}
}

#[cfg(test)]
mod tests {
	use crate::env::{Env, SharedEnv};
	use crate::errors::Error;
	use crate::interpreter::eval;
	use crate::list::List;
	use crate::numbers::Num;
	use crate::primitives::Prim;
	use crate::tuple::Tuple;
	use crate::types::Type;
	use crate::values::Val;

	use bigdecimal::BigDecimal;

	use std::str::FromStr;

	#[test]
	fn empty_input_evaluates_to_empty() -> Result<(), Error> {
		assert_eq!(Val::empty(), eval(&mut Env::new(None), "")?);
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
			assert_eq!(Val::from(true), eval(&mut env, "(1/3 as real) ~ 0.333333333333")?);
			assert_eq!(Val::from(true), eval(&mut env, "1/3 ~ 0.333333333333")?);
			assert_eq!(Val::from(false), eval(&mut env, "(1/3 as real) ~ 0.333")?);
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

	#[test]
	fn subtraction_and_negation() -> Result<(), Error> {
		assert_eq!(Val::from(3), eval(&mut Env::new(None), "-1+-2*-2")?);
		Ok(())
	}

	#[test]
	fn simple_compound_expression_with_parentheses() -> Result<(), Error> {
		assert_eq!(Val::from(5), eval(&mut Env::new(None), "-5 * (1 + -2)")?);
		Ok(())
	}

	#[test]
	fn exponentiation() -> Result<(), Error> {
		assert_eq!(Val::from(1), eval(&mut Env::new(None), "2^0")?);
		assert_eq!(Val::from(2), eval(&mut Env::new(None), "2^1")?);
		assert_eq!(Val::from(16), eval(&mut Env::new(None), "2^4")?);
		assert_eq!(Val::from(32), eval(&mut Env::new(None), "2^5")?);
		assert_eq!(Val::from(Num::rational(1, 4)), eval(&mut Env::new(None), "2^-2")?);
		Ok(())
	}

	#[test]
	fn rational_negation() -> Result<(), Error> {
		assert_eq!(Val::from(Num::rational(-1, 2)), eval(&mut Env::new(None), "-1/2")?);
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
		eval(&mut env, "let x = 42")?;
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
			let expected = make_tuple_value!(Val::from(1), make_tuple_value!(Val::from(2), Val::from(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1, (2, 3))")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers_and_booleans() -> Result<(), Error> {
			let expected = make_tuple_value!(Val::from(true), make_tuple_value!(Val::from(2), Val::from(false)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1 < 2, (2, false))")?);
			Ok(())
		}
	}

	mod list_construction {
		use super::*;
		#[test]
		fn singleton_list() -> Result<(), Error> {
			assert_eq!(make_list_value!(Val::from(1)), eval(&mut Env::new(None), "[1]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers() -> Result<(), Error> {
			let expected = make_list_value!(Val::from(1), make_list_value!(Val::from(2), Val::from(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "[1, [2, 3]]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers_and_booleans() -> Result<(), Error> {
			let expected = make_list_value!(Val::from(true), make_list_value!(Val::from(2), Val::from(false)));
			assert_eq!(expected, eval(&mut Env::new(None), "[1 < 2, [2, false]]")?);
			Ok(())
		}
	}

	#[test]
	fn list_destruction_does_not_cause_stack_overflow() {
		let result = eval(&mut Env::new(None), r"{
			let i = 0;
			let l = [];
			while i < 1000 do {
				let l = [i] | l;
				let i = i + 1;
			}
		}");
		assert!(result.is_ok());
	}

	mod math_operations_on_lists {
		use super::*;
		#[test]
		fn negation() -> Result<(), Error> {
			let expected = make_list_value!(Val::from(-1), Val::from(-2), Val::from(-3));
			assert_eq!(expected, eval(&mut Env::new(None), "-[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::from(2), Val::from(3), Val::from(4));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3] + 1")?);
			assert_eq!(expected, eval(&mut env, "1 + [1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::from(1), Val::from(2), Val::from(3));
			assert_eq!(expected, eval(&mut env, "[2, 3, 4]-1")?);
			assert_eq!(expected, eval(&mut env, "4-[3, 2, 1]")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::from(2), Val::from(4), Val::from(6));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3]2")?);
			assert_eq!(expected, eval(&mut env, "2[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::from(1), Val::from(2), Val::from(3));
			assert_eq!(expected, eval(&mut env, "[2, 4, 6]/2")?);
			assert_eq!(expected, eval(&mut env, "6/[6, 3, 2]")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), Error> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::from(1), Val::from(4), Val::from(16));
			assert_eq!(expected, eval(&mut env, "[1, 2, 4]^2")?);
			assert_eq!(expected, eval(&mut env, "2^[0, 2, 4]")?);
			Ok(())
		}
	}

	mod concatenation {
		use super::*;
		#[test]
		fn string_concatenation() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("hello, 1".to_string()), eval(&mut env, r#""hello, " | 1"#)?);
			assert_eq!(Val::from("1 world".to_string()), eval(&mut env, r#"1 | " world""#)?);
			assert_eq!(Val::from("hello, world".to_string()), eval(&mut env, r#""hello, " | "world""#)?);
			Ok(())
		}
		#[test]
		fn list_concatenation() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(Val::from(1), Val::from(2)), eval(&mut env, r#"[1] | [2]"#)?);
			assert_eq!(Val::from("1[2]".to_string()), eval(&mut env, r#""1" | [2]"#)?);
			assert_eq!(Val::from("[1]2".to_string()), eval(&mut env, r#"[1] | "2""#)?);
			Ok(())
		}
	}

	mod indexing {
		use super::*;
		#[test]
		fn valid_list_indexing() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(1), eval(&mut env, "[1, 2][0]")?);
			assert_eq!(Val::from(2), eval(&mut env, "[1, 2][1]")?);
			assert_eq!(Val::from(1), eval(&mut env, "[1, 2][2]")?);
			assert_eq!(Val::from(2), eval(&mut env, "[1, 2][-1]")?);
			Ok(())
		}
		#[test]
		fn invalid_list_indexing() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert!(eval(&mut env, "[][0]").is_err());
			assert!(eval(&mut env, "[1][true]").is_err());
			assert!(eval(&mut env, "[1][1, 2]").is_err());
			Ok(())
		}
		#[test]
		fn valid_string_indexing() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("h".to_string()), eval(&mut env, r#""hi"[0]"#)?);
			assert_eq!(Val::from("i".to_string()), eval(&mut env, r#""hi"[1]"#)?);
			assert_eq!(Val::from("h".to_string()), eval(&mut env, r#""hi"[2]"#)?);
			assert_eq!(Val::from("i".to_string()), eval(&mut env, r#""hi"[-1]"#)?);
			Ok(())
		}
		#[test]
		fn invalid_string_indexing() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert!(eval(&mut env, r#"""[0]"#).is_err());
			assert!(eval(&mut env, r#""hi"[true]"#).is_err());
			assert!(eval(&mut env, r#""hi"[1, 2]"#).is_err());
			Ok(())
		}
	}

	#[test]
	fn simple_function_application() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, "let f = () -> 42")?;
		assert_eq!(Val::from(42), eval(&mut env, "f()")?);
		Ok(())
	}

	mod order_of_operations {
		use super::*;
		fn env_with_inc() -> Result<SharedEnv, Error> {
			let mut env = Env::new(None);
			eval(&mut env, "let inc = a -> a + 1")?;
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
		eval(&mut env, "let apply = (f, a) -> f(a)")?;
		assert_eq!(Val::from(42), eval(&mut env, "apply(a -> a, 42)")?);
		Ok(())
	}

	#[test]
	fn curried_function() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, "let sum = a -> b -> a + b")?;
		assert_eq!(Val::from(3), eval(&mut env, "sum 1 2")?);
		Ok(())
	}

	#[test]
	fn environment_does_not_persist_between_function_chains() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let get_a = () -> a;
		}")?;
		// "a" should be undefined.
		assert!(eval(&mut env, "sum (1) (2) get_a ()").is_err());
		Ok(())
	}

	#[test]
	fn chained_application_with_and_without_parentheses() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let inc = a -> a + 1;
		}")?;
		assert_eq!(Val::from(3), eval(&mut env, "sum (1) 2")?);
		Ok(())
	}

	#[test]
	fn chained_application_does_not_pollute_applications_higher_in_the_call_chain() -> Result<(), Error> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let inc = b -> b + 1;
		}")?;
		assert_eq!(Val::from(8), eval(&mut env, "sum (inc 5) 2")?);
		Ok(())
	}

	#[test]
	fn returning_from_nested_block() -> Result<(), Error> {
		let mut env = Env::new(None);
		assert_eq!(Val::from(7), eval(&mut env, r"{
			let f = x -> {
				if x = 3 then {
					return 7
				};
				x
			};
			f(3)
		}")?);
		Ok(())
	}

	mod blocks {
		use super::*;
		#[test]
		fn empty_block() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::empty(), eval(&mut env, "{}")?);
			assert_eq!(Val::empty(), eval(&mut env, "{;;;}")?);
			Ok(())
		}
		#[test]
		fn compound_block() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), "{();();1}")?);
			Ok(())
		}
		#[test]
		fn unused_result() -> Result<(), Error> {
			assert!(eval(&mut Env::new(None), "{1;2}").is_err());
			assert!(eval(&mut Env::new(None), "{1;}").is_err());
			Ok(())
		}
		#[test]
		fn nested_blocks() -> Result<(), Error> {
			let mut env = Env::new(None);
			eval(&mut env, "{ let a = 0 }")?;
			eval(&mut env, "{ let b = { let a = a + 1; a } }")?;
			assert_eq!(Val::from(1), eval(&mut env, "b")?);
			Ok(())
		}
		#[test]
		fn return_outside_block_is_okay() -> Result<(), Error> {
			assert_eq!(Val::Returned { result: Box::new(Val::from(1)) }, eval(&mut Env::new(None), "return 1")?);
			Ok(())
		}
		#[test]
		fn return_from_nested_expr() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), r"{
				if true then {
					return 1
				};
				2
			}")?);
			Ok(())
		}
		#[test]
		fn return_from_called_function() -> Result<(), Error> {
			assert_eq!(Val::from(1), eval(&mut Env::new(None), r"{
				let f = () -> return 1;
				f();
				2
			}")?);
			Ok(())
		}
	}

	mod branch_statements {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), Error> {
			let mut env = Env::new(None);
			eval(&mut env, "if false then let a = 1/0 else let a = 1")?;
			assert_eq!(Val::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), Error> {
			let mut env = Env::new(None);
			eval(&mut env, "if true then let a = 1 else let a = 1/0")?;
			assert_eq!(Val::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn no_else_statement() -> Result<(), Error> {
			assert_eq!(Val::empty(), eval(&mut Env::new(None), "if false then let a = 1/0")?);
			Ok(())
		}
	}

	mod while_loops {
		use super::*;
		#[test]
		fn basic_while_loop() -> Result<(), Error> {
			let mut env = Env::new(None);
			eval(&mut env, r"{
				let a = 0;
				while a < 3 do let a = a + 1;
			}")?;
			assert_eq!(Val::from(3), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn unused_result() -> Result<(), Error> {
			assert!(eval(&mut Env::new(None), "while true do 1").is_err());
			Ok(())
		}
		#[test]
		fn early_return() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(7), eval(&mut env, r"{
				let a = 0;
				while a < 3 do {
					return return 7;
					let a = a + 1;
				}
			}")?);
			assert_eq!(Val::from(0), eval(&mut env, "a")?);
			Ok(())
		}
	}

	mod for_loops {
		use super::*;
		#[test]
		fn basic_for_loops() -> Result<(), Error> {
			let mut env = Env::new(None);
			eval(&mut env, r"{
				let a = 0;
				for x in [1, 2, 3] do let a = a + x;
				for x in [] do let a = 10;
			}")?;
			assert_eq!(Val::from(6), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn unused_result() -> Result<(), Error> {
			assert!(eval(&mut Env::new(None), "for x in [1, 2, 3] true do 1").is_err());
			Ok(())
		}
		#[test]
		fn early_return() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(7), eval(&mut env, r"{
				for x in [1, 2, 3] do {
					return return 7
				};
			}")?);
			assert_eq!(Val::from(1), eval(&mut env, "x")?);
			Ok(())
		}
	}

	mod intrinsics {
		use super::*;
		#[test]
		fn pop() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(), eval(&mut env, "pop([1])")?);
			assert!(eval(&mut env, "pop([])").is_err());
			Ok(())
		}
		#[test]
		fn get_type() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::Prim(Prim::Type(Type::Integer)), eval(&mut env, "get_type 1")?);
			assert_eq!(Val::Prim(Prim::Type(Type::Real)), eval(&mut env, "get_type 1.5")?);
			assert_eq!(Val::Prim(Prim::Type(Type::List)), eval(&mut env, "get_type []")?);
			assert_eq!(Val::Prim(Prim::Type(Type::List)), eval(&mut env, "get_type [1]")?);
			assert_eq!(Val::Prim(Prim::Type(Type::Type)), eval(&mut env, "get_type(get_type(1))")?);
			Ok(())
		}
	}

	mod type_conversions {
		use super::*;
		#[test]
		fn type_to_itself() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(1), eval(&mut env, "1 as integer")?);
			assert_eq!(Val::from("hello".to_string()), eval(&mut env, r#""hello" as string"#)?);
			assert_eq!(Val::from(true), eval(&mut env, "(x -> x) = (x -> x) as closure")?);
			Ok(())
		}
		#[test]
		fn domain_expansion() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(Num::rational(1, 1)), eval(&mut env, "1 as rational")?);
			assert_eq!(Val::from(Num::Real(1.into())), eval(&mut env, "1 as real")?);
			assert_eq!(Val::from(Num::Real(1.into())), eval(&mut env, "1 as rational as real")?);
			Ok(())
		}
		#[test]
		fn tuple_list_conversion() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(Val::from(1), Val::from(2)), eval(&mut env, "(1, 2) as list")?);
			assert_eq!(make_tuple_value!(Val::from(1), Val::from(2)), eval(&mut env, "[1, 2] as tuple")?);
			Ok(())
		}
		#[test]
		fn conversion_to_string() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("true".to_string()), eval(&mut env, "true as string")?);
			assert_eq!(Val::from("1".to_string()), eval(&mut env, "1 as string")?);
			assert_eq!(Val::from("(1, 2)".to_string()), eval(&mut env, "(1, 2) as string")?);
			Ok(())
		}
		#[test]
		fn invalid_conversions() -> Result<(), Error> {
			let mut env = Env::new(None);
			assert!(eval(&mut env, "1.5 as integer").is_err());
			assert!(eval(&mut env, r#""five" as real"#).is_err());
			assert!(eval(&mut env, "(x -> x) as boolean").is_err());
			Ok(())
		}
	}

	#[test]
	fn core_constants() -> Result<(), Error> {
		let mut env = Env::new(None);
		let pi_str = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
		let expected = Val::Prim(Prim::Num(Num::Real(BigDecimal::from_str(pi_str).unwrap())));
		eval(&mut env, "import \"core/constants.gynj\"")?;
		assert_eq!(expected, eval(&mut env, "PI")?);
		Ok(())
	}

	mod core_libs {
		use super::*;
		mod basic_math {
			use super::*;
			#[test]
			fn absolute_value() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::from(5), eval(&mut env, "abs 5")?);
				assert_eq!(Val::from(5), eval(&mut env, "abs(-5)")?);
				Ok(())
			}
		}
		mod combinatorics {
			use super::*;
			#[test]
			fn factorial() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::from(120), eval(&mut env, "fact 5")?);
				Ok(())
			}
			#[test]
			fn permutations() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::from(60), eval(&mut env, "nPk(5, 3)")?);
				Ok(())
			}
			#[test]
			fn combinations() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				// Using an epsilon here because of the division.
				assert_eq!(Val::from(true), eval(&mut env, "abs(nCk(5, 3) - 10) < 10**-50")?);
				Ok(())
			}
		}
		mod list_ops {
			use super::*;
			#[test]
			fn len() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::from(0), eval(&mut env, "len []")?);
				assert_eq!(Val::from(3), eval(&mut env, "len [1, 2, 3]")?);
				Ok(())
			}
			#[test]
			fn nth() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert!(eval(&mut env, "nth([], 0)").is_err());
				assert_eq!(Val::from(2), eval(&mut env, "nth([1, 2, 3], 1)")?);
				Ok(())
			}
			#[test]
			fn reverse() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(3), Val::from(2), Val::from(1)), eval(&mut env, "reverse [1, 2, 3]")?);
				Ok(())
			}
			#[test]
			fn insert_into_non_empty() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				let expected = make_list_value!(Val::from(1), Val::from(2), Val::from(3));
				assert_eq!(expected, eval(&mut env, "insert([1, 3], 1, 2)")?);
				Ok(())
			}
			#[test]
			fn insert_into_empty() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(1)), eval(&mut env, "insert([], 0, 1)")?);
				Ok(())
			}
			#[test]
			fn remove_from_middle() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(1), Val::from(3)), eval(&mut env, "remove([1, 2, 3], 1)")?);
				Ok(())
			}
			#[test]
			fn remove_from_beginning() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(2), Val::from(3)), eval(&mut env, "remove([1, 2, 3], 0)")?);
				Ok(())
			}
			#[test]
			fn map() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(1), Val::from(4), Val::from(9)), eval(&mut env, "map([1, 2, 3], x -> x^2)")?);
				Ok(())
			}
			#[test]
			fn reduce() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::from(6), eval(&mut env, "reduce([1, 2, 3], 0, (a, b) -> a + b)")?);
				Ok(())
			}
			#[test]
			fn flatmap() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				let expected = make_list_value!(Val::from(1), Val::from(1), Val::from(2), Val::from(2));
				assert_eq!(expected, eval(&mut env, "flatmap([1, 2], x -> [x, x])")?);
				Ok(())
			}
			#[test]
			fn range() -> Result<(), Error> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::from(1), Val::from(2), Val::from(3)), eval(&mut env, "range(1, 3)")?);
				Ok(())
			}
		}
	}	
}
