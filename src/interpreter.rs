use super::env::{Env, SharedEnv};
use super::errors::{GynjoErr, RtErr};
use super::exprs::{Expr, BinExpr, BinOp, Cluster, ClusterConnector, LambdaBody};
use super::format_with_env::FormatWithEnv;
use super::intrinsics::Intrinsic;
use super::lexer::lex;
use super::list::List;
use super::numbers::Num;
use super::primitives::{Prim, Bool};
use super::parser::parse;
use super::quantity::Quant;
use super::symbol::Sym;
use super::tuple::Tuple;
use super::types::{Type, NumType};
use super::unit_map::UnitMap;
use super::values::{Closure, Val};

use std::collections::VecDeque;
use std::io;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Val, RtErr>;

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
		Expr::Prim(primitive) => Ok(match primitive {
			// Convert number into dimensionless quantity.
			Prim::Num(number) => Val::scalar(number.shrink_domain()),
			primitive @ _ => Val::Prim(primitive),
		}),
		Expr::Unit(unit) => eval_unit(&mut env, unit),
		Expr::DeclUnit { dimension, name, scale } => eval_unit_declaration(&mut env, dimension, name, scale),
		Expr::Import { target } => eval_import(&mut env, target),
		Expr::Assign { lhs, rhs } => eval_assignment(&mut env, lhs, rhs),
		Expr::Branch { test, then_expr, else_expr } => eval_branch(&mut env, test, then_expr, else_expr),
		Expr::WhileLoop { test, body } => eval_while_loop(env, test, body),
		Expr::ForLoop { loop_var, range, body } => eval_for_loop(env, loop_var, range, body),
		Expr::Break => Ok(Val::Break),
		Expr::Return { result } => Ok(match eval_expr(env, *result)? {
			// Return is idempotent; don't wrap the return value if it's already wrapped.
			result @ Val::Return { .. } => result,
			// Wrap non-returned value into returned value.
			result @ _ => Val::Return { result: Box::new(result) },
		}),
		Expr::Read => {
			let mut input = String::new();
			io::stdin().read_line(&mut input).unwrap();
			Ok(Val::from(input.trim().to_string()))
		},
		Expr::Write { output } => {
			println!("{}", eval_expr(&mut env, *output)?.format_with_env(env));
			Ok(Val::empty())
		},
		Expr::GetType { expr } => Ok(Val::Prim(Prim::Type(eval_expr(&mut env, *expr)?.get_type()))),
	}
}

/// If possible, computes the value of the expression contained in `input` in the context of `env`.
pub fn eval(env: &mut SharedEnv, input: &str) -> Result<Val, GynjoErr> {
	// Lex.
	let tokens = lex(input).map_err(GynjoErr::lex)?;
	// Parse.
	let expr = parse(&tokens[..]).map_err(GynjoErr::parse)?;
	// Evaluate.
	eval_expr(env, expr).map_err(GynjoErr::rt)
}

fn eval_block(env: &mut SharedEnv, mut exprs: Box<Vec<Expr>>) -> EvalResult {
	let last = exprs.pop();
	if let Some(last) = last {
		for expr in exprs.into_iter() {
			match eval_expr(env, expr)? {
				Val::Tuple(Tuple { elems }) if elems.is_empty() => {
					// Non-final empty results are okay.
				},
				Val::Break => {
					// Break out of block.
					return Ok(Val::Break)
				},
				result @ Val::Return { .. } => {
					// Return non-final result.
					return Ok(result)
				},
				unused @ _ => {
					// Non-final, non-returned, non-empty results are errors.
					return Err(RtErr::UnusedResult(unused.format_with_env(env)));
				},
			}
		}
		// No break or early return. Return final value.
		Ok(eval_expr(env, last)?)
	} else {
		Ok(Val::empty())
	}
}

fn eval_bin_expr(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match bin_expr.op {
		BinOp::As => eval_as(env, bin_expr),
		BinOp::And => eval_and(env, bin_expr),
		BinOp::Or => eval_or(env, bin_expr),
		BinOp::Eq => Ok(Bool::from(eval_expr(env, *bin_expr.left)? == eval_expr(env, *bin_expr.right)?).into()),
		BinOp::Neq => Ok(Bool::from(eval_expr(env, *bin_expr.left)? != eval_expr(env, *bin_expr.right)?).into()),
		BinOp::Approx => eval_approx(env, bin_expr),
		BinOp::Lt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a < b)))
		},
		BinOp::Leq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a <= b)))
		},
		BinOp::Gt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a > b)))
		},
		BinOp::Geq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a >= b)))
		},
		BinOp::Add => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "addition", |a, b| {
				Ok(Val::from((a + b).map_err(RtErr::quant)?))
			})
		},
		BinOp::Sub => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "subtraction", |a, b| {
				Ok(Val::from((a - b).map_err(RtErr::quant)?))
			})
		},
		BinOp::Concat => eval_concat(env, bin_expr),
	}
}

fn eval_as(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.right)? {
		Val::Prim(Prim::Type(to)) => {
			match (eval_expr(env, *bin_expr.left)?, to) {
				// T -> T
				(value @ _, to @ _) if value.get_type() == to => Ok(value),
				// integer -> rational
				(Val::Quant(Quant { val: Num::Integer(val), units }), Type::Quant(NumType::Rational)) => {
					Ok(Quant { val: Num::Rational(val.into()), units }.into())
				},
				// integer | rational | real -> real
				(Val::Quant(quant), Type::Quant(NumType::Real)) => {
					Ok(Quant { val: Num::Real(quant.val.into()), units: quant.units }.into())
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
				(value @ _, Type::String) => Ok(Val::Prim(Prim::String(value.format_with_env(env)))),
				// Invalid conversion
				(value @ _, to @ _) => Err(RtErr::InvalidTypeCast { from: value.get_type(), to }),
			}
		},
		invalid @ _ => Err(RtErr::UnaryTypeMismatch {
			context: "type cast",
			expected: vec!(Type::Type),
			actual: invalid.get_type(),
		})
	}
}

fn eval_and(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.left)? {
		Val::Prim(Prim::Bool(left)) => if left.into() {
			match eval_expr(env, *bin_expr.right)? {
				Val::Prim(Prim::Bool(right)) => Ok(right.into()),
				invalid @ _ => Err(RtErr::UnaryTypeMismatch {
					context: "logical conjunction",
					expected: vec!(Type::Boolean),
					actual: invalid.get_type(),
				}),
			}
		} else {
			// Short-circuit to false.
			Ok(Bool::False.into())
		},
		invalid @ _ => Err(RtErr::UnaryTypeMismatch {
			context: "logical conjunction",
			expected: vec!(Type::Boolean),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_or(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.left)? {
		Val::Prim(Prim::Bool(left)) => if left.into() {
			// Short-circuit to true.
			Ok(Bool::True.into())
		} else {
			match eval_expr(env, *bin_expr.right)? {
				Val::Prim(Prim::Bool(right)) => Ok(right.into()),
				invalid @ _ => Err(RtErr::UnaryTypeMismatch {
					context: "logical disjunction",
					expected: vec!(Type::Boolean),
					actual: invalid.get_type(),
				}),
			}
		},
		invalid @ _ => Err(RtErr::UnaryTypeMismatch {
			context: "logical disjunction",
			expected: vec!(Type::Boolean),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_approx(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	let left = eval_expr(env, *bin_expr.left)?;
	let right = eval_expr(env, *bin_expr.right)?;
	Ok(Val::from(match (left, right) {
		(Val::Quant(left), Val::Quant(right)) => {
			let left_string = Quant { val: left.val.expand_domain(), units: left.units }.format_with_env(env);
			let right_string = Quant { val: right.val.expand_domain(), units: right.units }.format_with_env(env);
			left_string == right_string
		},
		(left @ _, right @ _) => left == right,
	}))
}

fn eval_concat(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	let left = eval_expr(env, *bin_expr.left)?;
	let right = eval_expr(env, *bin_expr.right)?;
	match (left, right) {
		// String concatenation
		(Val::Prim(Prim::String(left)), Val::Prim(Prim::String(right))) => {
			Ok(Val::from(format!("{}{}", left, right)))
		},
		(Val::Prim(Prim::String(left)), right @ _) => {
			Ok(Val::from(format!("{}{}", left, right.format_with_env(&env))))
		},
		(left @ _, Val::Prim(Prim::String(right))) => {
			Ok(Val::from(format!("{}{}", left.format_with_env(&env), right)))
		},
		// List concatenation
		(Val::List(left), Val::List(right)) => Ok(Val::List(left.concat(right))),
		// Invalid concatenation
		(left @ _, right @ _) => Err(RtErr::BinaryTypeMismatch {
			context: "concatenation",
			left: left.get_type(),
			right: right.get_type(),
		})
	}
}

fn eval_not(env: &mut SharedEnv, expr: Box<Expr>) -> EvalResult {
	match eval_expr(env, *expr)? {
		Val::Prim(Prim::Bool(b)) => Ok(Bool::from(!bool::from(b)).into()),
		invalid @ _ => Err(RtErr::UnaryTypeMismatch {
			context: "logical negation",
			expected: vec!(Type::Boolean),
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
	env.lock().unwrap().get_var(&symbol)
		.map(|v| v)
		.ok_or(RtErr::Undefined(symbol.name))
}

fn eval_unit(env: &mut SharedEnv, unit: String) -> EvalResult {
	env.lock().unwrap().get_unit(&unit)
		.map(|dimension_unit| Val::Quant(UnitMap::from(dimension_unit).into()))
		.ok_or(RtErr::Undefined(unit))
}

fn eval_unit_declaration(env: &mut SharedEnv, dimension: Sym, name: String, scale: Box<Expr>) -> EvalResult {
	// Evaluate the scale expression.
	match eval_expr(env, *scale)? {
		// Have to match a quantity rather than a number because numbers get eagerly converted to dimensionless quantities.
		Val::Quant(scale) => {
			// Perform the unit assignment, possibly overwriting the existing value.
			env.lock().unwrap().set_unit(dimension, name, scale.to_scalar().map_err(RtErr::quant)?);
			Ok(Val::empty())
		},
		invalid @ _ => Err(RtErr::UnaryTypeMismatch {
			context: "unit declaration",
			expected: vec!(Type::Quant(NumType::Integer), Type::Quant(NumType::Rational), Type::Quant(NumType::Real)),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_import(env: &mut SharedEnv, target: Box<Expr>) -> EvalResult {
	match eval_expr(env, *target)? {
	Val::Prim(Prim::String(filename)) => {
		let lib_text = std::fs::read_to_string(&filename)
			.map_err(|err| RtErr::CouldNotOpenFile {
				filename: filename.clone(),
				file_error: err.to_string(),
			})?;
		eval(env, &lib_text).map_err(|err| RtErr::LibErr {
			lib_name: filename,
			nested_error: Box::new(err),
		})
	}
	invalid @ _ => {
		Err(RtErr::UnaryTypeMismatch {
			context: "import",
			expected: vec!(Type::String),
			actual: invalid.get_type(),
		})
	}
}
}

fn eval_assignment(env: &mut SharedEnv, lhs: Sym, rhs: Box<Expr>) -> EvalResult {
	let rhs_value = eval_expr(env, *rhs)?;
	// Perform the variable assignment, possibly overwriting the existing value.
	env.lock().unwrap().set_var(lhs, rhs_value);
	Ok(Val::empty())
}

fn eval_branch(env: &mut SharedEnv, test: Box<Expr>, then_expr: Box<Expr>, else_expr: Box<Expr>) -> EvalResult {
	let test_value = eval_expr(env, *test)?;
	match test_value {
		Val::Prim(Prim::Bool(b)) => eval_expr(env, if b.into() { *then_expr } else { *else_expr }),
		_ => Err(RtErr::UnaryTypeMismatch {
			context: "conditional test",
			expected: vec!(Type::Boolean),
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
					Val::Break => {
						// Break out of loop.
						return Ok(Val::empty())
					}
					result @ Val::Return { .. } => {
						// Exit loop via return.
						return Ok(result)
					},
					unused @ _ => {
						// Non-final, non-returned, non-empty results are errors.
						return Err(RtErr::UnusedResult(unused.format_with_env(env)));
					},
				}
			} else {
				// End of loop.
				return Ok(Val::empty());
			},
			_ => print!("while-loop test value must be boolean, found {}", test_value.format_with_env(&env)),
		}
	}
}

fn eval_for_loop(env: &mut SharedEnv, loop_var: Sym, range: Box<Expr>, body: Box<Expr>) -> EvalResult {
	let range_value = eval_expr(env, *range)?;
	match range_value {
		Val::List(range_list) => {
			for value in range_list.iter() {
				// Assign the loop variable to the current value in the range list.
				env.lock().unwrap().set_var(loop_var.clone(), value.clone());
				// Evaluate the loop body in this context.
				match eval_expr(env, (*body).clone())? {
					Val::Tuple(Tuple { elems }) if elems.is_empty() => {
						// Non-final empty results are okay.
					},
					Val::Break => {
						// Break out of loop.
						return Ok(Val::empty())
					}
					result @ Val::Return { .. } => {
						// Exit loop via return.
						return Ok(result)
					},
					unused @ _ => {
						// Non-final, non-returned, non-empty results are errors.
						return Err(RtErr::UnusedResult(unused.format_with_env(env)));
					},
				}

			}
			Ok(Val::empty())
		},
		_ => Err(RtErr::UnaryTypeMismatch {
			context: "for-loop range".into(),
			expected: vec!(Type::List),
			actual: range_value.get_type()
		}),
	}
}

/// Evaluates a numerical operation on two values.
fn eval_bin_op(env: &mut SharedEnv, left: Val, right: Val, op_name: &'static str, op: fn(Quant, Quant) -> EvalResult) -> EvalResult {
	match (left, right) {
		// Quantity op Quantity
		(Val::Quant(left), Val::Quant(right)) => op(left, right),
		// List op Quantity
		(Val::List(list), Val::Quant(quant)) => {
			Ok(Val::List(list.map(|elem| eval_bin_op(env, elem.clone(), Val::Quant(quant.clone()), op_name, op))?))
		},
		// Quantity op List
		(Val::Quant(quant), Val::List(list)) => {
			Ok(Val::List(list.map(|elem| eval_bin_op(env, Val::Quant(quant.clone()), elem.clone(), op_name, op))?))
		},
		// Invalid numeric operation
		(left @ _, right @ _) => Err(RtErr::BinaryTypeMismatch {
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
fn get_idx_from_list(env: &SharedEnv, list: List, n: usize) -> Result<usize, RtErr> {
	// Check for exactly one element that fits in an i64.
	if let (Some(value), Some(true)) = (list.head(), list.tail().as_ref().map(List::is_empty)) {
		if let Some(signed_idx) = value.as_i64() {
			let signed_len = n as i64;
			let signed_idx = ((signed_idx % signed_len) + signed_len) % signed_len;
			return Ok(signed_idx as usize);
		}
	}
	Err(RtErr::InvalidIndex { idx: list.format_with_env(&env) })
}

fn eval_evaluated_cluster(env: &mut SharedEnv, mut cluster: Vec<EvaluatedClusterItem>) -> EvalResult {
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
						Err(RtErr::OutOfBounds)
					} else {
						// Can safely unwrap because idx is guaranteed to be in bounds.
						Ok(left.nth(get_idx_from_list(&env, right.clone(), left.len())?).unwrap())
					}
				},
				// String index
				(Val::Prim(Prim::String(left)), Val::List(right)) => {
					if left.is_empty() {
						Err(RtErr::OutOfBounds)
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
			cluster[idx].value = eval_bin_op(env, left, right, "exponentiation", |a, b| {
				Ok(Val::from(a.pow(b).map_err(RtErr::quant)?))
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
	// Implicit multiplications
	for idx in 0..cluster.len() - 1 {
		match &cluster[idx + 1].connector {
			ClusterConnector::AdjParen | ClusterConnector::AdjNonparen => {
				let left = cluster[idx].value.clone();
				let right = cluster[idx + 1].value.clone();
				cluster[idx].value = eval_bin_op(env, left, right, "multiplication", |a, b| {
					Ok(Val::from((a * b).map_err(RtErr::quant)?))
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			},
			_ => ()
		}
	}
	// Explicit multiplications and divisions
	for idx in 0..cluster.len() - 1 {
		match &cluster[idx + 1].connector {
			ClusterConnector::Mul => {
				let left = cluster[idx].value.clone();
				let right = cluster[idx + 1].value.clone();
				cluster[idx].value = eval_bin_op(env, left, right, "multiplication", |a, b| {
					Ok(Val::from((a * b).map_err(RtErr::quant)?))
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			},
			ClusterConnector::Div => {
				let left = cluster[idx].value.clone();
				let right = cluster[idx + 1].value.clone();
				cluster[idx].value = eval_bin_op(env, left, right, "division", |a, b| {
					Ok(Val::from((a / b).map_err(RtErr::quant)?))
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			},
			_ => ()
		}
	}
	// Cluster has now been collapsed to a single value.
	Ok(cluster.remove(0).value)
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
		return Err(RtErr::ArgCountMismatch {
			required: c.f.params.len(),
			received: args.len(),
		});
	}
	// Assign arguments to parameters within a copy of the closure's environment.
	let mut local_env = Env::new(Some(c.env.clone()));
	for (variable, value) in c.f.params.into_iter().zip(args.into_iter()) {
		local_env.lock().unwrap().set_var(variable, value);
	}
	// Evaluate function body within the application environment.
	let result = match c.f.body {
		LambdaBody::UserDefined(body) => eval_expr(&mut local_env, *body),
		LambdaBody::Intrinsic(body) => {
			match body {
				Intrinsic::Pop => match local_env.lock().unwrap().get_var(&"list".into()).unwrap() {
					Val::List(list) => Ok(Val::List(list.tail().ok_or(RtErr::OutOfBounds)?)),
					arg @ _ => Err(RtErr::UnaryTypeMismatch {
						context: "pop()",
						expected: vec!(Type::List),
						actual: arg.get_type()
					}),
				},
			}
		}
	}?;
	// Unwrap returned value, if any.
	Ok(match result {
		Val::Return { result } => *result,
		other @ _ => other,
	})
}

fn eval_numeric_negation(env: &mut SharedEnv, value: Val) -> EvalResult {
	match value {
		Val::Quant(quant) => Ok(Val::Quant(-quant)),
		Val::List(list) => Ok(Val::List(list.map(|elem| eval_numeric_negation(env, elem.clone()))?)),
		_ => Err(RtErr::UnaryTypeMismatch {
			context: "negation",
			expected: vec!(
				Type::Quant(NumType::Integer),
				Type::Quant(NumType::Rational),
				Type::Quant(NumType::Real),
				Type::List,
			),
			actual: value.get_type(),
		}),
	}
}

#[cfg(test)]
mod tests {
	use crate::env::{Env, SharedEnv};
	use crate::errors::{GynjoErr, RtErr};
	use crate::format_with_env::FormatWithEnv;
	use crate::interpreter::eval;
	use crate::list::List;
	use crate::numbers::Num;
	use crate::primitives::Prim;
	use crate::quantity::QuantErr;
	use crate::tuple::Tuple;
	use crate::types::{Type, NumType};
	use crate::values::Val;

	use bigdecimal::BigDecimal;

	use std::str::FromStr;

	#[test]
	fn empty_input_evaluates_to_empty() -> Result<(), GynjoErr> {
		assert_eq!(Val::empty(), eval(&mut Env::new(None), "")?);
		Ok(())
	}

	mod logical_operators {
		use super::*;
		#[test]
		fn and() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false and false")?);
			assert_eq!(Val::from(false), eval(&mut env, "false and true")?);
			assert_eq!(Val::from(false), eval(&mut env, "true and false")?);
			assert_eq!(Val::from(true), eval(&mut env, "true and true")?);
			Ok(())
		}
		#[test]
		fn or() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false or false")?);
			assert_eq!(Val::from(true), eval(&mut env, "false or true")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or false")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or true")?);
			Ok(())
		}
		#[test]
		fn not() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "not true")?);
			assert_eq!(Val::from(true), eval(&mut env, "not false")?);
			assert_eq!(Val::from(true), eval(&mut env, "not false and false")?);
			Ok(())
		}
		#[test]
		fn short_circuiting() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "false and 1/0")?);
			assert_eq!(Val::from(true), eval(&mut env, "true or 1/0")?);
			Ok(())
		}
		#[test]
		fn and_precedes_or() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "true or true and false")?);
			assert_eq!(Val::from(true), eval(&mut env, "false and true or true")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), GynjoErr> {
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
		fn eq() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 2")?);
			Ok(())
		}
		#[test]
		fn neq() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 != 2")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 != 1")?);
			Ok(())
		}
		#[test]
		fn approx() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "true ~ true")?);
			assert_eq!(Val::from(true), eval(&mut env, "(1/3 as real) ~ 0.333333333333")?);
			assert_eq!(Val::from(true), eval(&mut env, "1/3 ~ 0.333333333333")?);
			assert_eq!(Val::from(false), eval(&mut env, "(1/3 as real) ~ 0.333")?);
			Ok(())
		}
		#[test]
		fn lt() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 < 2.0")?);
			assert_eq!(Val::from(false), eval(&mut env, "1.0 < 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "2 < 1")?);
			Ok(())
		}
		#[test]
		fn leq() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 <= 2.0")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.0 <= 1")?);
			assert_eq!(Val::from(false), eval(&mut env, "2 <= 1")?);
			Ok(())
		}
		#[test]
		fn gt() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "1 > 2.0")?);
			assert_eq!(Val::from(false), eval(&mut env, "1.0 > 1")?);
			assert_eq!(Val::from(true), eval(&mut env, "2 > 1")?);
			Ok(())
		}
		#[test]
		fn geq() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "1 >= 2.0")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.0 >= 1")?);
			assert_eq!(Val::from(true), eval(&mut env, "2 >= 1")?);
			Ok(())
		}
		#[test]
		fn comparisons_and_logical_operators() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 1 and 2 = 2")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 1 and 2 = 3")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 2 or 3 = 3")?);
			assert_eq!(Val::from(false), eval(&mut env, "1 = 2 or 3 = 4")?);
			Ok(())
		}
		#[test]
		fn non_numbers_are_equal_checkable_but_not_comparable() -> Result<(), GynjoErr> {
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
			match eval(&mut env, "false < true").err().unwrap() {
				GynjoErr::Rt(RtErr::BinaryTypeMismatch { .. }) => (),
				_ => assert!(false),
			}
			match eval(&mut env, "(1, 2, 3) < (3, 2, 1)").err().unwrap() {
				GynjoErr::Rt(RtErr::BinaryTypeMismatch { .. }) => (),
				_ => assert!(false),
			}
			Ok(())
		}
		#[test]
		fn different_types_compare_inequal() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(false), eval(&mut env, "[1, 2, 3] = (1, 2, 3)")?);
			assert_eq!(Val::from(true), eval(&mut env, "(x -> x) != false")?);
			Ok(())
		}
		#[test]
		fn different_types_cannot_be_order_compared() {
			match eval(&mut Env::new(None), "(x -> x) < false").err().unwrap() {
				GynjoErr::Rt(RtErr::BinaryTypeMismatch { .. }) => (),
				_ => assert!(false),
			}
		}
		#[test]
		fn comparison_precedes_equality() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from(true), eval(&mut env, "1 < 2 = 2 < 3")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 > 2 != 2 < 3")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), GynjoErr> {
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
	fn subtraction_and_negation() -> Result<(), GynjoErr> {
		assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "-1+-2*-2")?);
		Ok(())
	}

	#[test]
	fn simple_compound_expression_with_parentheses() -> Result<(), GynjoErr> {
		assert_eq!(Val::scalar(5), eval(&mut Env::new(None), "-5 * (1 + -2)")?);
		Ok(())
	}

	#[test]
	fn exponentiation() -> Result<(), GynjoErr> {
		assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "2^0")?);
		assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2^1")?);
		assert_eq!(Val::scalar(16), eval(&mut Env::new(None), "2^4")?);
		assert_eq!(Val::scalar(32), eval(&mut Env::new(None), "2^5")?);
		assert_eq!(Val::scalar(Num::rational(1, 4)), eval(&mut Env::new(None), "2^-2")?);
		Ok(())
	}

	#[test]
	fn rational_negation() -> Result<(), GynjoErr> {
		assert_eq!(Val::scalar(Num::rational(-1, 2)), eval(&mut Env::new(None), "-1/2")?);
		Ok(())
	}

	mod mixed_domain_math {
		use super::*;
		#[test]
		fn real_literals_decay_to_integer() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "3.0")?);
			assert_eq!(Val::scalar(4_000_000), eval(&mut Env::new(None), "4000000.0")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "1 + 2")?);
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "1.0 + 2")?);
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "1 + 2.0")?);
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "1.0 + 2.0")?);
			assert_eq!(Val::scalar(3.5), eval(&mut Env::new(None), "1 + 2.5")?);
			assert_eq!(Val::scalar(3.5), eval(&mut Env::new(None), "1.5 + 2")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "2 - 1")?);
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "2.0 - 1")?);
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "2 - 1.0")?);
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "2.0 - 1.0")?);
			assert_eq!(Val::scalar(1.5), eval(&mut Env::new(None), "2.5 - 1")?);
			assert_eq!(Val::scalar(0.5), eval(&mut Env::new(None), "2 - 1.5")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2 * 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2.0 * 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2 * 1.0")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2.0 * 1.0")?);
			assert_eq!(Val::scalar(2.5), eval(&mut Env::new(None), "2.5 * 1")?);
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "2 * 1.5")?);
			assert_eq!(Val::scalar(3.75), eval(&mut Env::new(None), "2.5 * 1.5")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2 / 1")?);
			assert_eq!(Val::scalar(Num::rational(1, 2)), eval(&mut Env::new(None), "1 / 2")?);
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "1 / 2 * 2")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2.0 / 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "3 / 1.5")?);
			assert_eq!(Val::scalar(2.5), eval(&mut Env::new(None), "2.5 / 1.0")?);
			assert_eq!(Val::scalar(2.5), eval(&mut Env::new(None), "2.5 / 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "3 / 1.5")?);
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "1.5 / 1.5")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2 ^ 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2.0 ^ 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2 ^ 1.0")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "2.0 ^ 1.0")?);
			assert_eq!(Val::scalar(2.5), eval(&mut Env::new(None), "2.5 ^ 1")?);
			assert_eq!(Val::scalar(2), eval(&mut Env::new(None), "4 ^ 0.5")?);
			Ok(())
		}
	}

	#[test]
	fn exponentiation_is_right_associative() -> Result<(), GynjoErr> {
		assert_eq!(Val::scalar(262144), eval(&mut Env::new(None), "4^3^2")?);
		Ok(())
	}

	#[test]
	fn basic_assignment() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, "let x = 42")?;
		assert_eq!(Val::scalar(42), eval(&mut env, "x")?);
		Ok(())
	}

	mod tuples {
		use super::*;
		#[test]
		fn singleton_collapses_into_contained_value() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "(1)")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers() -> Result<(), GynjoErr> {
			let expected = make_tuple_value!(Val::scalar(1), make_tuple_value!(Val::scalar(2), Val::scalar(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1, (2, 3))")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers_and_booleans() -> Result<(), GynjoErr> {
			let expected = make_tuple_value!(Val::from(true), make_tuple_value!(Val::scalar(2), Val::from(false)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1 < 2, (2, false))")?);
			Ok(())
		}
	}

	mod list_construction {
		use super::*;
		#[test]
		fn singleton_list() -> Result<(), GynjoErr> {
			assert_eq!(make_list_value!(Val::scalar(1)), eval(&mut Env::new(None), "[1]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers() -> Result<(), GynjoErr> {
			let expected = make_list_value!(Val::scalar(1), make_list_value!(Val::scalar(2), Val::scalar(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "[1, [2, 3]]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers_and_booleans() -> Result<(), GynjoErr> {
			let expected = make_list_value!(Val::from(true), make_list_value!(Val::scalar(2), Val::from(false)));
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
		fn negation() -> Result<(), GynjoErr> {
			let expected = make_list_value!(Val::scalar(-1), Val::scalar(-2), Val::scalar(-3));
			assert_eq!(expected, eval(&mut Env::new(None), "-[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::scalar(2), Val::scalar(3), Val::scalar(4));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3] + 1")?);
			assert_eq!(expected, eval(&mut env, "1 + [1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::scalar(1), Val::scalar(2), Val::scalar(3));
			assert_eq!(expected, eval(&mut env, "[2, 3, 4]-1")?);
			assert_eq!(expected, eval(&mut env, "4-[3, 2, 1]")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::scalar(2), Val::scalar(4), Val::scalar(6));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3]2")?);
			assert_eq!(expected, eval(&mut env, "2[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::scalar(1), Val::scalar(2), Val::scalar(3));
			assert_eq!(expected, eval(&mut env, "[2, 4, 6]/2")?);
			assert_eq!(expected, eval(&mut env, "6/[6, 3, 2]")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let expected = make_list_value!(Val::scalar(1), Val::scalar(4), Val::scalar(16));
			assert_eq!(expected, eval(&mut env, "[1, 2, 4]^2")?);
			assert_eq!(expected, eval(&mut env, "2^[0, 2, 4]")?);
			Ok(())
		}
	}

	mod concatenation {
		use super::*;
		#[test]
		fn string_concatenation() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("hello, 1".to_string()), eval(&mut env, r#""hello, " | 1"#)?);
			assert_eq!(Val::from("1 world".to_string()), eval(&mut env, r#"1 | " world""#)?);
			assert_eq!(Val::from("hello, world".to_string()), eval(&mut env, r#""hello, " | "world""#)?);
			Ok(())
		}
		#[test]
		fn list_concatenation() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(Val::scalar(1), Val::scalar(2)), eval(&mut env, r#"[1] | [2]"#)?);
			assert_eq!(Val::from("1[2]".to_string()), eval(&mut env, r#""1" | [2]"#)?);
			assert_eq!(Val::from("[1]2".to_string()), eval(&mut env, r#"[1] | "2""#)?);
			Ok(())
		}
	}

	mod indexing {
		use super::*;
		#[test]
		fn valid_list_indexing() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(1), eval(&mut env, "[1, 2][0]")?);
			assert_eq!(Val::scalar(2), eval(&mut env, "[1, 2][1]")?);
			assert_eq!(Val::scalar(1), eval(&mut env, "[1, 2][2]")?);
			assert_eq!(Val::scalar(2), eval(&mut env, "[1, 2][-1]")?);
			Ok(())
		}
		#[test]
		fn invalid_list_indexing() {
			let mut env = Env::new(None);
			assert_eq!(GynjoErr::Rt(RtErr::OutOfBounds), eval(&mut env, "[][0]").err().unwrap());
			match eval(&mut env, "[1][true]").err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => assert!(false),
			}
			match eval(&mut env, "[1][1, 2]").err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => assert!(false),
			}
		}
		#[test]
		fn valid_string_indexing() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("h".to_string()), eval(&mut env, r#""hi"[0]"#)?);
			assert_eq!(Val::from("i".to_string()), eval(&mut env, r#""hi"[1]"#)?);
			assert_eq!(Val::from("h".to_string()), eval(&mut env, r#""hi"[2]"#)?);
			assert_eq!(Val::from("i".to_string()), eval(&mut env, r#""hi"[-1]"#)?);
			Ok(())
		}
		#[test]
		fn invalid_string_indexing() {
			let mut env = Env::new(None);
			assert_eq!(GynjoErr::Rt(RtErr::OutOfBounds), eval(&mut env, r#"""[0]"#).err().unwrap());
			match eval(&mut env, r#""hi"[true]"#).err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => assert!(false),
			}
			match eval(&mut env, r#""hi"[1, 2]"#).err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => assert!(false),
			}
		}
	}

	#[test]
	fn simple_function_application() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, "let f = () -> 42")?;
		assert_eq!(Val::scalar(42), eval(&mut env, "f()")?);
		Ok(())
	}

	mod order_of_operations {
		use super::*;
		fn env_with_inc() -> Result<SharedEnv, GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, "let inc = a -> a + 1")?;
			Ok(env)
		}
		#[test]
		fn parenthesized_function_call_before_exponentiation() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(36), eval(&mut env_with_inc()?, "4inc(2)^2")?);
			Ok(())
		}
		#[test]
		fn exponentiation_before_non_parenthesized_function_call() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(20), eval(&mut env_with_inc()?, "4inc 2^2")?);
			Ok(())
		}
		#[test]
		fn exponentiation_before_negation() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(-9), eval(&mut Env::new(None), "-3^2")?);
			Ok(())
		}
	}

	#[test]
	fn higher_order_functions() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, "let apply = (f, a) -> f(a)")?;
		assert_eq!(Val::scalar(42), eval(&mut env, "apply(a -> a, 42)")?);
		Ok(())
	}

	#[test]
	fn curried_function() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, "let sum = a -> b -> a + b")?;
		assert_eq!(Val::scalar(3), eval(&mut env, "sum 1 2")?);
		Ok(())
	}

	#[test]
	fn environment_does_not_persist_between_function_chains() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let get_a = () -> a;
		}")?;
		// "a" should be undefined.
		match eval(&mut env, "sum (1) (2) get_a ()").err().unwrap() {
			GynjoErr::Rt(RtErr::Undefined(_)) => (),
			_ => assert!(false),
		}
		Ok(())
	}

	#[test]
	fn chained_application_with_and_without_parentheses() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let inc = a -> a + 1;
		}")?;
		assert_eq!(Val::scalar(3), eval(&mut env, "sum (1) 2")?);
		Ok(())
	}

	#[test]
	fn chained_application_does_not_pollute_applications_higher_in_the_call_chain() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(&mut env, r"{
			let sum = a -> b -> a + b;
			let inc = b -> b + 1;
		}")?;
		assert_eq!(Val::scalar(8), eval(&mut env, "sum (inc 5) 2")?);
		Ok(())
	}

	#[test]
	fn returning_from_nested_block() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		assert_eq!(Val::scalar(7), eval(&mut env, r"{
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
		fn empty_block() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::empty(), eval(&mut env, "{}")?);
			assert_eq!(Val::empty(), eval(&mut env, "{;;;}")?);
			Ok(())
		}
		#[test]
		fn compound_block() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), "{();();1}")?);
			Ok(())
		}
		#[test]
		fn unused_result() {
			let mut env = Env::new(None);
			match eval(&mut env, "{1;2}").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => assert!(false),
			}
			match eval(&mut env, "{1;}").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => assert!(false),
			}
		}
		#[test]
		fn nested_blocks() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, "{ let a = 0 }")?;
			eval(&mut env, "{ let b = { let a = a + 1; a } }")?;
			assert_eq!(Val::scalar(1), eval(&mut env, "b")?);
			Ok(())
		}
	}

	mod branch_statements {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, "if false then let a = 1/0 else let a = 1")?;
			assert_eq!(Val::scalar(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, "if true then let a = 1 else let a = 1/0")?;
			assert_eq!(Val::scalar(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn no_else_statement() -> Result<(), GynjoErr> {
			assert_eq!(Val::empty(), eval(&mut Env::new(None), "if false then let a = 1/0")?);
			Ok(())
		}
	}

	mod while_loops {
		use super::*;
		#[test]
		fn basic_while_loop() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, r"{
				let a = 0;
				while a < 3 do let a = a + 1;
			}")?;
			assert_eq!(Val::scalar(3), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn unused_result() {
			match eval(&mut Env::new(None), "while true do 1").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => assert!(false),
			}
		}
	}

	mod for_loops {
		use super::*;
		#[test]
		fn basic_for_loops() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(&mut env, r"{
				let a = 0;
				for x in [1, 2, 3] do let a = a + x;
				for x in [] do let a = 10;
			}")?;
			assert_eq!(Val::scalar(6), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn unused_result() {
			match eval(&mut Env::new(None), "for x in [1, 2, 3] do 1").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => assert!(false),
			}
		}
	}

	mod return_and_break {
		use super::*;
		#[test]
		fn return_outside_function_is_okay() -> Result<(), GynjoErr> {
			assert_eq!(Val::Return { result: Box::new(Val::scalar(1)) }, eval(&mut Env::new(None), "return 1")?);
			Ok(())
		}
		#[test]
		fn break_outside_loop_is_okay() -> Result<(), GynjoErr> {
			assert_eq!(Val::Break, eval(&mut Env::new(None), "break")?);
			Ok(())
		}
		#[test]
		fn return_is_idempotent() -> Result<(), GynjoErr> {
			assert_eq!(Val::Return { result: Box::new(Val::scalar(1)) }, eval(&mut Env::new(None), "return return 1")?);
			Ok(())
		}
		#[test]
		fn return_from_nested_block() -> Result<(), GynjoErr> {
			assert_eq!(Val::Return { result: Box::new(Val::scalar(1)) }, eval(&mut Env::new(None), r"{
				if true then {
					return 1
				};
				2
			}")?);
			Ok(())
		}
		#[test]
		fn return_from_called_function() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(1), eval(&mut Env::new(None), r"{
				let f = () -> return ();
				f();
				1
			}")?);
			Ok(())
		}
		#[test]
		fn break_while() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::empty(), eval(&mut env, r"
				while true do {
					break;
					1/0
				}
			")?);
			Ok(())
		}
		#[test]
		fn return_from_while() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(7), eval(&mut env, r"(() -> {
				while true do {
					return 7;
					1/0
				}
			})()")?);
			Ok(())
		}
		#[test]
		fn break_for() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(1), eval(&mut env, r"{
				for x in [1, 2, 3] do {
					break;
					1/0
				};
				x
			}")?);
			Ok(())
		}
		#[test]
		fn return_from_for() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(7), eval(&mut env, r"(() -> {
				for x in [1, 2, 3] do {
					return 7;
					1/0
				};
			})()")?);
			Ok(())
		}
	}

	mod intrinsics {
		use super::*;
		#[test]
		fn pop() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(), eval(&mut env, "pop([1])")?);
			assert_eq!(GynjoErr::Rt(RtErr::OutOfBounds), eval(&mut env, "pop([])").err().unwrap());
			Ok(())
		}
		#[test]
		fn get_type() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::Prim(Prim::Type(Type::Quant(NumType::Integer))), eval(&mut env, "get_type 1")?);
			assert_eq!(Val::Prim(Prim::Type(Type::Quant(NumType::Real))), eval(&mut env, "get_type 1.5")?);
			assert_eq!(Val::Prim(Prim::Type(Type::List)), eval(&mut env, "get_type []")?);
			assert_eq!(Val::Prim(Prim::Type(Type::List)), eval(&mut env, "get_type [1]")?);
			assert_eq!(Val::Prim(Prim::Type(Type::Type)), eval(&mut env, "get_type get_type 1")?);
			Ok(())
		}
	}

	mod type_conversions {
		use super::*;
		#[test]
		fn type_to_itself() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(1), eval(&mut env, "1 as integer")?);
			assert_eq!(Val::from("hello".to_string()), eval(&mut env, r#""hello" as string"#)?);
			assert_eq!(Val::from(true), eval(&mut env, "(x -> x) = (x -> x) as closure")?);
			Ok(())
		}
		#[test]
		fn domain_expansion() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(Num::rational(1, 1)), eval(&mut env, "1 as rational")?);
			assert_eq!(Val::scalar(1.0), eval(&mut env, "1 as real")?);
			assert_eq!(Val::scalar(1.0), eval(&mut env, "1 as rational as real")?);
			Ok(())
		}
		#[test]
		fn tuple_list_conversion() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(Val::scalar(1), Val::scalar(2)), eval(&mut env, "(1, 2) as list")?);
			assert_eq!(make_tuple_value!(Val::scalar(1), Val::scalar(2)), eval(&mut env, "[1, 2] as tuple")?);
			Ok(())
		}
		#[test]
		fn conversion_to_string() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::from("true".to_string()), eval(&mut env, "true as string")?);
			assert_eq!(Val::from("1".to_string()), eval(&mut env, "1 as string")?);
			assert_eq!(Val::from("(1, 2)".to_string()), eval(&mut env, "(1, 2) as string")?);
			Ok(())
		}
		#[test]
		fn invalid_conversions() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let real_to_integer = GynjoErr::Rt(RtErr::InvalidTypeCast { from: Type::Quant(NumType::Real), to: Type::Quant(NumType::Integer) });
			let string_to_real = GynjoErr::Rt(RtErr::InvalidTypeCast { from: Type::String, to: Type::Quant(NumType::Real) });
			let closure_to_boolean = GynjoErr::Rt(RtErr::InvalidTypeCast { from: Type::Closure, to: Type::Boolean });
			assert_eq!(real_to_integer, eval(&mut env, "1.5 as integer").err().unwrap());
			assert_eq!(string_to_real, eval(&mut env, r#""five" as real"#).err().unwrap());
			assert_eq!(closure_to_boolean, eval(&mut env, "(x -> x) as boolean").err().unwrap());
			Ok(())
		}
	}

	mod units {
		use super::*;
		#[test]
		fn canceling() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(5), eval(&mut Env::with_core_libs(), "5.m/1.m")?);
			Ok(())
		}
		#[test]
		fn single_unit_addition_and_subtraction() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(eval(&mut env, "3.m")?, eval(&mut Env::with_core_libs(), "1.m + 2.m")?);
			assert_eq!(eval(&mut env, "1.m")?, eval(&mut env, "2.m - 1.m")?);
			Ok(())
		}
		#[test]
		fn invalid_addition_and_subtraction() {
			let mut env = Env::with_core_libs();
			assert_eq!(GynjoErr::Rt(RtErr::Quant(QuantErr::UnitErr)), eval(&mut env, "1.m + 1.s").err().unwrap());
			assert_eq!(GynjoErr::Rt(RtErr::Quant(QuantErr::UnitErr)), eval(&mut env, "1.m - 1.s").err().unwrap());
		}
		#[test]
		fn multiplication() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(eval(&mut env, "6.m.s")?, eval(&mut env, "2.m 3.s")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(eval(&mut env, "2.m/.s")?, eval(&mut env, "4.m/2.s")?);
			Ok(())
		}
		#[test]
		fn mixed_unit_arithmetic() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(eval(&mut env, "2.m")?, eval(&mut env, "1.m + 100.cm")?);
			assert_eq!(eval(&mut env, "1.m")?, eval(&mut env, "2.m - 100.cm")?);
			assert_eq!(eval(&mut env, "1")?, eval(&mut env, "1.m / 100.cm")?);
			assert_eq!(eval(&mut env, "1.m^2")?, eval(&mut env, "1.m * 100.cm")?);
			Ok(())
		}
		#[test]
		fn mixed_unit_comparisons() -> Result<(), GynjoErr> {
			assert_eq!(Val::from(true), eval(&mut Env::with_core_libs(), "1.m = 100.cm")?);
			Ok(())
		}
		#[test]
		fn canonical_display_order() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("1.kg.m.s", eval(&mut env, ".s.m.kg")?.format_with_env(&env));
			assert_eq!("1.s^2.m.kg^-1", eval(&mut env, "(1/.kg).s.m.s")?.format_with_env(&env));
			Ok(())
		}
	}

	#[test]
	fn core_constants() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		let pi_str = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
		let expected = Val::scalar(BigDecimal::from_str(pi_str).unwrap());
		eval(&mut env, "import \"core/constants.gynj\"")?;
		assert_eq!(expected, eval(&mut env, "PI")?);
		Ok(())
	}

	mod core_libs {
		use super::*;
		mod basic_math {
			use super::*;
			#[test]
			fn absolute_value() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::scalar(5), eval(&mut env, "abs 5")?);
				assert_eq!(Val::scalar(5), eval(&mut env, "abs(-5)")?);
				Ok(())
			}
		}
		mod combinatorics {
			use super::*;
			#[test]
			fn factorial() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::scalar(120), eval(&mut env, "fact 5")?);
				Ok(())
			}
			#[test]
			fn permutations() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::scalar(60), eval(&mut env, "nPk(5, 3)")?);
				Ok(())
			}
			#[test]
			fn combinations() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				// Using an epsilon here because of the division.
				assert_eq!(Val::from(true), eval(&mut env, "abs(nCk(5, 3) - 10) < 10**-50")?);
				Ok(())
			}
		}
		mod list_ops {
			use super::*;
			#[test]
			fn len() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::scalar(0), eval(&mut env, "len []")?);
				assert_eq!(Val::scalar(3), eval(&mut env, "len [1, 2, 3]")?);
				Ok(())
			}
			#[test]
			fn nth() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(GynjoErr::Rt(RtErr::OutOfBounds),  eval(&mut env, "nth([], 0)").err().unwrap());
				assert_eq!(Val::scalar(2), eval(&mut env, "nth([1, 2, 3], 1)")?);
				Ok(())
			}
			#[test]
			fn reverse() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(3), Val::scalar(2), Val::scalar(1)), eval(&mut env, "reverse [1, 2, 3]")?);
				Ok(())
			}
			#[test]
			fn insert_into_non_empty() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				let expected = make_list_value!(Val::scalar(1), Val::scalar(2), Val::scalar(3));
				assert_eq!(expected, eval(&mut env, "insert([1, 3], 1, 2)")?);
				Ok(())
			}
			#[test]
			fn insert_into_empty() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(1)), eval(&mut env, "insert([], 0, 1)")?);
				Ok(())
			}
			#[test]
			fn remove_from_middle() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(1), Val::scalar(3)), eval(&mut env, "remove([1, 2, 3], 1)")?);
				Ok(())
			}
			#[test]
			fn remove_from_beginning() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(2), Val::scalar(3)), eval(&mut env, "remove([1, 2, 3], 0)")?);
				Ok(())
			}
			#[test]
			fn map() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(1), Val::scalar(4), Val::scalar(9)), eval(&mut env, "map([1, 2, 3], x -> x^2)")?);
				Ok(())
			}
			#[test]
			fn reduce() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(Val::scalar(6), eval(&mut env, "reduce([1, 2, 3], 0, (a, b) -> a + b)")?);
				Ok(())
			}
			#[test]
			fn flatmap() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				let expected = make_list_value!(Val::scalar(1), Val::scalar(1), Val::scalar(2), Val::scalar(2));
				assert_eq!(expected, eval(&mut env, "flatmap([1, 2], x -> [x, x])")?);
				Ok(())
			}
			#[test]
			fn range() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(make_list_value!(Val::scalar(1), Val::scalar(2), Val::scalar(3)), eval(&mut env, "range(1, 3)")?);
				Ok(())
			}
		}
	}	
}
