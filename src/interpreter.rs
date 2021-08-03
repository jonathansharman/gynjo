use crate::env::{Env, SharedEnv};
use crate::errors::{GynjoErr, RtErr};
use crate::expressions::{BinExpr, BinOp, Cluster, ClusterConnector, Expr, LambdaBody};
use crate::format_with_env::FormatWithEnv;
use crate::intrinsics::Intrinsic;
use crate::lexer::lex;
use crate::parser::parse;
use crate::primitives::{Bool, Num, NumType, Prim, Type};
use crate::symbol::Sym;
use crate::values::{Closure, List, Quant, Range, Tuple, Unit, Val};

use std::collections::VecDeque;
use std::io;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Val, RtErr>;

/// If possible, computes the value of `expr` in the context of `env`.
pub fn eval_expr(mut env: &mut SharedEnv, expr: Expr) -> EvalResult {
	match expr {
		Expr::Block(exprs) => eval_block(&mut env, exprs),
		Expr::Bin(bin_expr) => eval_bin_expr(&mut env, bin_expr),
		Expr::Not(expr) => eval_not(&mut env, *expr),
		Expr::Cluster(cluster) => eval_cluster(env, cluster),
		Expr::Lambda(f) => Ok(Val::Closure(Closure {
			f,
			env: env.clone(),
		})),
		Expr::Tuple(expr_elems) => eval_tuple_expr(&mut env, expr_elems),
		Expr::List(expr_elems) => eval_list_expr(&mut env, *expr_elems),
		Expr::Range(exprs) => eval_range_expr(&mut env, (*exprs).0, (*exprs).1, (*exprs).2),
		Expr::Sym(symbol) => eval_symbol(&mut env, symbol),
		Expr::Prim(primitive) => Ok(match primitive {
			// Convert number into dimensionless quantity.
			Prim::Num(number) => Val::scalar(number.shrink_domain()),
			primitive => Val::Prim(primitive),
		}),
		Expr::Unit(unit) => eval_unit(&mut env, unit),
		Expr::DeclUnit {
			unit_name,
			value_expr,
		} => eval_unit_declaration(&mut env, unit_name, *value_expr),
		Expr::Basic(expr) => eval_basic(&mut env, *expr),
		Expr::Import(target) => eval_import(&mut env, *target),
		Expr::Assign { lhs, rhs } => eval_assignment(&mut env, lhs, *rhs),
		Expr::Branch {
			test,
			then_expr,
			else_expr,
		} => eval_branch(&mut env, *test, *then_expr, *else_expr),
		Expr::WhileLoop { test, body } => eval_while_loop(env, *test, *body),
		Expr::ForLoop {
			loop_var,
			range,
			body,
		} => eval_for_loop(env, loop_var, *range, body),
		Expr::Break => Ok(Val::Break),
		Expr::Return(result) => Ok(match eval_expr(env, *result)? {
			// Return is idempotent; don't wrap the return value if it's already wrapped.
			result @ Val::Return { .. } => result,
			// Wrap non-returned value into returned value.
			result => Val::Return {
				result: Box::new(result),
			},
		}),
		Expr::Read => {
			let mut input = String::new();
			io::stdin().read_line(&mut input).unwrap();
			Ok(Val::from(input.trim().to_string()))
		}
		Expr::Write(output) => {
			println!("{}", eval_expr(&mut env, *output)?.format_with_env(env));
			Ok(Val::empty())
		}
		Expr::GetType(expr) => Ok(Val::Prim(Prim::Type(
			eval_expr(&mut env, *expr)?.get_type(),
		))),
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

fn eval_block(env: &mut SharedEnv, mut exprs: Vec<Expr>) -> EvalResult {
	let last = exprs.pop();
	if let Some(last) = last {
		for expr in exprs.into_iter() {
			match eval_expr(env, expr)? {
				Val::Tuple(Tuple { elems }) if elems.is_empty() => {
					// Non-final empty results are okay.
				}
				Val::Break => {
					// Break out of block.
					return Ok(Val::Break);
				}
				result @ Val::Return { .. } => {
					// Return non-final result.
					return Ok(result);
				}
				unused => {
					// Non-final, non-returned, non-empty results are errors.
					return Err(RtErr::UnusedResult(unused.format_with_env(env)));
				}
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
		BinOp::In => eval_in(env, bin_expr),
		BinOp::And => eval_and(env, bin_expr),
		BinOp::Or => eval_or(env, bin_expr),
		BinOp::Eq => Ok(Bool::from(
			eval_expr(env, *bin_expr.left)? == eval_expr(env, *bin_expr.right)?,
		)
		.into()),
		BinOp::Neq => Ok(Bool::from(
			eval_expr(env, *bin_expr.left)? != eval_expr(env, *bin_expr.right)?,
		)
		.into()),
		BinOp::Approx => eval_approx(env, bin_expr),
		BinOp::Lt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a < b)))
		}
		BinOp::Leq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a <= b)))
		}
		BinOp::Gt => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a > b)))
		}
		BinOp::Geq => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "comparison", |a, b| Ok(Val::from(a >= b)))
		}
		BinOp::Add => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "addition", |a, b| {
				Ok(Val::from((a + b).map_err(RtErr::quant)?))
			})
		}
		BinOp::Sub => {
			let left = eval_expr(env, *bin_expr.left)?;
			let right = eval_expr(env, *bin_expr.right)?;
			eval_bin_op(env, left, right, "subtraction", |a, b| {
				Ok(Val::from((a - b).map_err(RtErr::quant)?))
			})
		}
		BinOp::Concat => eval_concat(env, bin_expr),
	}
}

fn eval_as(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.right)? {
		Val::Prim(Prim::Type(to)) => {
			match (eval_expr(env, *bin_expr.left)?, to) {
				// T -> T
				(value, to) if value.get_type() == to => Ok(value),
				// integer | rational -> rational
				(Val::Quant(quant), Type::Quant(NumType::Rational)) => {
					let (value, units) = quant.into_value_and_units();
					if let Num::Real(_) = value {
						Err(RtErr::InvalidTypeCast {
							from: Type::Quant(NumType::Real),
							to: Type::Quant(NumType::Rational),
						})
					} else {
						Ok(Quant::new(Num::Rational(value.into()), units).into())
					}
				}
				// integer | rational | real -> real
				(Val::Quant(quant), Type::Quant(NumType::Real)) => {
					let (value, units) = quant.into_value_and_units();
					Ok(Quant::new(Num::Real(value.into()), units).into())
				}
				// tuple -> list
				(Val::Tuple(tuple), Type::List) => {
					let mut list = List::empty();
					for value in tuple.elems.iter().rev() {
						list = list.push(value.clone());
					}
					Ok(Val::List(list))
				}
				// list -> tuple
				(Val::List(list), Type::Tuple) => {
					let mut elems = Vec::with_capacity(list.len());
					for value in list.iter() {
						elems.push(value.clone());
					}
					Ok(Val::Tuple(Tuple { elems }))
				}
				// range -> list
				(Val::Range(range), Type::List) => {
					// Ensure there's a start and end and infer the stride if it's missing.
					let range = match (&range.start, &range.end, &range.stride) {
						// Step from start to end by 1.
						(Some(start), Some(end), None) => Range {
							start: range.start.clone(),
							end: range.end.clone(),
							stride: Some(Quant::new(
								if start <= end { 1 } else { -1 }.into(),
								start.units().clone(),
							)),
						},
						// Step from start to end by stride.
						(Some(_), Some(_), Some(_)) => Range {
							start: range.start.clone(),
							end: range.end.clone(),
							stride: range.stride.clone(),
						},
						// If there's no start or no end, the range is considered out of bounds.
						_ => {
							return Err(RtErr::UnboundedRange {
								context: "range to list conversion",
							})
						}
					};
					let mut list = List::empty();
					for val in range.into_iter() {
						list = list.push(Val::Quant(val.map_err(RtErr::quant)?));
					}
					Ok(Val::List(list.reverse()))
				}
				// range -> tuple
				(Val::Range(range), Type::Tuple) => {
					// Ensure there's a start and end and infer the stride if it's missing.
					let range = match (&range.start, &range.end, &range.stride) {
						// Step from start to end by 1.
						(Some(start), Some(end), None) => Range {
							start: range.start.clone(),
							end: range.end.clone(),
							stride: Some(Quant::new(
								if start <= end { 1 } else { -1 }.into(),
								start.units().clone(),
							)),
						},
						// Step from start to end by stride.
						(Some(_), Some(_), Some(_)) => Range {
							start: range.start.clone(),
							end: range.end.clone(),
							stride: range.stride.clone(),
						},
						// If there's no start or no end, the range is considered out of bounds.
						_ => {
							return Err(RtErr::UnboundedRange {
								context: "range to tuple conversion",
							})
						}
					};
					let mut elems = Vec::new();
					for val in range.into_iter() {
						elems.push(Val::Quant(val.map_err(RtErr::quant)?));
					}
					Ok(Val::Tuple(Tuple { elems }))
				}
				// T -> string
				(value, Type::Text) => Ok(Val::Prim(Prim::Text(value.format_with_env(env).into()))),
				// Invalid conversion
				(value, to) => Err(RtErr::InvalidTypeCast {
					from: value.get_type(),
					to,
				}),
			}
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "type cast",
			expected: vec![Type::Type],
			actual: invalid.get_type(),
		}),
	}
}

fn eval_in(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.right)? {
		Val::Quant(to) => {
			let (to_value, to_units) = to.into_value_and_units();
			if to_value != Num::from(1) {
				return Err(RtErr::InvalidUnit);
			}
			match eval_expr(env, *bin_expr.left)? {
				Val::Quant(from) => Ok(from.convert_into(to_units).map_err(RtErr::quant)?.into()),
				invalid => Err(RtErr::UnaryTypeMismatch {
					context: "unit conversion",
					expected: Type::quant_types(),
					actual: invalid.get_type(),
				}),
			}
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "unit conversion",
			expected: Type::quant_types(),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_and(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.left)? {
		Val::Prim(Prim::Bool(left)) => {
			if left.into() {
				match eval_expr(env, *bin_expr.right)? {
					Val::Prim(Prim::Bool(right)) => Ok(right.into()),
					invalid => Err(RtErr::UnaryTypeMismatch {
						context: "logical conjunction",
						expected: vec![Type::Boolean],
						actual: invalid.get_type(),
					}),
				}
			} else {
				// Short-circuit to false.
				Ok(Bool::False.into())
			}
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "logical conjunction",
			expected: vec![Type::Boolean],
			actual: invalid.get_type(),
		}),
	}
}

fn eval_or(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	match eval_expr(env, *bin_expr.left)? {
		Val::Prim(Prim::Bool(left)) => {
			if left.into() {
				// Short-circuit to true.
				Ok(Bool::True.into())
			} else {
				match eval_expr(env, *bin_expr.right)? {
					Val::Prim(Prim::Bool(right)) => Ok(right.into()),
					invalid => Err(RtErr::UnaryTypeMismatch {
						context: "logical disjunction",
						expected: vec![Type::Boolean],
						actual: invalid.get_type(),
					}),
				}
			}
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "logical disjunction",
			expected: vec![Type::Boolean],
			actual: invalid.get_type(),
		}),
	}
}

fn eval_approx(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	let left = eval_expr(env, *bin_expr.left)?;
	let right = eval_expr(env, *bin_expr.right)?;
	Ok(Val::from(match (left, right) {
		(Val::Quant(left), Val::Quant(right)) => {
			let (left_value, left_units) = left.into_value_and_units();
			let (right_value, right_units) = right.into_value_and_units();
			let left_string =
				Quant::new(left_value.expand_domain(), left_units).format_with_env(env);
			let right_string =
				Quant::new(right_value.expand_domain(), right_units).format_with_env(env);
			left_string == right_string
		}
		(left, right) => left == right,
	}))
}

fn eval_concat(env: &mut SharedEnv, bin_expr: BinExpr) -> EvalResult {
	let left = eval_expr(env, *bin_expr.left)?;
	let right = eval_expr(env, *bin_expr.right)?;
	match (left, right) {
		// String concatenation
		(Val::Prim(Prim::Text(left)), Val::Prim(Prim::Text(right))) => {
			Ok(Val::from(format!("{}{}", left, right)))
		}
		(Val::Prim(Prim::Text(left)), right) => {
			Ok(Val::from(format!("{}{}", left, right.format_with_env(env))))
		}
		(left, Val::Prim(Prim::Text(right))) => {
			Ok(Val::from(format!("{}{}", left.format_with_env(env), right)))
		}
		// List concatenation
		(Val::List(left), Val::List(right)) => Ok(Val::List(left.concat(right))),
		// Invalid concatenation
		(left, right) => Err(RtErr::BinaryTypeMismatch {
			context: "concatenation",
			left: left.get_type(),
			right: right.get_type(),
		}),
	}
}

fn eval_not(env: &mut SharedEnv, expr: Expr) -> EvalResult {
	match eval_expr(env, expr)? {
		Val::Prim(Prim::Bool(b)) => Ok(Bool::from(!bool::from(b)).into()),
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "logical negation",
			expected: vec![Type::Boolean],
			actual: invalid.get_type(),
		}),
	}
}

fn eval_tuple_expr(env: &mut SharedEnv, expr_elems: Vec<Expr>) -> EvalResult {
	let mut elems = Vec::with_capacity(expr_elems.len());
	for value in expr_elems.into_iter().map(|elem| eval_expr(env, elem)) {
		elems.push(value?);
	}
	Ok(Val::Tuple(Tuple { elems }))
}

fn eval_list_expr(env: &mut SharedEnv, expr_elems: VecDeque<Expr>) -> EvalResult {
	let mut list = List::empty();
	for expr_elem in expr_elems.into_iter() {
		list = list.push(eval_expr(env, expr_elem)?);
	}
	Ok(Val::List(list))
}

fn eval_range_expr(
	env: &mut SharedEnv,
	start_expr: Option<Expr>,
	end_expr: Option<Expr>,
	stride_expr: Option<Expr>,
) -> EvalResult {
	let mut get_range_component_quantity = |component_expr: Option<Expr>, context| {
		let component_val = match component_expr {
			Some(expr) => Some(eval_expr(env, expr)?),
			None => None,
		};
		let component = match component_val {
			Some(Val::Quant(quant)) => Some(quant),
			Some(invalid) => {
				return Err(RtErr::UnaryTypeMismatch {
					context,
					expected: Type::quant_types(),
					actual: invalid.get_type(),
				})
			}
			None => None,
		};
		Ok(component)
	};
	let start = get_range_component_quantity(start_expr, "range expression start")?;
	let end = get_range_component_quantity(end_expr, "range expression end")?;
	let stride = get_range_component_quantity(stride_expr, "range expression stride")?;
	Ok(Val::Range(Box::new(Range { start, end, stride })))
}

fn eval_symbol(env: &mut SharedEnv, symbol: Sym) -> EvalResult {
	env.lock()
		.unwrap()
		.get_var(&symbol)
		.ok_or(RtErr::Undefined(symbol.name))
}

fn eval_unit(env: &mut SharedEnv, unit_name: String) -> EvalResult {
	let units = match env.lock().unwrap().get_unit(&unit_name) {
		Some(base) => Unit::non_base(unit_name, base).into(),
		None => Unit::base(unit_name).into(),
	};
	Ok(Val::from(Quant::new(Num::from(1), units)))
}

fn eval_unit_declaration(env: &mut SharedEnv, unit_name: String, value: Expr) -> EvalResult {
	// Evaluate the value expression.
	match eval_expr(env, value)? {
		Val::Quant(value) => {
			// Perform the unit assignment, possibly overwriting the existing value.
			env.lock()
				.unwrap()
				.set_unit(unit_name, value.convert_into_base().map_err(RtErr::quant)?);
			Ok(Val::empty())
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "unit declaration",
			expected: Type::quant_types(),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_basic(env: &mut SharedEnv, expr: Expr) -> EvalResult {
	match eval_expr(env, expr)? {
		Val::Quant(value) => Ok(value.convert_into_base().map_err(RtErr::quant)?.into()),
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "base units conversion",
			expected: Type::quant_types(),
			actual: invalid.get_type(),
		}),
	}
}

fn eval_import(env: &mut SharedEnv, target: Expr) -> EvalResult {
	match eval_expr(env, target)? {
		Val::Prim(Prim::Text(filename)) => {
			let filename: String = filename.into();
			let lib_text =
				std::fs::read_to_string(&filename).map_err(|err| RtErr::CouldNotOpenFile {
					filename: filename.clone(),
					file_error: err.to_string(),
				})?;
			eval(env, &lib_text).map_err(|err| RtErr::LibErr {
				lib_name: filename,
				nested_error: Box::new(err),
			})
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "import",
			expected: vec![Type::Text],
			actual: invalid.get_type(),
		}),
	}
}

fn eval_assignment(env: &mut SharedEnv, lhs: Sym, rhs: Expr) -> EvalResult {
	let rhs_value = eval_expr(env, rhs)?;
	// Perform the variable assignment, possibly overwriting the existing value.
	env.lock().unwrap().set_var(lhs, rhs_value);
	Ok(Val::empty())
}

fn eval_branch(env: &mut SharedEnv, test: Expr, then_expr: Expr, else_expr: Expr) -> EvalResult {
	let test_value = eval_expr(env, test)?;
	match test_value {
		Val::Prim(Prim::Bool(b)) => eval_expr(env, if b.into() { then_expr } else { else_expr }),
		_ => Err(RtErr::UnaryTypeMismatch {
			context: "conditional test",
			expected: vec![Type::Boolean],
			actual: test_value.get_type(),
		}),
	}
}

fn eval_while_loop(env: &mut SharedEnv, test: Expr, body: Expr) -> EvalResult {
	loop {
		// Evaluate the test condition.
		let test_value = eval_expr(env, test.clone())?;
		match test_value {
			Val::Prim(Prim::Bool(b)) => {
				if b.into() {
					// Evaluate next iteration.
					match eval_expr(env, body.clone())? {
						Val::Tuple(Tuple { elems }) if elems.is_empty() => {
							// Non-final empty results are okay.
						}
						Val::Break => {
							// Break out of loop.
							return Ok(Val::empty());
						}
						result @ Val::Return { .. } => {
							// Exit loop via return.
							return Ok(result);
						}
						unused => {
							// Non-final, non-returned, non-empty results are errors.
							return Err(RtErr::UnusedResult(unused.format_with_env(env)));
						}
					}
				} else {
					// End of loop.
					return Ok(Val::empty());
				}
			}
			_ => print!(
				"while-loop test value must be boolean, found {}",
				test_value.format_with_env(env)
			),
		}
	}
}

fn eval_for_loop(env: &mut SharedEnv, loop_var: Sym, range: Expr, body: Box<Expr>) -> EvalResult {
	// Common loop body logic. The return value is
	// (1) An error if body evaluation encountered an error,
	// (2) `None` if the loop should continue, or
	// (3) The loop result value if the loop hit a break/return.
	let eval_loop_body = |env: &mut SharedEnv| -> Result<Option<EvalResult>, RtErr> {
		match eval_expr(env, (*body).clone())? {
			// Non-final empty result. Continue the loop.
			Val::Tuple(Tuple { elems }) if elems.is_empty() => Ok(None),
			// Break expression. Break out of loop.
			Val::Break => Ok(Some(Ok(Val::empty()))),
			// Return expression. Exit loop via return.
			result @ Val::Return { .. } => Ok(Some(Ok(result))),
			// Error: non-final, non-returned, non-empty result.
			unused => Ok(Some(Err(RtErr::UnusedResult(unused.format_with_env(env))))),
		}
	};
	match eval_expr(env, range)? {
		Val::List(range_list) => {
			for value in range_list.iter() {
				// Assign the loop variable to the current value in the range list.
				env.lock().unwrap().set_var(loop_var.clone(), value.clone());
				// Evaluate the loop body in this context.
				if let Some(result) = eval_loop_body(env)? {
					return result;
				}
			}
			Ok(Val::empty())
		}
		Val::Range(range) => {
			// Ensure there's a start and infer the stride if it's missing.
			let range = match (&range.start, &range.end, &range.stride) {
				// Step from start by 1 forever.
				(Some(start), None, None) => Range {
					start: range.start.clone(),
					end: None,
					stride: Some(Quant::new(1.into(), start.units().clone())),
				},
				// Step from start to end by 1.
				(Some(start), Some(end), None) => Range {
					start: range.start.clone(),
					end: range.end.clone(),
					stride: Some(Quant::new(
						if start <= end { 1 } else { -1 }.into(),
						start.units().clone(),
					)),
				},
				// Step from start by stride forever.
				(Some(_), None, Some(_)) => Range {
					start: range.start.clone(),
					end: None,
					stride: range.stride.clone(),
				},
				// Step from start to end by stride.
				(Some(_), Some(_), Some(_)) => Range {
					start: range.start.clone(),
					end: range.end.clone(),
					stride: range.stride.clone(),
				},
				// If there's no start, the range is considered out of bounds.
				_ => {
					return Err(RtErr::UnboundedRange {
						context: "for-loop range",
					})
				}
			};
			for value in range.into_iter() {
				// Assign the loop variable to the current value in the range.
				env.lock()
					.unwrap()
					.set_var(loop_var.clone(), Val::Quant(value.map_err(RtErr::quant)?));
				// Evaluate the loop body in this context.
				if let Some(result) = eval_loop_body(env)? {
					return result;
				}
			}
			Ok(Val::empty())
		}
		invalid => Err(RtErr::UnaryTypeMismatch {
			context: "for-loop range",
			expected: vec![Type::List, Type::Range],
			actual: invalid.get_type(),
		}),
	}
}

/// Evaluates a numerical operation on two values.
fn eval_bin_op(
	env: &mut SharedEnv,
	left: Val,
	right: Val,
	op_name: &'static str,
	op: fn(Quant, Quant) -> EvalResult,
) -> EvalResult {
	match (left, right) {
		// Quantity op Quantity
		(Val::Quant(left), Val::Quant(right)) => op(left, right),
		// List op Quantity
		(Val::List(list), Val::Quant(quant)) => {
			Ok(Val::List(list.map(|elem| {
				eval_bin_op(env, elem.clone(), Val::Quant(quant.clone()), op_name, op)
			})?))
		}
		// Quantity op List
		(Val::Quant(quant), Val::List(list)) => {
			Ok(Val::List(list.map(|elem| {
				eval_bin_op(env, Val::Quant(quant.clone()), elem.clone(), op_name, op)
			})?))
		}
		// Invalid numeric operation
		(left, right) => Err(RtErr::BinaryTypeMismatch {
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

fn eval_evaluated_cluster(
	env: &mut SharedEnv,
	mut cluster: Vec<EvaluatedClusterItem>,
) -> EvalResult {
	// Parenthesized applications
	for idx in 0..cluster.len() - 1 {
		if let Val::Closure(closure) = &cluster[idx].value {
			if cluster[idx + 1].connector == ClusterConnector::AdjParen {
				cluster[idx].value =
					eval_application(closure.clone(), cluster[idx + 1].value.clone())?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			}
		}
	}
	// List/string indexing
	for idx in 0..cluster.len() - 1 {
		if let ClusterConnector::AdjNonparen = &cluster[idx + 1].connector {
			if let Val::List(right) = &cluster[idx + 1].value {
				// The RHS may be an index/slice.
				cluster[idx].value = match &cluster[idx].value {
					// List index/slice operation
					Val::List(left) => left.slice(right.as_index(env, left.len() as i64)?),
					// String index/slice operation
					Val::Prim(Prim::Text(left)) => {
						Ok(left.slice(right.as_index(env, left.len() as i64)?)?)
					}
					_ => continue,
				}?;
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
				cluster[idx].value =
					eval_application(closure.clone(), cluster[idx + 1].value.clone())?;
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
			}
			_ => (),
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
			}
			ClusterConnector::Div => {
				let left = cluster[idx].value.clone();
				let right = cluster[idx + 1].value.clone();
				cluster[idx].value = eval_bin_op(env, left, right, "division", |a, b| {
					Ok(Val::from((a / b).map_err(RtErr::quant)?))
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			}
			_ => (),
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
			value: if item.negated {
				eval_numeric_negation(env, value)?
			} else {
				value
			},
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
		Val::Tuple(tuple) => tuple.elems,
		_ => vec![args],
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
		LambdaBody::Intrinsic(body) => match body {
			Intrinsic::Pop => match local_env.lock().unwrap().get_var(&"list".into()).unwrap() {
				Val::List(list) => Ok(Val::List(list.tail().ok_or(RtErr::OutOfBounds)?)),
				arg => Err(RtErr::UnaryTypeMismatch {
					context: "pop()",
					expected: vec![Type::List],
					actual: arg.get_type(),
				}),
			},
		},
	}?;
	// Unwrap returned value, if any.
	Ok(match result {
		Val::Return { result } => *result,
		other => other,
	})
}

fn eval_numeric_negation(env: &mut SharedEnv, value: Val) -> EvalResult {
	match value {
		Val::Quant(quant) => Ok(Val::Quant(-quant)),
		Val::List(list) => Ok(Val::List(
			list.map(|elem| eval_numeric_negation(env, elem.clone()))?,
		)),
		_ => Err(RtErr::UnaryTypeMismatch {
			context: "negation",
			expected: vec![
				Type::Quant(NumType::Integer),
				Type::Quant(NumType::Rational),
				Type::Quant(NumType::Real),
				Type::List,
			],
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
	use crate::primitives::{Num, NumType, Prim, Type};
	use crate::values::{List, QuantErr, Tuple, UnitErr, Val};
	use crate::{make_list, make_list_value, make_tuple_value};

	use bigdecimal::BigDecimal;

	use std::str::FromStr;

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
			assert_eq!(
				Val::from(true),
				eval(&mut env, "(1/3 as real) ~ 0.333333333333")?
			);
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
				_ => panic!(),
			}
			match eval(&mut env, "(1, 2, 3) < (3, 2, 1)").err().unwrap() {
				GynjoErr::Rt(RtErr::BinaryTypeMismatch { .. }) => (),
				_ => panic!(),
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
				_ => panic!(),
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
		assert_eq!(
			Val::scalar(Num::rational(1, 4)),
			eval(&mut Env::new(None), "2^-2")?
		);
		Ok(())
	}

	#[test]
	fn rational_negation() -> Result<(), GynjoErr> {
		assert_eq!(
			Val::scalar(Num::rational(-1, 2)),
			eval(&mut Env::new(None), "-1/2")?
		);
		Ok(())
	}

	mod mixed_domain_math {
		use super::*;
		#[test]
		fn real_literals_decay_to_integer() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(3), eval(&mut Env::new(None), "3.0")?);
			assert_eq!(
				Val::scalar(4_000_000),
				eval(&mut Env::new(None), "4000000.0")?
			);
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
			assert_eq!(
				Val::scalar(Num::rational(1, 2)),
				eval(&mut Env::new(None), "1 / 2")?
			);
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
			let expected = make_tuple_value!(
				Val::scalar(1),
				make_tuple_value!(Val::scalar(2), Val::scalar(3))
			);
			assert_eq!(expected, eval(&mut Env::new(None), "(1, (2, 3))")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers_and_booleans() -> Result<(), GynjoErr> {
			let expected = make_tuple_value!(
				Val::from(true),
				make_tuple_value!(Val::scalar(2), Val::from(false))
			);
			assert_eq!(expected, eval(&mut Env::new(None), "(1 < 2, (2, false))")?);
			Ok(())
		}
	}

	mod list_construction {
		use super::*;
		#[test]
		fn singleton_list() -> Result<(), GynjoErr> {
			assert_eq!(
				make_list_value!(Val::scalar(1)),
				eval(&mut Env::new(None), "[1]")?
			);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers() -> Result<(), GynjoErr> {
			let expected = make_list_value!(
				Val::scalar(1),
				make_list_value!(Val::scalar(2), Val::scalar(3))
			);
			assert_eq!(expected, eval(&mut Env::new(None), "[1, [2, 3]]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers_and_booleans() -> Result<(), GynjoErr> {
			let expected = make_list_value!(
				Val::from(true),
				make_list_value!(Val::scalar(2), Val::from(false))
			);
			assert_eq!(expected, eval(&mut Env::new(None), "[1 < 2, [2, false]]")?);
			Ok(())
		}
	}

	#[test]
	fn list_destruction_does_not_cause_stack_overflow() {
		let result = eval(
			&mut Env::new(None),
			r"{
			let i = 0;
			let l = [];
			while i < 1000 do {
				let l = [i] | l;
				let i = i + 1;
			}
		}",
		);
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
			assert_eq!(
				Val::from("hello, 1".to_string()),
				eval(&mut env, r#""hello, " | 1"#)?
			);
			assert_eq!(
				Val::from("1 world".to_string()),
				eval(&mut env, r#"1 | " world""#)?
			);
			assert_eq!(
				Val::from("hello, world".to_string()),
				eval(&mut env, r#""hello, " | "world""#)?
			);
			Ok(())
		}
		#[test]
		fn list_concatenation() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				make_list_value!(Val::scalar(1), Val::scalar(2)),
				eval(&mut env, r#"[1] | [2]"#)?
			);
			assert_eq!(
				Val::from("1[2]".to_string()),
				eval(&mut env, r#""1" | [2]"#)?
			);
			assert_eq!(
				Val::from("[1]2".to_string()),
				eval(&mut env, r#"[1] | "2""#)?
			);
			Ok(())
		}
	}

	mod indexing_and_slicing {
		use super::*;
		use rstest::*;
		#[test]
		fn valid_list_indexing() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(1), eval(&mut env, "[1, 2][0]")?);
			assert_eq!(Val::scalar(2), eval(&mut env, "[1, 2][1]")?);
			assert_eq!(Val::scalar(1), eval(&mut env, "[1, 2][2]")?);
			assert_eq!(Val::scalar(2), eval(&mut env, "[1, 2][-1]")?);
			Ok(())
		}
		#[rstest]
		#[case("..", "1,2")]
		#[case(".. by -2", "2")]
		#[case(".. by -1", "2,1")]
		#[case(".. by 1", "1,2")]
		#[case(".. by 2", "1")]
		#[case("..-1", "")]
		#[case("..-1 by -2", "2")]
		#[case("..-1 by -1", "2,1")]
		#[case("..-1 by 1", "")]
		#[case("..-1 by 2", "")]
		#[case("..0", "")]
		#[case("..0 by -2", "2")]
		#[case("..0 by -1", "2,1")]
		#[case("..0 by 1", "")]
		#[case("..0 by 2", "")]
		#[case("..1", "1")]
		#[case("..1 by -2", "2")]
		#[case("..1 by -1", "2")]
		#[case("..1 by 1", "1")]
		#[case("..1 by 2", "1")]
		#[case("..2", "1,2")]
		#[case("..2 by -2", "")]
		#[case("..2 by -1", "")]
		#[case("..2 by 1", "1,2")]
		#[case("..2 by 2", "1")]
		#[case("..3", "1,2")]
		#[case("..3 by -2", "")]
		#[case("..3 by -1", "")]
		#[case("..3 by 1", "1,2")]
		#[case("..3 by 2", "1")]
		#[case("-1..", "1,2")]
		#[case("-1.. by -2", "")]
		#[case("-1.. by -1", "")]
		#[case("-1.. by 1", "1,2")]
		#[case("-1.. by 2", "2")]
		#[case("-1..-1", "")]
		#[case("-1..-1 by -2", "")]
		#[case("-1..-1 by -1", "")]
		#[case("-1..-1 by 1", "")]
		#[case("-1..-1 by 2", "")]
		#[case("-1..0", "")]
		#[case("-1..0 by -2", "")]
		#[case("-1..0 by -1", "")]
		#[case("-1..0 by 1", "")]
		#[case("-1..0 by 2", "")]
		#[case("-1..1", "1")]
		#[case("-1..1 by -2", "")]
		#[case("-1..1 by -1", "")]
		#[case("-1..1 by 1", "1")]
		#[case("-1..1 by 2", "")]
		#[case("-1..2", "1,2")]
		#[case("-1..2 by -2", "")]
		#[case("-1..2 by -1", "")]
		#[case("-1..2 by 1", "1,2")]
		#[case("-1..2 by 2", "2")]
		#[case("-1..3", "1,2")]
		#[case("-1..3 by -2", "")]
		#[case("-1..3 by -1", "")]
		#[case("-1..3 by 1", "1,2")]
		#[case("-1..3 by 2", "2")]
		#[case("0..", "1,2")]
		#[case("0.. by -2", "")]
		#[case("0.. by -1", "")]
		#[case("0.. by 1", "1,2")]
		#[case("0.. by 2", "1")]
		#[case("0..-1", "")]
		#[case("0..-1 by -2", "")]
		#[case("0..-1 by -1", "")]
		#[case("0..-1 by 1", "")]
		#[case("0..-1 by 2", "")]
		#[case("0..0", "")]
		#[case("0..0 by -2", "")]
		#[case("0..0 by -1", "")]
		#[case("0..0 by 1", "")]
		#[case("0..0 by 2", "")]
		#[case("0..1", "1")]
		#[case("0..1 by -2", "")]
		#[case("0..1 by -1", "")]
		#[case("0..1 by 1", "1")]
		#[case("0..1 by 2", "1")]
		#[case("0..2", "1,2")]
		#[case("0..2 by -2", "")]
		#[case("0..2 by -1", "")]
		#[case("0..2 by 1", "1,2")]
		#[case("0..2 by 2", "1")]
		#[case("0..3", "1,2")]
		#[case("0..3 by -2", "")]
		#[case("0..3 by -1", "")]
		#[case("0..3 by 1", "1,2")]
		#[case("0..3 by 2", "1")]
		#[case("1..", "2")]
		#[case("1.. by -2", "1")]
		#[case("1.. by -1", "1")]
		#[case("1.. by 1", "2")]
		#[case("1.. by 2", "2")]
		#[case("1..-1", "1")]
		#[case("1..-1 by -2", "1")]
		#[case("1..-1 by -1", "1")]
		#[case("1..-1 by 1", "")]
		#[case("1..-1 by 2", "")]
		#[case("1..0", "1")]
		#[case("1..0 by -2", "1")]
		#[case("1..0 by -1", "1")]
		#[case("1..0 by 1", "")]
		#[case("1..0 by 2", "")]
		#[case("1..1", "")]
		#[case("1..1 by -2", "")]
		#[case("1..1 by -1", "")]
		#[case("1..1 by 1", "")]
		#[case("1..1 by 2", "")]
		#[case("1..2", "2")]
		#[case("1..2 by -2", "")]
		#[case("1..2 by -1", "")]
		#[case("1..2 by 1", "2")]
		#[case("1..2 by 2", "2")]
		#[case("1..3", "2")]
		#[case("1..3 by -2", "")]
		#[case("1..3 by -1", "")]
		#[case("1..3 by 1", "2")]
		#[case("1..3 by 2", "2")]
		#[case("2..", "")]
		#[case("2.. by -2", "2")]
		#[case("2.. by -1", "2,1")]
		#[case("2.. by 1", "")]
		#[case("2.. by 2", "")]
		#[case("2..-1", "2,1")]
		#[case("2..-1 by -2", "2")]
		#[case("2..-1 by -1", "2,1")]
		#[case("2..-1 by 1", "")]
		#[case("2..-1 by 2", "")]
		#[case("2..0", "2,1")]
		#[case("2..0 by -2", "2")]
		#[case("2..0 by -1", "2,1")]
		#[case("2..0 by 1", "")]
		#[case("2..0 by 2", "")]
		#[case("2..1", "2")]
		#[case("2..1 by -2", "2")]
		#[case("2..1 by -1", "2")]
		#[case("2..1 by 1", "")]
		#[case("2..1 by 2", "")]
		#[case("2..2", "")]
		#[case("2..2 by -2", "")]
		#[case("2..2 by -1", "")]
		#[case("2..2 by 1", "")]
		#[case("2..2 by 2", "")]
		#[case("2..3", "")]
		#[case("2..3 by -2", "")]
		#[case("2..3 by -1", "")]
		#[case("2..3 by 1", "")]
		#[case("2..3 by 2", "")]
		#[case("3..", "")]
		#[case("3.. by -2", "1")]
		#[case("3.. by -1", "2,1")]
		#[case("3.. by 1", "")]
		#[case("3.. by 2", "")]
		#[case("3..-1", "2,1")]
		#[case("3..-1 by -2", "1")]
		#[case("3..-1 by -1", "2,1")]
		#[case("3..-1 by 1", "")]
		#[case("3..-1 by 2", "")]
		#[case("3..0", "2,1")]
		#[case("3..0 by -2", "1")]
		#[case("3..0 by -1", "2,1")]
		#[case("3..0 by 1", "")]
		#[case("3..0 by 2", "")]
		#[case("3..1", "2")]
		#[case("3..1 by -2", "")]
		#[case("3..1 by -1", "2")]
		#[case("3..1 by 1", "")]
		#[case("3..1 by 2", "")]
		#[case("3..2", "")]
		#[case("3..2 by -2", "")]
		#[case("3..2 by -1", "")]
		#[case("3..2 by 1", "")]
		#[case("3..2 by 2", "")]
		#[case("3..3", "")]
		#[case("3..3 by -2", "")]
		#[case("3..3 by -1", "")]
		#[case("3..3 by 1", "")]
		#[case("3..3 by 2", "")]
		fn valid_list_slicing(#[case] index: &str, #[case] expected: &str) -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let input = format!("[1,2][{}]", index);
			let expected = eval(&mut env, &format!("[{}]", expected))?;
			let actual = eval(&mut env, &input)?;
			assert_eq!(expected, actual);
			Ok(())
		}
		#[test]
		fn invalid_list_indexing() {
			let mut env = Env::new(None);
			assert_eq!(
				GynjoErr::Rt(RtErr::OutOfBounds),
				eval(&mut env, "[][0]").err().unwrap()
			);
			match eval(&mut env, "[1][true]").err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => panic!(),
			}
			match eval(&mut env, "[1][1, 2]").err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => panic!(),
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
		#[rstest]
		#[case("..", "hi")]
		#[case(".. by -2", "i")]
		#[case(".. by -1", "ih")]
		#[case(".. by 1", "hi")]
		#[case(".. by 2", "h")]
		#[case("..-1", "")]
		#[case("..-1 by -2", "i")]
		#[case("..-1 by -1", "ih")]
		#[case("..-1 by 1", "")]
		#[case("..-1 by 2", "")]
		#[case("..0", "")]
		#[case("..0 by -2", "i")]
		#[case("..0 by -1", "ih")]
		#[case("..0 by 1", "")]
		#[case("..0 by 2", "")]
		#[case("..1", "h")]
		#[case("..1 by -2", "i")]
		#[case("..1 by -1", "i")]
		#[case("..1 by 1", "h")]
		#[case("..1 by 2", "h")]
		#[case("..2", "hi")]
		#[case("..2 by -2", "")]
		#[case("..2 by -1", "")]
		#[case("..2 by 1", "hi")]
		#[case("..2 by 2", "h")]
		#[case("..3", "hi")]
		#[case("..3 by -2", "")]
		#[case("..3 by -1", "")]
		#[case("..3 by 1", "hi")]
		#[case("..3 by 2", "h")]
		#[case("-1..", "hi")]
		#[case("-1.. by -2", "")]
		#[case("-1.. by -1", "")]
		#[case("-1.. by 1", "hi")]
		#[case("-1.. by 2", "i")]
		#[case("-1..-1", "")]
		#[case("-1..-1 by -2", "")]
		#[case("-1..-1 by -1", "")]
		#[case("-1..-1 by 1", "")]
		#[case("-1..-1 by 2", "")]
		#[case("-1..0", "")]
		#[case("-1..0 by -2", "")]
		#[case("-1..0 by -1", "")]
		#[case("-1..0 by 1", "")]
		#[case("-1..0 by 2", "")]
		#[case("-1..1", "h")]
		#[case("-1..1 by -2", "")]
		#[case("-1..1 by -1", "")]
		#[case("-1..1 by 1", "h")]
		#[case("-1..1 by 2", "")]
		#[case("-1..2", "hi")]
		#[case("-1..2 by -2", "")]
		#[case("-1..2 by -1", "")]
		#[case("-1..2 by 1", "hi")]
		#[case("-1..2 by 2", "i")]
		#[case("-1..3", "hi")]
		#[case("-1..3 by -2", "")]
		#[case("-1..3 by -1", "")]
		#[case("-1..3 by 1", "hi")]
		#[case("-1..3 by 2", "i")]
		#[case("0..", "hi")]
		#[case("0.. by -2", "")]
		#[case("0.. by -1", "")]
		#[case("0.. by 1", "hi")]
		#[case("0.. by 2", "h")]
		#[case("0..-1", "")]
		#[case("0..-1 by -2", "")]
		#[case("0..-1 by -1", "")]
		#[case("0..-1 by 1", "")]
		#[case("0..-1 by 2", "")]
		#[case("0..0", "")]
		#[case("0..0 by -2", "")]
		#[case("0..0 by -1", "")]
		#[case("0..0 by 1", "")]
		#[case("0..0 by 2", "")]
		#[case("0..1", "h")]
		#[case("0..1 by -2", "")]
		#[case("0..1 by -1", "")]
		#[case("0..1 by 1", "h")]
		#[case("0..1 by 2", "h")]
		#[case("0..2", "hi")]
		#[case("0..2 by -2", "")]
		#[case("0..2 by -1", "")]
		#[case("0..2 by 1", "hi")]
		#[case("0..2 by 2", "h")]
		#[case("0..3", "hi")]
		#[case("0..3 by -2", "")]
		#[case("0..3 by -1", "")]
		#[case("0..3 by 1", "hi")]
		#[case("0..3 by 2", "h")]
		#[case("1..", "i")]
		#[case("1.. by -2", "h")]
		#[case("1.. by -1", "h")]
		#[case("1.. by 1", "i")]
		#[case("1.. by 2", "i")]
		#[case("1..-1", "h")]
		#[case("1..-1 by -2", "h")]
		#[case("1..-1 by -1", "h")]
		#[case("1..-1 by 1", "")]
		#[case("1..-1 by 2", "")]
		#[case("1..0", "h")]
		#[case("1..0 by -2", "h")]
		#[case("1..0 by -1", "h")]
		#[case("1..0 by 1", "")]
		#[case("1..0 by 2", "")]
		#[case("1..1", "")]
		#[case("1..1 by -2", "")]
		#[case("1..1 by -1", "")]
		#[case("1..1 by 1", "")]
		#[case("1..1 by 2", "")]
		#[case("1..2", "i")]
		#[case("1..2 by -2", "")]
		#[case("1..2 by -1", "")]
		#[case("1..2 by 1", "i")]
		#[case("1..2 by 2", "i")]
		#[case("1..3", "i")]
		#[case("1..3 by -2", "")]
		#[case("1..3 by -1", "")]
		#[case("1..3 by 1", "i")]
		#[case("1..3 by 2", "i")]
		#[case("2..", "")]
		#[case("2.. by -2", "i")]
		#[case("2.. by -1", "ih")]
		#[case("2.. by 1", "")]
		#[case("2.. by 2", "")]
		#[case("2..-1", "ih")]
		#[case("2..-1 by -2", "i")]
		#[case("2..-1 by -1", "ih")]
		#[case("2..-1 by 1", "")]
		#[case("2..-1 by 2", "")]
		#[case("2..0", "ih")]
		#[case("2..0 by -2", "i")]
		#[case("2..0 by -1", "ih")]
		#[case("2..0 by 1", "")]
		#[case("2..0 by 2", "")]
		#[case("2..1", "i")]
		#[case("2..1 by -2", "i")]
		#[case("2..1 by -1", "i")]
		#[case("2..1 by 1", "")]
		#[case("2..1 by 2", "")]
		#[case("2..2", "")]
		#[case("2..2 by -2", "")]
		#[case("2..2 by -1", "")]
		#[case("2..2 by 1", "")]
		#[case("2..2 by 2", "")]
		#[case("2..3", "")]
		#[case("2..3 by -2", "")]
		#[case("2..3 by -1", "")]
		#[case("2..3 by 1", "")]
		#[case("2..3 by 2", "")]
		#[case("3..", "")]
		#[case("3.. by -2", "h")]
		#[case("3.. by -1", "ih")]
		#[case("3.. by 1", "")]
		#[case("3.. by 2", "")]
		#[case("3..-1", "ih")]
		#[case("3..-1 by -2", "h")]
		#[case("3..-1 by -1", "ih")]
		#[case("3..-1 by 1", "")]
		#[case("3..-1 by 2", "")]
		#[case("3..0", "ih")]
		#[case("3..0 by -2", "h")]
		#[case("3..0 by -1", "ih")]
		#[case("3..0 by 1", "")]
		#[case("3..0 by 2", "")]
		#[case("3..1", "i")]
		#[case("3..1 by -2", "")]
		#[case("3..1 by -1", "i")]
		#[case("3..1 by 1", "")]
		#[case("3..1 by 2", "")]
		#[case("3..2", "")]
		#[case("3..2 by -2", "")]
		#[case("3..2 by -1", "")]
		#[case("3..2 by 1", "")]
		#[case("3..2 by 2", "")]
		#[case("3..3", "")]
		#[case("3..3 by -2", "")]
		#[case("3..3 by -1", "")]
		#[case("3..3 by 1", "")]
		#[case("3..3 by 2", "")]
		fn valid_string_slicing(
			#[case] index: &str,
			#[case] expected: &str,
		) -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let input = format!(r#" "hi"[{}] "#, index);
			let expected = eval(&mut env, &format!(r#" "{}" "#, expected))?;
			let actual = eval(&mut env, &input)?;
			assert_eq!(expected, actual);
			Ok(())
		}
		#[test]
		fn invalid_string_indexing() {
			let mut env = Env::new(None);
			assert_eq!(
				GynjoErr::Rt(RtErr::OutOfBounds),
				eval(&mut env, r#" ""[0] "#).err().unwrap()
			);
			match eval(&mut env, r#" "hi"[true] "#).err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => panic!(),
			}
			match eval(&mut env, r#" "hi"[1, 2] "#).err().unwrap() {
				GynjoErr::Rt(RtErr::InvalidIndex { .. }) => (),
				_ => panic!(),
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
		eval(
			&mut env,
			r"{
			let sum = a -> b -> a + b;
			let get_a = () -> a;
		}",
		)?;
		// "a" should be undefined.
		match eval(&mut env, "sum (1) (2) get_a ()").err().unwrap() {
			GynjoErr::Rt(RtErr::Undefined(_)) => (),
			_ => panic!(),
		}
		Ok(())
	}

	#[test]
	fn chained_application_with_and_without_parentheses() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(
			&mut env,
			r"{
			let sum = a -> b -> a + b;
			let inc = a -> a + 1;
		}",
		)?;
		assert_eq!(Val::scalar(3), eval(&mut env, "sum (1) 2")?);
		Ok(())
	}

	#[test]
	fn chained_application_does_not_pollute_applications_higher_in_the_call_chain(
	) -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		eval(
			&mut env,
			r"{
			let sum = a -> b -> a + b;
			let inc = b -> b + 1;
		}",
		)?;
		assert_eq!(Val::scalar(8), eval(&mut env, "sum (inc 5) 2")?);
		Ok(())
	}

	#[test]
	fn returning_from_nested_block() -> Result<(), GynjoErr> {
		let mut env = Env::new(None);
		assert_eq!(
			Val::scalar(7),
			eval(
				&mut env,
				r"{
			let f = x -> {
				if x = 3 then {
					return 7
				};
				x
			};
			f(3)
		}"
			)?
		);
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
				_ => panic!(),
			}
			match eval(&mut env, "{1;}").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => panic!(),
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
			assert_eq!(
				Val::empty(),
				eval(&mut Env::new(None), "if false then let a = 1/0")?
			);
			Ok(())
		}
	}

	mod while_loop {
		use super::*;
		#[test]
		fn basic() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			eval(
				&mut env,
				r"{
				let a = 0;
				while a < 3 do let a = a + 1;
			}",
			)?;
			assert_eq!(Val::scalar(3), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn unused_result() {
			match eval(&mut Env::new(None), "while true do 1").err().unwrap() {
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => panic!(),
			}
		}
	}

	mod for_loop {
		use super::*;
		#[test]
		fn over_list() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let result = eval(
				&mut env,
				r"{
				let a = 0;
				for x in [1, 2, 3] do let a = a + x;
				for x in [] do let a = 10;
				a
			}",
			)?;
			assert_eq!(Val::scalar(6), result);
			Ok(())
		}
		#[test]
		fn over_range() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let result = eval(
				&mut env,
				r"{
				let a = 0;
				for x in 1..4 do let a = a + x;
				for x in [] do let a = 10;
				a
			}",
			)?;
			assert_eq!(Val::scalar(6), result);
			Ok(())
		}
		#[test]
		fn unused_result() {
			match eval(&mut Env::new(None), "for x in [1, 2, 3] do 1")
				.err()
				.unwrap()
			{
				GynjoErr::Rt(RtErr::UnusedResult(_)) => (),
				_ => panic!(),
			}
		}
	}

	mod return_and_break {
		use super::*;
		#[test]
		fn return_outside_function_is_okay() -> Result<(), GynjoErr> {
			assert_eq!(
				Val::Return {
					result: Box::new(Val::scalar(1))
				},
				eval(&mut Env::new(None), "return 1")?
			);
			Ok(())
		}
		#[test]
		fn break_outside_loop_is_okay() -> Result<(), GynjoErr> {
			assert_eq!(Val::Break, eval(&mut Env::new(None), "break")?);
			Ok(())
		}
		#[test]
		fn return_is_idempotent() -> Result<(), GynjoErr> {
			assert_eq!(
				Val::Return {
					result: Box::new(Val::scalar(1))
				},
				eval(&mut Env::new(None), "return return 1")?
			);
			Ok(())
		}
		#[test]
		fn return_from_nested_block() -> Result<(), GynjoErr> {
			assert_eq!(
				Val::Return {
					result: Box::new(Val::scalar(1))
				},
				eval(
					&mut Env::new(None),
					r"{
				if true then {
					return 1
				};
				2
			}"
				)?
			);
			Ok(())
		}
		#[test]
		fn return_from_called_function() -> Result<(), GynjoErr> {
			assert_eq!(
				Val::scalar(1),
				eval(
					&mut Env::new(None),
					r"{
				let f = () -> return ();
				f();
				1
			}"
				)?
			);
			Ok(())
		}
		#[test]
		fn break_while() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::empty(),
				eval(
					&mut env,
					r"
				while true do {
					break;
					1/0
				}
			"
				)?
			);
			Ok(())
		}
		#[test]
		fn return_from_while() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::scalar(7),
				eval(
					&mut env,
					r"(() -> {
				while true do {
					return 7;
					1/0
				}
			})()"
				)?
			);
			Ok(())
		}
		#[test]
		fn break_for() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::scalar(1),
				eval(
					&mut env,
					r"{
				for x in [1, 2, 3] do {
					break;
					1/0
				};
				x
			}"
				)?
			);
			Ok(())
		}
		#[test]
		fn return_from_for() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::scalar(7),
				eval(
					&mut env,
					r"(() -> {
				for x in [1, 2, 3] do {
					return 7;
					1/0
				};
			})()"
				)?
			);
			Ok(())
		}
	}

	mod intrinsics {
		use super::*;
		#[test]
		fn pop() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(make_list_value!(), eval(&mut env, "pop([1])")?);
			assert_eq!(
				GynjoErr::Rt(RtErr::OutOfBounds),
				eval(&mut env, "pop([])").err().unwrap()
			);
			Ok(())
		}
	}

	mod type_operations {
		use super::*;
		#[test]
		fn type_as_itself() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(Val::scalar(1), eval(&mut env, "1 as integer")?);
			assert_eq!(
				Val::from("hello".to_string()),
				eval(&mut env, r#""hello" as text"#)?
			);
			assert_eq!(
				Val::from(true),
				eval(&mut env, "(x -> x) = (x -> x) as closure")?
			);
			Ok(())
		}
		#[test]
		fn domain_expansion() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::scalar(Num::rational(1, 1)),
				eval(&mut env, "1 as rational")?
			);
			assert_eq!(Val::scalar(1.0), eval(&mut env, "1 as real")?);
			assert_eq!(Val::scalar(1.0), eval(&mut env, "1 as rational as real")?);
			Ok(())
		}
		#[test]
		fn tuple_list_conversion() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				make_list_value!(Val::scalar(1), Val::scalar(2)),
				eval(&mut env, "(1, 2) as list")?
			);
			assert_eq!(
				make_tuple_value!(Val::scalar(1), Val::scalar(2)),
				eval(&mut env, "[1, 2] as tuple")?
			);
			Ok(())
		}
		#[test]
		fn range_conversions() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				make_list_value!(Val::scalar(1), Val::scalar(2)),
				eval(&mut env, "(1..3) as list")?
			);
			assert_eq!(
				make_tuple_value!(Val::scalar(1), Val::scalar(2)),
				eval(&mut env, "(1..3) as tuple")?
			);
			assert!(eval(&mut env, "(1..) as list").is_err());
			assert!(eval(&mut env, "(..1) as list").is_err());
			assert!(eval(&mut env, "(1..) as tuple").is_err());
			assert!(eval(&mut env, "(..1) as tuple").is_err());
			Ok(())
		}
		#[test]
		fn conversion_to_text() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::from("true".to_string()),
				eval(&mut env, "true as text")?
			);
			assert_eq!(Val::from("1".to_string()), eval(&mut env, "1 as text")?);
			assert_eq!(
				Val::from("(1, 2)".to_string()),
				eval(&mut env, "(1, 2) as text")?
			);
			Ok(())
		}
		#[test]
		fn invalid_conversions() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			let real_to_integer = GynjoErr::Rt(RtErr::InvalidTypeCast {
				from: Type::Quant(NumType::Real),
				to: Type::Quant(NumType::Integer),
			});
			let string_to_real = GynjoErr::Rt(RtErr::InvalidTypeCast {
				from: Type::Text,
				to: Type::Quant(NumType::Real),
			});
			let closure_to_boolean = GynjoErr::Rt(RtErr::InvalidTypeCast {
				from: Type::Closure,
				to: Type::Boolean,
			});
			assert_eq!(
				real_to_integer,
				eval(&mut env, "1.5 as integer").err().unwrap()
			);
			assert_eq!(
				string_to_real,
				eval(&mut env, r#""five" as real"#).err().unwrap()
			);
			assert_eq!(
				closure_to_boolean,
				eval(&mut env, "(x -> x) as boolean").err().unwrap()
			);
			Ok(())
		}
		#[test]
		fn get_type() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!(
				Val::Prim(Prim::Type(Type::Quant(NumType::Integer))),
				eval(&mut env, "get_type 1")?
			);
			assert_eq!(
				Val::Prim(Prim::Type(Type::Quant(NumType::Real))),
				eval(&mut env, "get_type 1.5")?
			);
			assert_eq!(
				Val::Prim(Prim::Type(Type::List)),
				eval(&mut env, "get_type []")?
			);
			assert_eq!(
				Val::Prim(Prim::Type(Type::List)),
				eval(&mut env, "get_type [1]")?
			);
			assert_eq!(
				Val::Prim(Prim::Type(Type::Type)),
				eval(&mut env, "get_type get_type 1")?
			);
			Ok(())
		}
	}

	mod units {
		use super::*;
		#[test]
		fn canceling() -> Result<(), GynjoErr> {
			assert_eq!(Val::scalar(5), eval(&mut Env::new(None), "5.m/1.m")?);
			Ok(())
		}
		#[test]
		fn single_unit_addition_and_subtraction() -> Result<(), GynjoErr> {
			let mut env = Env::new(None);
			assert_eq!("3.m", eval(&mut env, "1.m + 2.m")?.format_with_env(&env));
			assert_eq!("1.m", eval(&mut env, "2.m - 1.m")?.format_with_env(&env));
			Ok(())
		}
		#[test]
		fn invalid_addition_and_subtraction() {
			let mut env = Env::new(None);
			let err = GynjoErr::Rt(RtErr::Quant(QuantErr::Unit(UnitErr::Incompatible)));
			assert_eq!(err, eval(&mut env, "1.m + 1.s").err().unwrap());
			assert_eq!(err, eval(&mut env, "1.m - 1.s").err().unwrap());
		}
		#[test]
		fn multiplication() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("6.m.s", eval(&mut env, "2.m 3.s")?.format_with_env(&env));
			assert_eq!("6.m^2", eval(&mut env, "2.m 300.cm")?.format_with_env(&env));
			Ok(())
		}
		#[test]
		fn division() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("2.m.s^-1", eval(&mut env, "4.m/2.s")?.format_with_env(&env));
			assert_eq!("4", eval(&mut env, "4.m/100.cm")?.format_with_env(&env));
			assert_eq!(
				"16",
				eval(&mut env, "(4.m)^2/(100.cm)^2")?.format_with_env(&env)
			);
			Ok(())
		}
		#[test]
		fn mixed_unit_arithmetic() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("2.m", eval(&mut env, "1.m + 100.cm")?.format_with_env(&env));
			assert_eq!("1.m", eval(&mut env, "2.m - 100.cm")?.format_with_env(&env));
			Ok(())
		}
		#[test]
		fn mixed_unit_comparisons() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(Val::from(true), eval(&mut env, "1.m = 100.cm")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.m^2 = 1.m * 100.cm")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 1.m / 100.cm")?);
			assert_eq!(Val::from(true), eval(&mut env, "1 = 100.cm / 1.m")?);
			assert_eq!(Val::from(true), eval(&mut env, "1.m < 200.cm")?);
			assert_eq!(Val::from(true), eval(&mut env, "2.m > 100.cm")?);
			Ok(())
		}
		#[test]
		fn explicit_conversion() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!(
				"2.km",
				eval(&mut env, "1000.m + 100000.cm in .km")?.format_with_env(&env)
			);
			assert_eq!(
				"3600.s^2",
				eval(&mut env, ".min^2 in .s^2")?.format_with_env(&env)
			);
			Ok(())
		}
		#[test]
		fn basic_unit_conversion() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("1.m", eval(&mut env, "basic 100.cm")?.format_with_env(&env));
			assert_eq!(
				"3600.s^2",
				eval(&mut env, "basic .min^2")?.format_with_env(&env)
			);
			Ok(())
		}
		#[test]
		fn canonical_display_order() -> Result<(), GynjoErr> {
			let mut env = Env::with_core_libs();
			assert_eq!("1.kg.m.s", eval(&mut env, ".s.m.kg")?.format_with_env(&env));
			assert_eq!(
				"1.s^2.m.kg^-1",
				eval(&mut env, "(1/.kg).s.m.s")?.format_with_env(&env)
			);
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
				assert_eq!(
					Val::from(true),
					eval(&mut env, "abs(nCk(5, 3) - 10) < 10**-50")?
				);
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
			fn reverse() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(
					make_list_value!(Val::scalar(3), Val::scalar(2), Val::scalar(1)),
					eval(&mut env, "reverse [1, 2, 3]")?
				);
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
				assert_eq!(
					make_list_value!(Val::scalar(1)),
					eval(&mut env, "insert([], 0, 1)")?
				);
				Ok(())
			}
			#[test]
			fn remove_from_middle() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(
					make_list_value!(Val::scalar(1), Val::scalar(3)),
					eval(&mut env, "remove([1, 2, 3], 1)")?
				);
				Ok(())
			}
			#[test]
			fn remove_from_beginning() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(
					make_list_value!(Val::scalar(2), Val::scalar(3)),
					eval(&mut env, "remove([1, 2, 3], 0)")?
				);
				Ok(())
			}
			#[test]
			fn map() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(
					make_list_value!(Val::scalar(1), Val::scalar(4), Val::scalar(9)),
					eval(&mut env, "map([1, 2, 3], x -> x^2)")?
				);
				Ok(())
			}
			#[test]
			fn reduce() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				assert_eq!(
					Val::scalar(6),
					eval(&mut env, "reduce([1, 2, 3], 0, (a, b) -> a + b)")?
				);
				Ok(())
			}
			#[test]
			fn flatmap() -> Result<(), GynjoErr> {
				let mut env = Env::with_core_libs();
				let expected = make_list_value!(
					Val::scalar(1),
					Val::scalar(1),
					Val::scalar(2),
					Val::scalar(2)
				);
				assert_eq!(expected, eval(&mut env, "flatmap([1, 2], x -> [x, x])")?);
				Ok(())
			}
		}
	}
}
