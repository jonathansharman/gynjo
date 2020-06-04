use super::env::Env;
use super::exprs::{Expr, BinaryOp, Cluster, ClusterConnector, LambdaBody};
use super::intrinsics::Intrinsic;
use super::lexer::lex;
use super::number::Number;
use super::primitives::{Primitive, Boolean};
use super::parser::{parse_expr, parse_stmt};
use super::stmts::Stmt;
use super::values::{Closure, Tuple, List, Value};

use std::cell::RefCell;
use std::io;
use std::rc::Rc;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Value, String>;

/// Result of executing a Gynjo statement.
type ExecResult = Result<(), String>;

/// If possible, computes the value of `expr` in the context of `env`.
pub fn eval_expr(mut env: &mut Rc<RefCell<Env>>, expr: Expr) -> EvalResult {
	match expr {
		Expr::Cond { test, then_expr, else_expr } => {
			eval_expr(&mut env, *test).and_then(|test_value| {
				match test_value {
					Value::Primitive(Primitive::Boolean(boolean)) => eval_expr(&mut env, if boolean.into() { *then_expr } else { *else_expr }),
					_ => Err(format!("expected boolean in conditional test, found {}", test_value.to_string(env))),
				}
			})
		},
		Expr::Block { stmts } => {
			for stmt in stmts.into_iter() {
				// A return statement exits the block early and produces a value.
				if let Stmt::Return { result } = stmt {
					return eval_expr(&mut env, *result);
				}
				// Otherwise, just execute the statement and check for errors.
				if let Err(error) = exec_stmt(&mut env, stmt) {
					return Err(format!("in block statement: {}", error));
				}
			}
			// Return nothing if there was no return statement.
			Ok(Value::Tuple(Tuple::empty()))
		},
		Expr::BinaryExpr(bin_expr) => match bin_expr.op {
			BinaryOp::And => {
				match eval_expr(&mut env, *bin_expr.left)? {
					Value::Primitive(Primitive::Boolean(left)) => if left.into() {
						match eval_expr(&mut env, *bin_expr.right)? {
							Value::Primitive(Primitive::Boolean(right)) => Ok(right.into()),
							invalid @ _ => Err(format!("cannot take logical conjunction of non-boolean value {}", invalid.to_string(&env))),
						}
					} else {
						// Short-circuit to false.
						Ok(Boolean::False.into())
					},
					invalid @ _ => Err(format!("cannot take logical conjunction of non-boolean value {}", invalid.to_string(&env))),
				}
			},
			BinaryOp::Or => {
				match eval_expr(env, *bin_expr.left)? {
					Value::Primitive(Primitive::Boolean(left)) => if left.into() {
						// Short-circuit to true.
						Ok(Boolean::True.into())
					} else {
						match eval_expr(env, *bin_expr.right)? {
							Value::Primitive(Primitive::Boolean(right)) => Ok(right.into()),
							invalid @ _ => Err(format!("cannot take logical disjunction of non-boolean value {}", invalid.to_string(&env))),
						}
					},
					invalid @ _ => Err(format!("cannot take logical disjunction of non-boolean value {}", invalid.to_string(&env))),
				}
			},
			BinaryOp::Eq => Ok(Boolean::from(eval_expr(env, *bin_expr.left)? == eval_expr(env, *bin_expr.right)?).into()),
			BinaryOp::Neq => Ok(Boolean::from(eval_expr(env, *bin_expr.left)? != eval_expr(env, *bin_expr.right)?).into()),
			BinaryOp::Approx => Ok(Boolean::from(eval_expr(env, *bin_expr.left)?.to_string(&env) == eval_expr(env, *bin_expr.right)?.to_string(&env)).into()),
			BinaryOp::Lt => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Value::from(a < b)))
			},
			BinaryOp::Leq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Value::from(a <= b)))
			},
			BinaryOp::Gt => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Value::from(a > b)))
			},
			BinaryOp::Geq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| Ok(Value::from(a >= b)))
			},
			BinaryOp::Add => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "addition", |a, b| Ok(Value::from(a + b)))
			},
			BinaryOp::Sub => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "subtraction", |a, b| Ok(Value::from(a - b)))
			},
		},
		Expr::Not { expr } => {
			match eval_expr(env, *expr)? {
				Value::Primitive(Primitive::Boolean(b)) => Ok(Boolean::from(!bool::from(b)).into()),
				invalid @ _ => Err(format!("cannot take logical negation of {}", invalid.to_string(&env))),
			}
		},
		Expr::Cluster(cluster) => eval_cluster(env, cluster),
		Expr::Lambda(f) => Ok(Value::Closure(Closure { f: f, env: env.clone() })),
		Expr::TupleExpr(expr_elems) => {
			let mut elems = Box::new(Vec::with_capacity(expr_elems.len()));
			for value in expr_elems.into_iter().map(|elem| eval_expr(&mut env, elem)) {
				elems.push(value?);
			}
			Ok(Value::Tuple(Tuple { elems: elems }))
		},
		Expr::ListExpr(expr_elems) => {
			let mut list = List::Empty;
			for expr_elem in expr_elems.into_iter() {
				list = List::Cons { head: Box::new(eval_expr(&mut env, expr_elem)?), tail: Rc::new(list) };
			}
			Ok(Value::List(list))
		},
		Expr::Symbol(symbol) => env.borrow().lookup(&symbol)
			.map(|v| v.clone())
			.ok_or(format!("'{}' is undefined", symbol.name)),
		Expr::Primitive(primitive) => Ok(Value::Primitive(match primitive {
			Primitive::Number(number) => Primitive::Number(number.shrink_domain()),
			primitive @ _ => primitive
		})),
	}
}

/// If possible, computes the value of the expression contained in `input` in the context of `env`.
pub fn eval(env: &mut Rc<RefCell<Env>>, input: &str) -> EvalResult {
	// Lex.
	let tokens = lex(input).map_err(|err| format!("(lex error) {}", err))?;
	// Parse.
	let (tokens, expr) = parse_expr(&tokens[..]).map_err(|err| format!("(parse error) {}", err))?;
	if !tokens.is_empty() {
		Err(format!("(parse_error) unused tokens starting at {}", tokens.first().unwrap().to_string()))
	} else {
		// Evaluate.
		eval_expr(env, expr)
	}
}

/// If possible, executes `stmt` in the context of `env`.
pub fn exec_stmt(mut env: &mut Rc<RefCell<Env>>, stmt: Stmt) -> ExecResult {
	match stmt {
		Stmt::Nop => Ok(()),
		Stmt::Import { filename } => {
			let library_text = std::fs::read_to_string(&filename)
				.map_err(|err| format!("failed to load library \"{}\" ({})", filename, err.to_string()))?;
			exec(&mut env, &library_text)
		},
		Stmt::Assign { lhs, rhs } => {
			let rhs_value = eval_expr(env, *rhs).map_err(|err| format!("in RHS of assignment: {}", err))?;
			// Perform the assignment, possibly overwriting the existing value.
			env.borrow_mut().assign(lhs, rhs_value);
			Ok(())
		},
		Stmt::Branch { test, then_stmt, else_stmt } => {
			let test_value = eval_expr(env, *test).map_err(|err| format!("in branch test expression: {}", err))?;
			match test_value {
				Value::Primitive(Primitive::Boolean(b)) => exec_stmt(env, if b.into() { *then_stmt } else { *else_stmt }),
				_ => Err(format!("expected boolean in conditional test, found {}", test_value.to_string(env))),
			}
		},
		Stmt::WhileLoop { test, body } => {
			loop {
				// Evaluate the test condition.
				let test_value = eval_expr(env, (*test).clone()).map_err(|err| format!("in while-loop test expression: {}", err))?;
				match test_value {
					Value::Primitive(Primitive::Boolean(b)) => if b.into() {
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
				Value::List(range_list) => {
					for value in range_list.iter() {
						// Assign the loop variable to the current value in the range list.
						env.borrow_mut().assign(loop_var.clone(), value.clone());
						// Execute the loop body in this context.
						exec_stmt(env, (*body).clone()).map_err(|err| format!("in body of for-loop: {}", err))?;
					}
					Ok(())
				},
				_ => Err(format!("expected a list, found {}", range_value.to_string(env))),
			}
		},
		Stmt::Return { .. } => Err("cannot return outside statement block".to_string()),
		Stmt::ExprStmt(expr) => {
			let value = eval_expr(env, *expr)?;
			// Expression statements must evaluate to ().
			match value {
				Value::Tuple(Tuple { elems }) if (elems.is_empty()) => Ok(()),
				_ => Err(format!("unused expression result: {}", value.to_string(env))),
			}
		}
	}
}

/// If possible, executes the statements contained in `input` in the context of `env`.
pub fn exec(env: &mut Rc<RefCell<Env>>, input: &str) -> ExecResult {
	// Lex.
	let tokens = lex(input)?;
	let mut token_slice = &tokens[..];
	// While there is still input left, parse and execute.
	while !token_slice.is_empty() {
		// Parse.
		let (remaining_tokens, stmt) = parse_stmt(token_slice).map_err(|err| format!("(parse error) {}", err))?;
		token_slice = remaining_tokens;
		// Execute.
		exec_stmt(env, stmt).map_err(|err| format!("(runtime error) {}", err))?;
	}
	Ok(())
}

/// Evaluates a numerical operation on a list and a number.
fn list_num_op(env: &Rc<RefCell<Env>>, list: List, number: Number, number_on_left: bool, op_name: &str, op: fn(Number, Number) -> EvalResult) -> Result<List, String> {
	match list {
		List::Empty => Ok(List::Empty),
		List::Cons { head, tail } => Ok(List::Cons {
			head: if number_on_left {
				Box::new(bin_num_op(env, Value::Primitive(Primitive::Number(number.clone())), *head, op_name, op)?)
			} else {
				Box::new(bin_num_op(env, *head, Value::Primitive(Primitive::Number(number.clone())), op_name, op)?)
			},
			tail: Rc::new(list_num_op(env, (*tail).clone(), number, number_on_left, op_name, op)?),
		}),
	}
}

/// Evaluates a numerical operation on two values.
fn bin_num_op(env: &Rc<RefCell<Env>>, left: Value, right: Value, op_name: &str, op: fn(Number, Number) -> EvalResult) -> EvalResult {
	match (left, right) {
		// Number op Number
		(Value::Primitive(Primitive::Number(left)), Value::Primitive(Primitive::Number(right))) => Ok(op(left, right)?),
		// List op Number
		(Value::List(list), Value::Primitive(Primitive::Number(number))) => Ok(Value::List(list_num_op(env, list, number, false, op_name, op)?)),
		// Number op List
		(Value::Primitive(Primitive::Number(number)), Value::List(list)) => Ok(Value::List(list_num_op(env, list, number, true, op_name, op)?)),
		// Invalid numeric operation
		(left @ _, right @ _) => Err(format!("cannot perform {} with {} and {}", op_name, left.to_string(&env), right.to_string(&env))),
	}
}

/// A cluster item with its expression resolved to a value.
struct EvaluatedClusterItem {
	/// This item's value.
	value: Value,
	/// How this item is connected to the previous item.
	connector: ClusterConnector,
}

fn eval_evaluated_cluster(env: &mut Rc<RefCell<Env>>, mut cluster: Vec<EvaluatedClusterItem>) -> EvalResult {
	if cluster.len() > 1 {
		// Parenthesized applications
		for idx in 0..cluster.len() - 1 {
			if let Value::Closure(closure) = &cluster[idx].value {
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
				cluster[idx].value = bin_num_op(env, left, right, "exponentiation", |_, _| {
					Err("BigDecimal does not support exponentiation yet".to_string())
				})?;
				cluster.remove(idx + 1);
				return eval_evaluated_cluster(env, cluster);
			}
		}
		// Non-parenthesized applications
		for idx in 0..cluster.len() - 1 {
			if let Value::Closure(closure) = &cluster[idx].value {
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
					cluster[idx].value = bin_num_op(env, left, right, "multiplication", |a, b| Ok(Value::from(a * b)))?;
					cluster.remove(idx + 1);
					return eval_evaluated_cluster(env, cluster);
				},
				ClusterConnector::Div => {
					let left = cluster[idx].value.clone();
					let right = cluster[idx + 1].value.clone();
					cluster[idx].value = bin_num_op(env, left, right, "division", |a, b| {
						Ok(Value::from((a / b).ok_or("division by zero".to_string())?))
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
	let mut evaluated_cluster: Vec<EvaluatedClusterItem> = Vec::with_capacity(cluster.items.len());
	for item in cluster.items {
		let value = eval_expr(env, *item.expr)?;
		evaluated_cluster.push(EvaluatedClusterItem {
			value: if item.negated { eval_negation(env, value)? } else { value },
			connector: item.connector,
		});
	}
	// Now that the items' types are known, evaluate down to a single value.
	eval_evaluated_cluster(env, evaluated_cluster)
}

/// Evaluates a closure application.
fn eval_application(c: Closure, args: Value) -> EvalResult {
	// Extract arguments into a vector.
	let args = match args {
		Value::Tuple(tuple) => {
			*tuple.elems
		},
		_ => vec!(args),
	};
	// Ensure correct number of arguments.
	if args.len() != c.f.params.len() {
		return Err(format!("function requires {} argument{}, received {}", c.f.params.len(), if c.f.params.len() == 1 { "" } else { "s" }, args.len()));
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
					Value::List(List::Cons { head, .. }) => Ok((*head).clone()),
					arg @ _ => Err(format!("top() expected a non-empty list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Pop => match local_env.borrow().lookup(&"list".into()).unwrap() {
					Value::List(List::Cons { tail, .. }) => Ok(Value::List((*tail).clone())),
					arg @ _ => Err(format!("pop() expected a non-empty list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Push => match local_env.borrow().lookup(&"list".into()).unwrap() {
					Value::List(list) => {
						let value = local_env.borrow().lookup(&"value".into()).unwrap().clone();
						Ok(Value::List(List::Cons { head: Box::new(value), tail: Rc::new(list.clone()) }))
					},
					arg @ _ => Err(format!("push() expected a list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Print => {
					print!("{}", local_env.borrow().lookup(&"value".into()).unwrap().to_string(&local_env));
					Ok(Value::Tuple(Tuple::empty()))
				},
				Intrinsic::Read => {
					let mut input = String::new();
					io::stdin().read_line(&mut input).unwrap();
					Ok(Value::from(input.trim().to_string()))
				},
				Intrinsic::ToReal => match local_env.borrow().lookup(&"value".into()).unwrap() {
					Value::Primitive(Primitive::Number(number)) => Ok(Value::from(Number::Real(number.into()))),
					arg @ _ => Err(format!("cannot convert {} to real", arg.to_string(&local_env))),
				},
			}
		}
	}
}

fn negate_list(env: &Rc<RefCell<Env>>, list: List) -> Result<List, String> {
	match list {
		List::Empty => Ok(List::Empty),
		List::Cons { head, tail } => Ok(List::Cons {
			head: Box::new(eval_negation(env, *head)?),
			tail: Rc::new(negate_list(env, (*tail).clone())?),
		}),
	}
}

fn eval_negation(env: &Rc<RefCell<Env>>, value: Value) -> EvalResult {
	match value {
		Value::Primitive(Primitive::Number(Number::Integer(integer))) => Ok(Value::Primitive(Primitive::Number(Number::Integer(-integer)))),
		Value::Primitive(Primitive::Number(Number::Real(real))) => Ok(Value::Primitive(Primitive::Number(Number::Real(-real)))),
		Value::List(list) => Ok(Value::List(negate_list(env, list)?)),
		_ => Err(format!("cannot negate {}", value.to_string(env))),
	}
}

#[cfg(test)]
mod tests {
	use crate::interpreter::{eval, exec};
	use crate::env::Env;
	use crate::number::Number;
	use crate::primitives::Primitive;
	use crate::values::{Value, Tuple, List};

	use bigdecimal::BigDecimal;

	use std::cell::RefCell;
	use std::rc::Rc;
	use std::str::FromStr;

	#[test]
	fn execution_of_empty_statement_returns_nothing() -> Result<(), String> {
		assert_eq!((), exec(&mut Env::new(None), "")?);
		Ok(())
	}

	mod logical_operators {
		use super::*;
		#[test]
		fn and() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "false and false")?);
			assert_eq!(Value::from(false), eval(&mut env, "false and true")?);
			assert_eq!(Value::from(false), eval(&mut env, "true and false")?);
			assert_eq!(Value::from(true), eval(&mut env, "true and true")?);
			Ok(())
		}
		#[test]
		fn or() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "false or false")?);
			assert_eq!(Value::from(true), eval(&mut env, "false or true")?);
			assert_eq!(Value::from(true), eval(&mut env, "true or false")?);
			assert_eq!(Value::from(true), eval(&mut env, "true or true")?);
			Ok(())
		}
		#[test]
		fn not() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "not true")?);
			assert_eq!(Value::from(true), eval(&mut env, "not false")?);
			assert_eq!(Value::from(true), eval(&mut env, "not false and false")?);
			Ok(())
		}
		#[test]
		fn short_circuiting() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "false and 1/0")?);
			assert_eq!(Value::from(true), eval(&mut env, "true or 1/0")?);
			Ok(())
		}
		#[test]
		fn and_precedes_or() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "true or true and false")?);
			assert_eq!(Value::from(true), eval(&mut env, "false and true or true")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "(false) and true")?);
			assert_eq!(Value::from(true), eval(&mut env, "false or ((true))")?);
			assert_eq!(Value::from(true), eval(&mut env, "not (false)")?);
			Ok(())
		}
	}

	mod comparisons {
		use super::*;
		#[test]
		fn eq() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 = 1")?);
			assert_eq!(Value::from(false), eval(&mut env, "1 = 2")?);
			Ok(())
		}
		#[test]
		fn neq() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 != 2")?);
			assert_eq!(Value::from(false), eval(&mut env, "1 != 1")?);
			Ok(())
		}
		#[test]
		fn approx() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "real(1/3) ~ 0.333333333333")?);
			assert_eq!(Value::from(false), eval(&mut env, "real(1/3) ~ 0.333")?);
			Ok(())
		}
		#[test]
		fn lt() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 < 2.0")?);
			assert_eq!(Value::from(false), eval(&mut env, "1.0 < 1")?);
			assert_eq!(Value::from(false), eval(&mut env, "2 < 1")?);
			Ok(())
		}
		#[test]
		fn leq() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 <= 2.0")?);
			assert_eq!(Value::from(true), eval(&mut env, "1.0 <= 1")?);
			assert_eq!(Value::from(false), eval(&mut env, "2 <= 1")?);
			Ok(())
		}
		#[test]
		fn gt() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "1 > 2.0")?);
			assert_eq!(Value::from(false), eval(&mut env, "1.0 > 1")?);
			assert_eq!(Value::from(true), eval(&mut env, "2 > 1")?);
			Ok(())
		}
		#[test]
		fn geq() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "1 >= 2.0")?);
			assert_eq!(Value::from(true), eval(&mut env, "1.0 >= 1")?);
			assert_eq!(Value::from(true), eval(&mut env, "2 >= 1")?);
			Ok(())
		}
		#[test]
		fn comparisons_and_logical_operators() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 = 1 and 2 = 2")?);
			assert_eq!(Value::from(false), eval(&mut env, "1 = 1 and 2 = 3")?);
			assert_eq!(Value::from(true), eval(&mut env, "1 = 2 or 3 = 3")?);
			assert_eq!(Value::from(false), eval(&mut env, "1 = 2 or 3 = 4")?);
			Ok(())
		}
		#[test]
		fn non_numbers_are_equal_checkable_but_not_comparable() -> Result<(), String> {
			let mut env = Env::new(None);
			// Booleans and tuples can be equality-checked.
			assert_eq!(Value::from(true), eval(&mut env, "true = true")?);
			assert_eq!(Value::from(false), eval(&mut env, "true = false")?);
			assert_eq!(Value::from(false), eval(&mut env, "true != true")?);
			assert_eq!(Value::from(true), eval(&mut env, "true != false")?);
			assert_eq!(Value::from(true), eval(&mut env, "(1, 2, 3) = (1, 2, 3)")?);
			assert_eq!(Value::from(false), eval(&mut env, "(1, 2, 3) = (3, 2, 1)")?);
			assert_eq!(Value::from(false), eval(&mut env, "(1, 2, 3) != (1, 2, 3)")?);
			assert_eq!(Value::from(true), eval(&mut env, "(1, 2, 3) != (3, 2, 1)")?);
			// Cannot be compared.
			assert!(eval(&mut env, "false < true").is_err());
			assert!(eval(&mut env, "(1, 2, 3) < (3, 2, 1)").is_err());
			Ok(())
		}
		#[test]
		fn different_types_compare_inequal() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(false), eval(&mut env, "[1, 2, 3] = (1, 2, 3)")?);
			assert_eq!(Value::from(true), eval(&mut env, "(x -> x) != false")?);
			Ok(())
		}
		#[test]
		fn different_types_cannot_be_order_compared() -> Result<(), String> {
			assert!(eval(&mut Env::new(None), "(x -> x) < false").is_err());
			Ok(())
		}
		#[test]
		fn comparison_precedes_equality() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "1 < 2 = 2 < 3")?);
			assert_eq!(Value::from(true), eval(&mut env, "1 > 2 != 2 < 3")?);
			Ok(())
		}
		#[test]
		fn parenthesized() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(true), eval(&mut env, "(1) = 1")?);
			assert_eq!(Value::from(false), eval(&mut env, "1 != (1)")?);
			assert_eq!(Value::from(true), eval(&mut env, "((1)) < 2")?);
			assert_eq!(Value::from(true), eval(&mut env, "1 <= ((1))")?);
			assert_eq!(Value::from(false), eval(&mut env, "(1) > (1)")?);
			assert_eq!(Value::from(false), eval(&mut env, "((1)) >= (2)")?);
			Ok(())
		}
	}

	mod conditional_expressions {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), String> {
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "false ? 1/0 : 1")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), String> {
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "true ? 1 : 1/0")?);
			Ok(())
		}
	}

	#[test]
	fn subtraction_and_negation() -> Result<(), String> {
		assert_eq!(Value::from(3), eval(&mut Env::new(None), "-1+-2*-2")?);
		Ok(())
	}

	#[test]
	fn simple_compound_expression_with_parentheses() -> Result<(), String> {
		assert_eq!(Value::from(5), eval(&mut Env::new(None), "-5 *(1 +  -2)")?);
		Ok(())
	}

	mod mixed_domain_math {
		use super::*;
		#[test]
		fn real_literals_decay_to_integer() -> Result<(), String> {
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "3.0")?);
			assert_eq!(Value::from(4_000_000), eval(&mut Env::new(None), "4000000.0")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), String> {
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "1 + 2")?);
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "1.0 + 2")?);
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "1 + 2.0")?);
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "1.0 + 2.0")?);
			assert_eq!(Value::from(3.5), eval(&mut Env::new(None), "1 + 2.5")?);
			assert_eq!(Value::from(3.5), eval(&mut Env::new(None), "1.5 + 2")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), String> {
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "2 - 1")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "2.0 - 1")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "2 - 1.0")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "2.0 - 1.0")?);
			assert_eq!(Value::from(1.5), eval(&mut Env::new(None), "2.5 - 1")?);
			assert_eq!(Value::from(0.5), eval(&mut Env::new(None), "2 - 1.5")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), String> {
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 * 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 * 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 * 1.0")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 * 1.0")?);
			assert_eq!(Value::from(2.5), eval(&mut Env::new(None), "2.5 * 1")?);
			assert_eq!(Value::from(3), eval(&mut Env::new(None), "2 * 1.5")?);
			assert_eq!(Value::from(3.75), eval(&mut Env::new(None), "2.5 * 1.5")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), String> {
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 / 1")?);
			assert_eq!(Value::from(Number::rational(1, 2)), eval(&mut Env::new(None), "1 / 2")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "1 / 2 * 2")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 / 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 / 1.0")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 / 1.0")?);
			assert_eq!(Value::from(2.5), eval(&mut Env::new(None), "2.5 / 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "3 / 1.5")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "1.5 / 1.5")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), String> {
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 ^ 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 ^ 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2 ^ 1.0")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "2.0 ^ 1.0")?);
			assert_eq!(Value::from(2.5), eval(&mut Env::new(None), "2.5 ^ 1")?);
			assert_eq!(Value::from(2), eval(&mut Env::new(None), "3 ^ 1.5")?);
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "1.5 ^ 1.5")?);
			Ok(())
		}
	}

	#[test]
	fn exponentiation_is_right_associative() -> Result<(), String> {
		assert_eq!(Value::from(262144), eval(&mut Env::new(None), "4^3^2")?);
		Ok(())
	}

	#[test]
	fn basic_assignment() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, "let x = 42")?;
		assert_eq!(Value::from(42), eval(&mut env, "x")?);
		Ok(())
	}

	mod tuples {
		use super::*;
		#[test]
		fn singleton_collapses_into_contained_value() -> Result<(), String> {
			assert_eq!(Value::from(1), eval(&mut Env::new(None), "(1)")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers() -> Result<(), String> {
			let expected = make_tuple!(Value::from(1), make_tuple!(Value::from(2), Value::from(3)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1, (2, 3))")?);
			Ok(())
		}
		#[test]
		fn nested_tuple_of_numbers_and_booleans() -> Result<(), String> {
			let expected = make_tuple!(Value::from(true), make_tuple!(Value::from(2), Value::from(false)));
			assert_eq!(expected, eval(&mut Env::new(None), "(1 < 2, (2, false))")?);
			Ok(())
		}
	}

	mod list_construction {
		use super::*;
		#[test]
		fn singleton_list() -> Result<(), String> {
			assert_eq!(Value::List(make_list!(Value::from(1))), eval(&mut Env::new(None), "[1]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers() -> Result<(), String> {
			let expected = Value::List(make_list!(Value::from(1), Value::List(make_list!(Value::from(2), Value::from(3)))));
			assert_eq!(expected, eval(&mut Env::new(None), "[1, [2, 3]]")?);
			Ok(())
		}
		#[test]
		fn nested_list_of_numbers_and_booleans() -> Result<(), String> {
			let expected = Value::List(make_list!(Value::from(true), Value::List(make_list!(Value::from(2), Value::from(false)))));
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
		fn negation() -> Result<(), String> {
			let expected = Value::List(make_list!(Value::from(-1), Value::from(-2), Value::from(-3)));
			assert_eq!(expected, eval(&mut Env::new(None), "-[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn addition() -> Result<(), String> {
			let mut env = Env::new(None);
			let expected = Value::List(make_list!(Value::from(2), Value::from(3), Value::from(4)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3] + 1")?);
			assert_eq!(expected, eval(&mut env, "1 + [1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn subtraction() -> Result<(), String> {
			let mut env = Env::new(None);
			let expected = Value::List(make_list!(Value::from(1), Value::from(2), Value::from(3)));
			assert_eq!(expected, eval(&mut env, "[2, 3, 4]-1")?);
			assert_eq!(expected, eval(&mut env, "4-[3, 2, 1]")?);
			Ok(())
		}
		#[test]
		fn multiplication() -> Result<(), String> {
			let mut env = Env::new(None);
			let expected = Value::List(make_list!(Value::from(2), Value::from(4), Value::from(6)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 3]2")?);
			assert_eq!(expected, eval(&mut env, "2[1, 2, 3]")?);
			Ok(())
		}
		#[test]
		fn division() -> Result<(), String> {
			let mut env = Env::new(None);
			let expected = Value::List(make_list!(Value::from(1), Value::from(2), Value::from(3)));
			assert_eq!(expected, eval(&mut env, "[2, 4, 6]/2")?);
			assert_eq!(expected, eval(&mut env, "6/[6, 3, 2]")?);
			Ok(())
		}
		#[test]
		fn exponentiation() -> Result<(), String> {
			let mut env = Env::new(None);
			let expected = Value::List(make_list!(Value::from(1), Value::from(4), Value::from(16)));
			assert_eq!(expected, eval(&mut env, "[1, 2, 4]^2")?);
			assert_eq!(expected, eval(&mut env, "2^[0, 1, 4]")?);
			Ok(())
		}
	}

	#[test]
	fn simple_function_application() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, "let f = () -> 42")?;
		assert_eq!(Value::from(42), eval(&mut env, "f()")?);
		Ok(())
	}

	mod order_of_operations {
		use super::*;
		fn env_with_inc() -> Result<Rc<RefCell<Env>>, String> {
			let mut env = Env::new(None);
			exec(&mut env, "let inc = a -> a + 1")?;
			Ok(env)
		}
		#[test]
		fn parenthesized_function_call_before_exponentiation() -> Result<(), String> {
			assert_eq!(Value::from(36), eval(&mut env_with_inc()?, "4inc(2)^2")?);
			Ok(())
		}
		#[test]
		fn exponentiation_before_non_parenthesized_function_call() -> Result<(), String> {
			assert_eq!(Value::from(20), eval(&mut env_with_inc()?, "4inc 2^2")?);
			Ok(())
		}
	}

	#[test]
	fn higher_order_functions() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, "let apply = (f, a) -> f(a)")?;
		assert_eq!(Value::from(42), eval(&mut env, "apply(a -> a, 42)")?);
		Ok(())
	}

	#[test]
	fn curried_function() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, "let sum = a -> b -> a + b")?;
		assert_eq!(Value::from(3), eval(&mut env, "sum 1 2")?);
		Ok(())
	}

	#[test]
	fn environment_does_not_persist_between_function_chains() -> Result<(), String> {
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
	fn chained_application_with_and_without_parentheses() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let sum = a -> b -> a + b
			let inc = a -> a + 1
		")?;
		assert_eq!(Value::from(3), eval(&mut env, "sum (1) 2")?);
		Ok(())
	}

	#[test]
	fn chained_application_does_not_pollute_applications_higher_in_the_call_chain() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let sum = a -> b -> a + b
			let inc = b -> b + 1
		")?;
		assert_eq!(Value::from(8), eval(&mut env, "sum (inc 5) 2")?);
		Ok(())
	}

	#[test]
	fn importing_core_constants() -> Result<(), String> {
		let mut env = Env::new(None);
		let pi_str = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
		let expected = Value::Primitive(Primitive::Number(Number::Real(BigDecimal::from_str(pi_str).unwrap())));
		exec(&mut env, "import \"core/constants.gynj\"")?;
		assert_eq!(expected, eval(&mut env, "PI")?);
		Ok(())
	}

	#[test]
	fn blocks() -> Result<(), String> {
		let mut env = Env::new(None);
		eval(&mut env, "{}")?;
		eval(&mut env, "{ let a = 0 }")?;
		eval(&mut env, "{ let b = { let a = a + 1 return a } }")?;
		assert_eq!(Value::from(1), eval(&mut env, "b")?);
		Ok(())
	}

	mod branch_statements {
		use super::*;
		#[test]
		fn true_is_lazy() -> Result<(), String> {
			let mut env = Env::new(None);
			exec(&mut env, "if false then let a = 1/0 else let a = 1")?;
			assert_eq!(Value::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn false_is_lazy() -> Result<(), String> {
			let mut env = Env::new(None);
			exec(&mut env, "if true then let a = 1 else let a = 1/0")?;
			assert_eq!(Value::from(1), eval(&mut env, "a")?);
			Ok(())
		}
		#[test]
		fn no_else_statement() -> Result<(), String> {
			assert_eq!((), exec(&mut Env::new(None), "if false then let a = 1/0")?);
			Ok(())
		}
	}

	#[test]
	fn while_loops() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let a = 0
			while a < 3 do let a = a + 1
		")?;
		assert_eq!(Value::from(3), eval(&mut env, "a")?);
		Ok(())
	}

	#[test]
	fn for_loops() -> Result<(), String> {
		let mut env = Env::new(None);
		exec(&mut env, r"
			let a = 0
			for x in [1, 2, 3] do let a = a + x
			for x in [] do let a = 10
		")?;
		assert_eq!(Value::from(6), eval(&mut env, "a")?);
		Ok(())
	}

	mod intrinsics {
		use super::*;
		#[test]
		fn top() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(1), eval(&mut env, "top([1])")?);
			assert!(eval(&mut env, "top([])").is_err());
			Ok(())
		}
		#[test]
		fn pop() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::List(make_list!()), eval(&mut env, "pop([1])")?);
			assert!(eval(&mut env, "pop([])").is_err());
			Ok(())
		}
		#[test]
		fn push() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::List(make_list!(Value::from(1))), eval(&mut env, "push([], 1)")?);
			assert_eq!(Value::List(make_list!(Value::from(2), Value::from(1))), eval(&mut env, "push([1], 2)")?);
			Ok(())
		}
		#[test]
		fn to_real() -> Result<(), String> {
			let mut env = Env::new(None);
			assert_eq!(Value::from(1.0), eval(&mut env, "real(1)")?);
			assert_eq!(Value::from(0.5), eval(&mut env, "real(1/2)")?);
			Ok(())
		}
	}
}
