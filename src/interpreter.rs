use super::env::Env;
use super::exprs::{Expr, BinaryOp, Cluster, ClusterConnector, LambdaBody};
use super::intrinsics::Intrinsic;
use super::lexer::lex;
use super::primitives::{Primitive, Boolean};
use super::parser::{parse_expr, parse_stmt};
use super::stmts::Stmt;
use super::values::{Closure, Tuple, List, Value};

use bigdecimal::BigDecimal;

use std::io;
use std::rc::Rc;

/// Result of evaluating a Gynjo expression.
type EvalResult = Result<Value, String>;

/// Result of executing a Gynjo statement.
type ExecResult = Result<(), String>;

/// If possible, computes the value of `expr` in the context of `env`.
pub fn eval_expr(mut env: &mut Rc<Env>, expr: Expr) -> EvalResult {
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
				bin_num_op(&env, left, right, "comparison", |a, b| -> EvalResult { Ok(Boolean::from(a < b).into()) })
			},
			BinaryOp::Leq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| -> EvalResult { Ok(Boolean::from(a <= b).into()) })
			},
			BinaryOp::Gt => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| -> EvalResult { Ok(Boolean::from(a > b).into()) })
			},
			BinaryOp::Geq => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "comparison", |a, b| -> EvalResult { Ok(Boolean::from(a >= b).into()) })
			},
			BinaryOp::Add => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "addition", |a, b| -> EvalResult { Ok(Value::Primitive(Primitive::Number(a + b))) })
			},
			BinaryOp::Sub => {
				let left = eval_expr(&mut env, *bin_expr.left)?;
				let right = eval_expr(&mut env, *bin_expr.right)?;
				bin_num_op(&env, left, right, "subtraction", |a, b| -> EvalResult { Ok(Value::Primitive(Primitive::Number(a - b))) })
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
		Expr::Symbol(symbol) => env.lookup(&symbol)
			.map(|v| v.clone())
			.ok_or(format!("'{}' is undefined", symbol.name)),
		Expr::Primitive(primitive) => Ok(Value::Primitive(primitive)),
	}
}

/// If possible, computes the value of the expression contained in `input` in the context of `env`.
pub fn eval(env: &mut Rc<Env>, input: &str) -> EvalResult {
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
pub fn exec_stmt(mut env: &mut Rc<Env>, stmt: Stmt) -> ExecResult {
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
			env.assign(lhs, rhs_value);
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
						env.assign(loop_var.clone(), value.clone());
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
pub fn exec(env: &mut Rc<Env>, input: &str) -> ExecResult {
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
fn list_num_op(env: &Rc<Env>, list: List, number: BigDecimal, number_on_left: bool, op_name: &str, op: fn(BigDecimal, BigDecimal) -> EvalResult) -> Result<List, String> {
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
fn bin_num_op(env: &Rc<Env>, left: Value, right: Value, op_name: &str, op: fn(BigDecimal, BigDecimal) -> EvalResult) -> EvalResult {
	match (left, right) {
		// Number op Number
		(Value::Primitive(Primitive::Number(left)), Value::Primitive(Primitive::Number(right))) => op(left, right),
		// List op Number
		(Value::List(list), Value::Primitive(Primitive::Number(number))) => Ok(Value::List(list_num_op(env, list, number, false, op_name, op)?)),
		// Number op List
		(Value::Primitive(Primitive::Number(number)), Value::List(list)) => Ok(Value::List(list_num_op(env, list, number, true, op_name, op)?)),
		// Invalid numeric operation
		(left @ _, right @ _) => Err(format!("cannot perform {} with {} and {}", op_name, left.to_string(&env), right.to_string(&env))),
	}
}

/// These are cluster sub-expressions that can only be fully parsed once their value types are known.
enum LateExpr {
	Value(Value),
	Apply(Closure, Value),
	Exponentiate(Value, Value),
	Multiply(Value, Value),
	Divide(Value, Value),
}

/// An evaluated cluster item.
struct EvaluatedClusterItem {
	/// This item's value.
	value: Value,
	/// How this item is connected to the previous item.
	connector: ClusterConnector,
}

type LateParseResult<'a> = Result<(&'a [EvaluatedClusterItem], LateExpr), String>;

fn parse_multiplications_and_divisions(items: &[EvaluatedClusterItem]) -> LateParseResult {
	Ok((items, LateExpr::Value(Value::Tuple(Tuple::empty()))))
}

fn parse_non_parenthesized_applications(items: &[EvaluatedClusterItem]) -> LateParseResult {
	Ok((items, LateExpr::Value(Value::Tuple(Tuple::empty()))))
}

fn parse_exponentiations(items: &[EvaluatedClusterItem]) -> LateParseResult {
	Ok((items, LateExpr::Value(Value::Tuple(Tuple::empty()))))
}

fn parse_parenthesized_applications(items: &[EvaluatedClusterItem]) -> LateParseResult {
	Ok((items, LateExpr::Value(Value::Tuple(Tuple::empty()))))
}

fn eval_late_expr(env: &Rc<Env>, late_expr: LateExpr) -> EvalResult {
	match late_expr {
		LateExpr::Value(value) => Ok(value),
		LateExpr::Apply(closure, args) => {
			match args {
				// Argument is already a tuple.
				Value::Tuple(tuple) => eval_application(closure, *tuple.elems),
				// Wrap argument in a tuple.
				_ => eval_application(closure, vec!(args)),
			}
		},
		LateExpr::Exponentiate(left, right) => bin_num_op(env, left, right, "exponentiation", |_, _| -> EvalResult {
			Err("BigDecimal does not support exponentiation yet".to_string())
		}),
		LateExpr::Multiply(left, right) => bin_num_op(env, left, right, "multiplication", |a, b| -> EvalResult {
			Ok(Value::Primitive(Primitive::Number(a * b)))
		}),
		LateExpr::Divide(left, right) => bin_num_op(env, left, right, "multiplication", |a, b| -> EvalResult {
			if b == BigDecimal::from(0) {
				Err("division by zero".to_string())
			} else {
				Ok(Value::Primitive(Primitive::Number(a / b)))
			}
		}),
	}
}

/// Evaluates a cluster expression.
fn eval_cluster(env: &mut Rc<Env>, cluster: Cluster) -> EvalResult {
	// First, evaluate the cluster items.
	let mut evaluated_cluster: Vec<EvaluatedClusterItem> = Vec::with_capacity(cluster.items.len());
	for item in cluster.items {
		let value = eval_expr(env, *item.expr)?;
		evaluated_cluster.push(EvaluatedClusterItem {
			value: if item.negated { eval_negation(env, &value)? } else { value },
			connector: item.connector,
		});
	}
	// Now that the items' types are known, fully resolve the parse tree.
	let (_, late_expr) = parse_multiplications_and_divisions(&evaluated_cluster[..])?;
	// Evaluate parse tree.
	eval_late_expr(env, late_expr)
}

/// Evaluates a closure application.
fn eval_application(c: Closure, args: Vec<Value>) -> EvalResult {
	// Ensure correct number of arguments.
	if args.len() != c.f.params.len() {
		return Err(format!("function requires {} argument{}, received {}", c.f.params.len(), if c.f.params.len() == 1 { "" } else { "s" }, args.len()));
	}
	// Assign arguments to parameters within a copy of the closure's environment.
	let mut local_env = Env::new(Some(c.env.clone()));
	for (variable, value) in c.f.params.into_iter().zip(args.into_iter()) {
		local_env.assign(variable, value);
	}
	let mut local_env = Rc::new(local_env);
	// Evaluate function body within the application environment.
	match c.f.body {
		LambdaBody::UserDefined(body) => eval_expr(&mut local_env, *body),
		LambdaBody::Intrinsic(body) => {
			match body {
				Intrinsic::Top => match local_env.lookup(&"list".into()).unwrap() {
					Value::List(List::Cons { head, .. }) => Ok((**head).clone()),
					arg @ _ => Err(format!("top() expected a non-empty list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Pop => match local_env.lookup(&"list".into()).unwrap() {
					Value::List(List::Cons { tail, .. }) => Ok(Value::List((**tail).clone())),
					arg @ _ => Err(format!("pop() expected a non-empty list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Push => match local_env.lookup(&"list".into()).unwrap() {
					Value::List(list) => {
						let value = local_env.lookup(&"value".into()).unwrap().clone();
						Ok(Value::List(List::Cons { head: Box::new(value), tail: Rc::new(list.clone()) }))
					},
					arg @ _ => Err(format!("push() expected a list, found {}", arg.to_string(&local_env))),
				},
				Intrinsic::Print => {
					println!("{}", local_env.lookup(&"value".into()).unwrap().to_string(&local_env));
					Ok(Value::Tuple(Tuple::empty()))
				},
				Intrinsic::Read => {
					let mut input = String::new();
					io::stdin().read_line(&mut input);
					Ok(Value::Primitive(Primitive::String(input)))
				}
			}
		}
	}
}

fn eval_negation(env: &Rc<Env>, value: &Value) -> EvalResult {
	match value {
		Value::Primitive(Primitive::Number(number)) => Ok(Value::Primitive(Primitive::Number(-number))),
		_ => Err(format!("cannot negate {}", value.to_string(env))),
	}
}
