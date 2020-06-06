use super::exprs::{Expr, BinExpr, BinOp, Cluster, ClusterItem, ClusterConnector, Lambda, LambdaBody};
use super::intrinsics::Intrinsic;
use super::primitives::Prim;
use super::stmts::Stmt;
use super::symbol::Sym;
use super::tokens::Tok;

use std::collections::HashMap;
use std::collections::VecDeque;

/// Result of expression parsing: (remaining tokens, parsed expression).
type ParseExprResult<'a> = Result<(&'a [Tok], Expr), String>;

/// Result of statement parsing: (remaining tokens, parsed statement).
type ParseStmtResult<'a> = Result<(&'a [Tok], Stmt), String>;

/// If possible, parses the next single expression from `tokens`.
/// Returns a slice of the unused tokens along with the parsed expression.
pub fn parse_expr(tokens: &[Tok]) -> ParseExprResult {
	let (tokens, negation_result) = parse_negation(&tokens)?;
	// Check for conditional expression.
	let (tokens, is_conditional_expr) = parse_optional_token(&tokens, &Tok::Question);
	if is_conditional_expr {
		// Parse expression if true.
		let (tokens, then_expr) = parse_expr(tokens).map_err(|_| "expected true case in conditional expression")?;
		// Parse ":".
		let tokens = parse_required_token(&tokens, &Tok::Colon, "conditional expression")?;
		// Parse expression if false.
		let (tokens, else_expr) = parse_expr(tokens).map_err(|_| "expected false case in conditional expression")?;
		Ok((tokens, Expr::Cond { test: Box::new(negation_result), then_expr: Box::new(then_expr), else_expr: Box::new(else_expr) }))
	} else {
		Ok((tokens, negation_result))
	}
}

/// If possible, parses the next single statement from `tokens`.
/// Returns an iterator to the next unused token along with the parsed statement, or an error message.
pub fn parse_stmt<'a>(tokens: &'a [Tok]) -> ParseStmtResult {
	match tokens {
		[] => Ok((tokens, Stmt::Nop)),
		[Tok::Import, rest @ ..] => parse_import(rest),
		[Tok::Let, rest @ ..] => parse_assignment(rest),
		[Tok::If, rest @ ..] => parse_branch(rest),
		[Tok::While, rest @ ..] => parse_while_loop(rest),
		[Tok::For, rest @ ..] => parse_for_loop(rest),
		[Tok::Return, rest @ ..] => parse_ret(rest),
		_ => {
			let (tokens, expr) = parse_expr(tokens)?;
			let tokens = parse_required_token(&tokens, &Tok::Semicolon, "expression statement")?;
			Ok((tokens, Stmt::ExprStmt(Box::new(expr))))
		},
	}
}

/// Parses a function body.
fn parse_body(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[Tok::Arrow, rest @ ..] => parse_expr(rest),
		_ => Err("expected function body".to_string()),
	}
}

/// Parses a Gynjo value.
fn parse_value(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Err("expected value".to_string()),
		// Tuple or lambda
		[Tok::Lparen, after_lparen @ ..] => {
			let mut elems: Vec<Expr> = Vec::new();
			// Parse contained expressions.
			let after_tuple = match parse_expr(after_lparen) {
				// Found at least one expression.
				Ok((after_first_elem, first_elem)) => {
					elems.push(first_elem);
					// Try to parse additional comma-delimited expressions.
					let mut tokens = after_first_elem;
					loop {
						match tokens {
							[] => return Err("expected ',' or ')' in tuple expression".to_string()),
							// Additional tuple element
							[Tok::Comma, after_comma @ ..] => {
								tokens = after_comma;
								let (after_next_elem, next_elem) = parse_expr(tokens).map_err(|_| "expected expression after ',' in tuple expression")?;
								tokens = after_next_elem;
								elems.push(next_elem);
							},
							// End of tuple
							[Tok::Rparen, rest @ ..] => {
								tokens = rest;
								break;
							},
							[invalid @ _, ..] => return Err(format!("expected ',' or ')' in tuple expression, found {}", invalid.to_string())),
						}
					}
					tokens
				},
				// Empty tuple; parse close parenthesis.
				_ => parse_required_token(&tokens[1..], &Tok::Rparen, "tuple expression")?,
			};
			// Try to parse this expression as a lambda.
			let mut params = Vec::with_capacity(elems.len());
			let mut could_be_lambda = true;
			for expr in elems.iter() {
				match expr {
					Expr::Sym(param) => params.push(param.clone()),
					_ => {
						could_be_lambda = false;
						break;
					},
				}
			}
			if could_be_lambda {
				// Try to parse a lambda body.
				if let Ok((after_body, body_expr)) = parse_body(after_tuple) {
					// Assemble lambda from parameter tuple and body.
					return Ok((after_body, Expr::Lambda(Lambda {
						params: params,
						body: LambdaBody::UserDefined(Box::new(body_expr)),
					})));
				}
			}
			// Collapse singletons back into their contained values. This allows use of parentheses for
			// value grouping without having to special-case interpretation when an argument is a singleton.
			if elems.len() == 1 {
				// Extract singleton element.
				Ok((after_tuple, elems.remove(0)))
			} else {
				// Return unmodified tuple.
				Ok((after_tuple, Expr::TupleExpr(Box::new(elems))))
			}
		},
		// List
		[Tok::Lsquare, after_lsquare @ ..] => {
			let mut tokens = after_lsquare;
			let mut elems = Box::new(VecDeque::new());
			// Try to parse an expression.
			if let Ok((after_first_elem, first_elem)) = parse_expr(tokens) {
				tokens = after_first_elem;
				elems.push_front(first_elem);
				// Try to parse additional comma-delimited expressions.
				loop {
					match tokens {
						[] => return Err("expected ',' or ']' in list expression".to_string()),
						// Additional list element
						[Tok::Comma, after_comma @ ..] => {
							tokens = after_comma;
							let (after_next_elem, next_elem) = parse_expr(tokens).map_err(|_| "expected expression after ',' in list expression")?;
							tokens = after_next_elem;
							elems.push_front(next_elem);
						},
						// End of list
						[Tok::Rsquare, rest @ ..] => {
							tokens = rest;
							break;
						},
						[invalid @ _, ..] => return Err(format!("expected ',' or ']' in list expression, found {}", invalid.to_string())),
					}
				}
			} else {
				// Parse close square bracket of empty list.
				tokens = parse_required_token(&tokens, &Tok::Rsquare, "list expression")?;
			}
			Ok((tokens, Expr::ListExpr(elems)))
		},
		// Block
		[Tok::Lcurly, after_lcurly @ ..] => {
			let mut tokens = after_lcurly;
			let mut stmts = Box::new(Vec::new());
			// Parse statements.
			loop {
				if let Ok((after_stmt, stmt)) = parse_stmt(tokens) {
					tokens = after_stmt;
					stmts.push(stmt);
				} else {
					break;
				}
			}
			// Parse close curly brace.
			tokens = parse_required_token(&tokens, &Tok::Rcurly, "block")?;
			return Ok((tokens, Expr::Block { stmts: stmts }));
		},
		// Intrinsic function
		[Tok::Intrinsic(f), tokens @ ..] => {
			let params = match f {
				Intrinsic::Top | Intrinsic::Pop => vec!(Sym::from("list")),
				Intrinsic::Push => vec!(Sym::from("list"), Sym::from("value")),
				Intrinsic::Print => vec!(Sym::from("value")),
				Intrinsic::Read => vec!(),
				Intrinsic::ToReal => vec!(Sym::from("value")),
			};
			Ok((tokens, Expr::Lambda(Lambda { params, body: LambdaBody::Intrinsic(f.clone()) })))
		},
		// Symbol or lambda
		[Tok::Sym(symbol), tokens @ ..] => {
			// Could be a parentheses-less unary lambda. Try to parse a lambda body.
			if let Ok((tokens, body_expr)) = parse_body(tokens) {
				// Assemble lambda from the parameter wrapped in a tuple and the body.
				Ok((tokens, Expr::Lambda(Lambda {
					params: vec!(symbol.clone()),
					body: LambdaBody::UserDefined(Box::new(body_expr)),
				})))
			} else {
				// It's just a symbol.
				Ok((tokens, Expr::Sym(symbol.clone())))
			}
		},
		// Primitive
		[Tok::Prim(primitive), tokens @ ..] => Ok((tokens, Expr::Prim(primitive.clone()))),
		[t, ..] => Err(format!("unexpected token in expression: {}", t.to_string())),
	}
}

/// Parses a single `required` token from `tokens` and returns the remaining tokens.
/// `context` - The expression in which the token is required, for the purpose of error reporting.
fn parse_required_token<'a>(tokens: &'a [Tok], required: &Tok, context: &'static str) -> Result<&'a [Tok], String> {
	match tokens {
		[] => Err(format!("expected '{}' in {}", required.to_string(), context)),
		[t, rest @ ..] if (t == required) => Ok(rest),
		[invalid @ _, ..] => Err(format!("expected '{}' in {}, found {}", required.to_string(), context, invalid.to_string())),
	}
}

/// Parses a single `optional` token from `tokens` and returns the remaining tokens and whether the token was parsed.
fn parse_optional_token<'a>(tokens: &'a [Tok], optional: &Tok) -> (&'a [Tok], bool) {
	match tokens {
		[t, rest @ ..] if (t == optional) => (rest, true),
		_ => (tokens, false),
	}
}

/// Parses a cluster item after an operator or parenthesis.
fn parse_mandatory_cluster_item(tokens: &[Tok], connector: ClusterConnector) -> Result<(&[Tok], ClusterItem), String> {
	let (after_minus, negated) = parse_optional_token(tokens, &Tok::Minus);
	let (after_expr, expr) = parse_value(after_minus)?;
	Ok((after_expr, ClusterItem {
		expr: Box::new(expr),
		negated: negated,
		connector: connector,
	}))
}

/// Parses a cluster of function calls, exponentiations, (possibly implicit) multiplications,
/// divisions and/or negations. The result is something that will require further parsing by
/// the interpreter once the required semantic info is available.
fn parse_cluster(tokens: &[Tok]) -> ParseExprResult {
	// Parse the first cluster item.
	let (tokens, cluster_negated) = parse_optional_token(tokens, &Tok::Minus);
	let (tokens, first_expr) = parse_value(tokens)?;
	let mut items = vec!(ClusterItem {
		expr: Box::new(first_expr),
		negated: false,
		connector: ClusterConnector::None,
	});
	// Parse subsequent cluster items.
	let mut tokens = tokens;
	loop {
		match tokens {
			[] => break,
			[Tok::Mul, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Mul)?;
				tokens = after_item;
				items.push(item);
			},
			[Tok::Div, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Div)?;
				tokens = after_item;
				items.push(item);
			},
			[Tok::Exp, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Exp)?;
				tokens = after_item;
				items.push(item);
			},
			[Tok::Lparen, ..] => {
				// Don't consume the left parenthesis.
				let (after_item, item) = parse_mandatory_cluster_item(tokens, ClusterConnector::AdjParen)?;
				tokens = after_item;
				items.push(item);
			},
			_ => {
				// Try to parse an adjacent (non-negated) value, but it's not required.
				if let Ok((after_expr, expr)) = parse_value(tokens) {
					tokens = after_expr;
					items.push(ClusterItem {
						expr: Box::new(expr),
						negated: false,
						connector: ClusterConnector::AdjNonparen,
					});
				} else {
					break;
				}
			},
		}
	}
	if items.len() == 1 && !cluster_negated {
		// Found a single non-negated value. Just extract it here.
		Ok((tokens, *items.remove(0).expr))
	} else {
		Ok((tokens, Expr::Cluster(Cluster { negated: cluster_negated, items })))
	}
}

/// Parses a series of left-associative binary expressions with equal precedence, using `subparse` to parse each operand.
fn parse_binary_expressions<'a>(tokens: &'a [Tok], subparse: fn(&'a [Tok]) -> ParseExprResult, op_map: &HashMap<Tok, BinOp>) -> ParseExprResult<'a> {
	// Parse first expression.
	let (mut tokens, mut exprs) = subparse(tokens)?;
	// Parse subsequent expressions.
	loop {
		match tokens.first() {
			Some(token) => match op_map.get(token) {
				Some(op) => {
					let (after_expr, next_expr) = subparse(&tokens[1..])
						.map_err(|_| format!("expected operand after '{}'", op.to_string()))?;
					tokens = after_expr;
					// Incorporate next expression into expression.
					exprs = Expr::BinaryExpr(BinExpr {
						op: *op,
						left: Box::new(exprs),
						right: Box::new(next_expr),
					});
				},
				None => break,
			},
			None => break,
		};
	}
	Ok((tokens, exprs))
}

/// Parses a series of additions and subtractions.
fn parse_terms(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_cluster,
		&[(Tok::Plus, BinOp::Add), (Tok::Minus, BinOp::Sub)].iter().cloned().collect())
}

/// Parses a series of comparison checks (not including equality, inequality, or approximate equality).
fn parse_comparisons(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_terms,
		&[(Tok::Lt, BinOp::Lt), (Tok::Leq, BinOp::Leq), (Tok::Gt, BinOp::Gt), (Tok::Geq, BinOp::Geq)].iter().cloned().collect())
}

/// Parses a series of equality, inequality, or approximate equality checks.
fn parse_eq_checks(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_comparisons,
		&[(Tok::Eq, BinOp::Eq), (Tok::Neq, BinOp::Neq), (Tok::Approx, BinOp::Approx)].iter().cloned().collect())
}

/// Parses a series of logical conjunctions.
fn parse_conjunctions(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_eq_checks, &[(Tok::And, BinOp::And)].iter().cloned().collect())
}

/// Parses a series of logical disjunctions.
fn parse_disjunctions(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_conjunctions, &[(Tok::Or, BinOp::Or)].iter().cloned().collect())
}

/// Parses a logical negation. Note that negation is right-associative.
fn parse_negation(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Err("expected expression".to_string()),
		[Tok::Not, tokens @ ..] => {
			let (tokens, expr) = parse_negation(tokens)?;
			Ok((tokens, Expr::Not { expr: Box::new(expr) }))
		},
		_ => parse_disjunctions(tokens),
	}
}

/// Parses a return statement, starting after "return".
fn parse_ret(tokens: &[Tok]) -> ParseStmtResult {
	let (tokens, expr) = parse_expr(tokens).map_err(|_| "expected return expression")?;
	Ok((tokens, Stmt::Return { result: Box::new(expr) }))
}

/// Parses a for-loop, starting after "for".
fn parse_for_loop(tokens: &[Tok]) -> ParseStmtResult {
	match tokens {
		// Parse loop variable and "in".
		[Tok::Sym(loop_var), Tok::In, tokens @ ..] => {
			// Parse range expression.
			let (tokens, range) = parse_expr(tokens).map_err(|_| "expected range expression in for-loop")?;
			// Parse "do".
			let tokens = parse_required_token(tokens, &Tok::Do, "for-loop")?;
			// Parse body.
			let (tokens, body) = parse_stmt(tokens)?;
			// Assemble for-loop.
			Ok((tokens, Stmt::ForLoop { loop_var: loop_var.clone(), range: Box::new(range), body: Box::new(body) }))
		},
		_ => Err("expected loop variable followed by \"in\" in for-loop".to_string()),
	}
}

/// Parses a while-loop, starting after "while".
fn parse_while_loop(tokens: &[Tok]) -> ParseStmtResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens).map_err(|_| "expected test expression in while-loop")?;
	// Parse "do".
	let tokens = parse_required_token(&tokens, &Tok::Do, "while-loop")?;
	// Parse body.
	let (tokens, body_stmt) = parse_stmt(tokens).map_err(|_| "expected body in while-loop")?;
	// Assemble while-loop.
	Ok((tokens, Stmt::WhileLoop {
		test: Box::new(test_expr),
		body: Box::new(body_stmt),
	}))
}

/// Parses a branch statment - if-then or if-then-else - starting after "if".
fn parse_branch(tokens: &[Tok]) -> ParseStmtResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens).map_err(|_| "expected test expression in branch statment")?;
	// Parse "then".
	let tokens = parse_required_token(&tokens, &Tok::Then, "branch statement")?;
	// Parse "then" branch.
	let (tokens, then_stmt) = parse_stmt(&tokens).map_err(|_| "expected true case in branch statement")?;
	// Check for "else" branch.
	let (tokens, has_else_branch) = parse_optional_token(&tokens, &Tok::Else);
	let (tokens, else_stmt) = if has_else_branch {
		// Parse "else" branch.
		parse_stmt(tokens).map_err(|_| "expected false case in branch statement")?
	} else {
		// Missing else statement is a no-op.
		(tokens, Stmt::Nop)
	};
	// Assemble branch statement.
	Ok((tokens, Stmt::Branch {
		test: Box::new(test_expr),
		then_stmt: Box::new(then_stmt),
		else_stmt: Box::new(else_stmt),
	}))
}

/// Parses an assignment operation, starting after "let".
fn parse_assignment(tokens: &[Tok]) -> ParseStmtResult {
	// Parse LHS.
	match tokens {
		[] => Err("expected assignment".to_string()),
		[Tok::Sym(lhs), after_symbol @ ..] => {
			// Parse "=".
			let after_eq = parse_required_token(&after_symbol, &Tok::Eq, "assignment")?;
			// Parse RHS.
			let (after_rhs, rhs) = parse_expr(after_eq)?;
			// Assemble assignment from symbol and RHS.
			Ok((after_rhs, Stmt::Assign { lhs: lhs.clone(), rhs: Box::new(rhs) }))
		},
		[invalid @ _, ..] => Err(format!("expected loop variable in for-loop, found {}", invalid.to_string())),
	}
}

/// Parses an import statement, starting after "import".
fn parse_import(tokens: &[Tok]) -> ParseStmtResult {
	match tokens {
		[] => Err("expected import target".to_string()),
		[Tok::Sym(Sym { name }), tokens @ ..] => Ok((tokens, Stmt::Import { filename : name.clone() })),
		[Tok::Prim(Prim::String(filename)), tokens @ ..] => Ok((tokens, Stmt::Import { filename: filename.clone() })),
		[first, ..] => Err(format!("expected filename (symbol or string) in import statement, found {}", first.to_string())),
	}
}
