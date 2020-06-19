use super::errors::ParseError;
use super::exprs::{Expr, BinExpr, BinOp, Cluster, ClusterItem, ClusterConnector, Lambda, LambdaBody};
use super::intrinsics::Intrinsic;
use super::symbol::Sym;
use super::tokens::Tok;

use std::collections::HashMap;
use std::collections::VecDeque;

/// Result of expression parsing: (remaining tokens, parsed expression).
type ParseExprResult<'a> = Result<(&'a [Tok], Expr), ParseError>;

/// Parses an expression from `tokens`. Requires all tokens to be consumed.
pub fn parse(tokens: &[Tok]) -> Result<Expr, ParseError> {
	let (tokens, expr) = parse_expr(&tokens[..])?;
	if tokens.is_empty() {
		Ok(expr)
	} else {
		Err(ParseError::UnusedInput { first_unused: tokens.first().unwrap().clone() })
	}
}

/// Parses a single `required` token from `tokens` and returns the remaining tokens.
/// `context` - The expression in which the token is required, for the purpose of error reporting.
fn parse_required_token<'a>(tokens: &'a [Tok], required: &Tok, context: &'static str) -> Result<&'a [Tok], ParseError> {
	match tokens {
		[] => Err(ParseError::EndOfInput {
			context,
			expected: format!("\"{}\"", required),
		}),
		[t, rest @ ..] if (t == required) => Ok(rest),
		[invalid @ _, ..] => Err(ParseError::InvalidInput {
			context,
			expected: Some(format!("\"{}\"", required)),
			actual: invalid.clone(),
		}),
	}
}

/// Parses a single `optional` token from `tokens` and returns the remaining tokens and whether the token was parsed.
fn parse_optional_token<'a>(tokens: &'a [Tok], optional: &Tok) -> (&'a [Tok], bool) {
	match tokens {
		[t, rest @ ..] if (t == optional) => (rest, true),
		_ => (tokens, false),
	}
}

/// If possible, parses the next single expression from `tokens`.
/// Returns a slice of the unused tokens along with the parsed expression.
fn parse_expr(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Ok((tokens, Expr::TupleExpr(Box::new(Vec::new())))),
		[Tok::Import, rest @ ..] => parse_import(rest),
		[Tok::Let, rest @ ..] => parse_assignment(rest),
		[Tok::If, rest @ ..] => parse_branch(rest),
		[Tok::While, rest @ ..] => parse_while_loop(rest),
		[Tok::For, rest @ ..] => parse_for_loop(rest),
		[Tok::Return, rest @ ..] => parse_ret(rest),
		_ => parse_logical_negation(tokens),
	}
}

/// Parses a function body.
fn parse_body(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Err(ParseError::EndOfInput { context: "lambda", expected: "function body".into() }),
		[Tok::Arrow, rest @ ..] => parse_expr(rest),
		[invalid @ _, ..] => Err(ParseError::InvalidInput {
			context: "lambda",
			expected: Some("function body".into()),
			actual: invalid.clone(),
		}),
	}
}

/// Parses a Gynjo value.
fn parse_value(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Err(ParseError::EndOfInput {
			context: "value expression",
			expected: "value expression".into(),
		}),
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
							[] => return Err(ParseError::EndOfInput {
								context: "tuple expression",
								expected: "',' or ')'".into(),
							}),
							// Additional tuple element
							[Tok::Comma, after_comma @ ..] => {
								tokens = after_comma;
								let (after_next_elem, next_elem) = parse_expr(tokens)?;
								tokens = after_next_elem;
								elems.push(next_elem);
							},
							// End of tuple
							[Tok::Rparen, rest @ ..] => {
								tokens = rest;
								break;
							},
							[invalid @ _, ..] => return Err(ParseError::InvalidInput {
								context: "tuple expression",
								expected: Some(r#""," or ")""#.into()),
								actual: invalid.clone(),
							}),
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
						[] => return Err(ParseError::EndOfInput {
							context: "list expression",
							expected: r#"," or "]""#.into(),
						}),
						// Additional list element
						[Tok::Comma, after_comma @ ..] => {
							tokens = after_comma;
							let (after_next_elem, next_elem) = parse_expr(tokens)?;
							tokens = after_next_elem;
							elems.push_front(next_elem);
						},
						// End of list
						[Tok::Rsquare, rest @ ..] => {
							tokens = rest;
							break;
						},
						[invalid @ _, ..] => return Err(ParseError::InvalidInput {
							context: "list expression",
							expected: Some(r#""," or ")""#.into()),
							actual: invalid.clone(),
						}),
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
			let mut exprs = Box::new(Vec::new());
			// Parse expressions.
			let mut final_semicolon = false;
			loop {
				match tokens {
					[] => return Err(ParseError::EndOfInput {
						context: "block",
						expected: r#""," or "}""#.into(),
					}),
					[Tok::Semicolon, after_semicolon @ ..] => {
						// Semicolons optionally separate block expressions.
						tokens = after_semicolon;
						final_semicolon = true;
					},
					[Tok::Rcurly, after_rclurly @ ..] => {
						// End of block. If block ended with a semicolon, the result is ().
						if final_semicolon {
							exprs.push(Expr::TupleExpr(Box::new(Vec::new())));
						}
						return Ok((after_rclurly, Expr::Block { exprs }))
					},
					_ => {
						let (after_expr, expr) = parse_expr(tokens)?;
						tokens = after_expr;
						exprs.push(expr);
						final_semicolon = false;
					},
				}
			}
		},
		// Intrinsic function
		[Tok::Intrinsic(f), tokens @ ..] => {
			let params = match f {
				Intrinsic::Pop => vec!(Sym::from("list")),
			};
			Ok((tokens, Expr::Lambda(Lambda { params, body: LambdaBody::Intrinsic(*f) })))
		},
		// I/O
		[Tok::Read, tokens @ ..] => Ok((tokens, Expr::Read)),
		[Tok::Write, tokens @ ..] => {
			let (tokens, output) = parse_expr(tokens)?;
			Ok((tokens, Expr::Write { output: Box::new(output) }))
		},
		// Get type
		[Tok::GetType, tokens @ ..] => {
			let (tokens, expr) = parse_expr(tokens)?;
			Ok((tokens, Expr::GetType { expr: Box::new(expr) }))
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
		// Invalid
		[invalid, ..] => Err(ParseError::InvalidInput {
			context: "value expression",
			expected: None,
			actual: invalid.clone(),
		}),
	}
}

/// Parses a cluster item after an operator or parenthesis.
fn parse_mandatory_cluster_item(tokens: &[Tok], connector: ClusterConnector) -> Result<(&[Tok], ClusterItem), ParseError> {
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
					let (after_expr, next_expr) = subparse(&tokens[1..])?;
					tokens = after_expr;
					// Incorporate next expression into expression.
					exprs = Expr::BinExpr(BinExpr {
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

/// Parses a series of concatenations.
fn parse_concatenations(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_cluster, &[(Tok::Concat, BinOp::Concat)].iter().cloned().collect())
}

/// Parses a series of additions and subtractions.
fn parse_terms(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_concatenations,
		&[(Tok::Plus, BinOp::Add), (Tok::Minus, BinOp::Sub)].iter().cloned().collect())
}

/// Parses a series of type casts.
fn parse_type_casts(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_terms, &[(Tok::As, BinOp::As)].iter().cloned().collect())
}

/// Parses a series of comparison checks (not including equality, inequality, or approximate equality).
fn parse_comparisons(tokens: &[Tok]) -> ParseExprResult {
	parse_binary_expressions(tokens, parse_type_casts,
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
fn parse_logical_negation(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[Tok::Not, tokens @ ..] => {
			let (tokens, expr) = parse_logical_negation(tokens)?;
			Ok((tokens, Expr::Not { expr: Box::new(expr) }))
		},
		_ => parse_disjunctions(tokens),
	}
}

/// Parses a return statement, starting after "return".
fn parse_ret(tokens: &[Tok]) -> ParseExprResult {
	let (tokens, expr) = parse_expr(tokens)?;
	Ok((tokens, Expr::Return { result: Box::new(expr) }))
}

/// Parses a for-loop, starting after "for".
fn parse_for_loop(tokens: &[Tok]) -> ParseExprResult {
	match tokens {
		[] => Err(ParseError::EndOfInput {
			context: "for-loop",
			expected: "loop variable".into(),
		}),
		[Tok::Sym(loop_var), tokens @ ..] => {
			// Parse "in".
			let tokens = parse_required_token(tokens, &Tok::In, "for-loop")?;
			// Parse range expression.
			let (tokens, range) = parse_expr(tokens)?;
			// Parse "do".
			let tokens = parse_required_token(tokens, &Tok::Do, "for-loop")?;
			// Parse body.
			let (tokens, body) = parse_expr(tokens)?;
			// Assemble for-loop.
			Ok((tokens, Expr::ForLoop { loop_var: loop_var.clone(), range: Box::new(range), body: Box::new(body) }))
		},
		[invalid @ _, ..] => Err(ParseError::InvalidInput {
			context: "for-loop",
			expected: Some("loop variable".into()),
			actual: invalid.clone(),
		}),
	}
}

/// Parses a while-loop, starting after "while".
fn parse_while_loop(tokens: &[Tok]) -> ParseExprResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens)?;
	// Parse "do".
	let tokens = parse_required_token(&tokens, &Tok::Do, "while-loop")?;
	// Parse body.
	let (tokens, body_expr) = parse_expr(tokens)?;
	// Assemble while-loop.
	Ok((tokens, Expr::WhileLoop {
		test: Box::new(test_expr),
		body: Box::new(body_expr),
	}))
}

/// Parses a branch statment - if-then or if-then-else - starting after "if".
fn parse_branch(tokens: &[Tok]) -> ParseExprResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens)?;
	// Parse "then".
	let tokens = parse_required_token(&tokens, &Tok::Then, "branch statement")?;
	// Parse "then" branch.
	let (tokens, then_expr) = parse_expr(&tokens)?;
	// Check for "else" branch.
	let (tokens, has_else_branch) = parse_optional_token(&tokens, &Tok::Else);
	let (tokens, else_expr) = if has_else_branch {
		// Parse "else" branch.
		parse_expr(tokens)?
	} else {
		// Missing else branch is equivalent to ().
		(tokens, Expr::TupleExpr(Box::new(Vec::new())))
	};
	// Assemble branch statement.
	Ok((tokens, Expr::Branch {
		test: Box::new(test_expr),
		then_expr: Box::new(then_expr),
		else_expr: Box::new(else_expr),
	}))
}

/// Parses an assignment operation, starting after "let".
fn parse_assignment(tokens: &[Tok]) -> ParseExprResult {
	// Parse LHS.
	match tokens {
		[] => Err(ParseError::EndOfInput {
			context: "assignment",
			expected: "variable".into(),
		}),
		[Tok::Sym(lhs), tokens @ ..] => {
			// Parse "=".
			let tokens = parse_required_token(&tokens, &Tok::Eq, "assignment")?;
			// Parse RHS.
			let (tokens, rhs) = parse_expr(tokens)?;
			// Assemble assignment from symbol and RHS.
			Ok((tokens, Expr::Assign { lhs: lhs.clone(), rhs: Box::new(rhs) }))
		},
		[invalid @ _, ..] => Err(ParseError::InvalidInput {
			context: "assignment",
			expected: Some("variable".into()),
			actual: invalid.clone(),
		}),
	}
}

/// Parses an import statement, starting after "import".
fn parse_import(tokens: &[Tok]) -> ParseExprResult {
	let (tokens, target) = parse_expr(tokens)?;
	Ok((tokens, Expr::Import { target: Box::new(target) }))
}
