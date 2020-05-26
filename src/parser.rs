use super::exprs::{Expr, BinaryExpr, BinaryOp, Cluster ,ClusterItem, ClusterConnector, Lambda, LambdaBody};
use super::intrinsics::Intrinsic;
use super::literals::Literal;
use super::stmts::Stmt;
use super::symbol::Symbol;
use super::tokens::Token;

/// Result of expression parsing: (remaining tokens, parsed expression).
type ParseExprResult<'a> = Result<(&'a [Token], Expr), String>;

/// Result of statement parsing: (remaining tokens, parsed statement).
type ParseStmtResult<'a> = Result<(&'a [Token], Stmt), String>;

/// If possible, parses the next single expression from `tokens`.
/// Returns a slice of the unused tokens along with the parsed expression.
pub fn parse_expr(tokens: &[Token]) -> ParseExprResult {
	let (tokens, negation_result) = parse_negation(&tokens)?;
	// Check for conditional expression.
	let (tokens, is_conditional_expr) = parse_optional_token(&tokens, &Token::Question);
	if is_conditional_expr {
		// Parse expression if true.
		let (tokens, then_expr) = parse_expr(tokens).map_err(|_| "expected true case in conditional expression")?;
		// Parse ":".
		let tokens = parse_required_token(&tokens, &Token::Colon, "conditional expression")?;
		// Parse expression if false.
		let (tokens, else_expr) = parse_expr(tokens).map_err(|_| "expected false case in conditional expression")?;
		Ok((tokens, Expr::Cond { test: Box::new(negation_result), then_expr: Box::new(then_expr), else_expr: Box::new(else_expr) }))
	} else {
		Ok((tokens, negation_result))
	}
}

/// If possible, parses the next single statement from `tokens`.
/// Returns an iterator to the next unused token along with the parsed statement, or an error message.
pub fn parse_stmt<'a>(tokens: &'a [Token]) -> ParseStmtResult {
	match tokens {
		[] => Ok((tokens, Stmt::Nop)),
		[Token::Import, rest @ ..] => parse_import(rest),
		[Token::Let, rest @ ..] => parse_assignment(rest),
		[Token::If, rest @ ..] => parse_branch(rest),
		[Token::While, rest @ ..] => parse_while_loop(rest),
		[Token::For, rest @ ..] => parse_for_loop(rest),
		[Token::Return, rest @ ..] => parse_ret(rest),
		_ => {
			let (tokens, expr) = parse_expr(tokens)?;
			parse_required_token(&tokens, &Token::Semicolon, "expression statement");
			Ok((tokens, Stmt::ExprStmt(Box::new(expr))))
		},
	}
}

/// Parses a function body.
fn parse_body(tokens: &[Token]) -> ParseExprResult {
	#![feature(slice_patterns)]
	match tokens {
		[Token::Arrow, rest @ ..] => parse_expr(rest),
		_ => Err("expected function body".to_string()),
	}
}

/// Parses a Gynjo value.
fn parse_value(tokens: &[Token]) -> ParseExprResult {
	match tokens {
		[] => Err("expected value".to_string()),
		// Tuple or lambda
		[Token::Lparen, ref after_lparen @ ..] => {
			let mut elems: Vec<Expr> = Vec::new();
			// Parse contained expressions.
			let after_elems = match parse_expr(after_lparen) {
				// Found at least one expression.
				Ok((after_first_elem, first_elem)) => {
					elems.push(first_elem);
					// Try to parse additional comma-delimited expressions.
					let mut tokens = after_first_elem;
					loop {
						match tokens {
							[] => return Err("expected ',' or ')' in tuple expression".to_string()),
							// Additional tuple element
							[Token::Comma, after_comma @ ..] => {
								tokens = after_comma;
								let (after_next_elem, next_elem) = parse_expr(tokens).map_err(|_| "expected expression after ',' in tuple expression")?;
								tokens = after_next_elem;
								elems.push(next_elem);
							},
							// End of tuple
							[Token::Rparen, rest @ ..] => {
								tokens = rest;
								break;
							},
							[invalid @ _, ..] => return Err(format!("expected ',' or ')' in tuple expression, found {}", invalid.to_string())),
						}
					}
					tokens
				},
				// Empty tuple; parse close parenthesis.
				_ => parse_required_token(tokens, &Token::Rparen, "tuple expression")?,
			};
			// Try to parse this expression as a lambda.
			let mut params = Vec::with_capacity(elems.len());
			let mut could_be_lambda = true;
			for expr in elems {
				match expr {
					Expr::Symbol(param) => params.push(param),
					_ => {
						could_be_lambda = false;
						break;
					},
				}
			}
			if could_be_lambda {
				// Try to parse a lambda body.
				if let Ok((after_body, body_expr)) = parse_body(after_elems) {
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
				Ok((tokens, *elems.first().unwrap()))
			} else {
				// Return unmodified tuple.
				Ok((tokens, Expr::TupleExpr(Box::new(elems))))
			}
		},
		// List
		[Token::Lsquare, after_lsquare @ ..] => {
			let mut tokens = after_lsquare;
			let mut elems = Box::new(Vec::new());
			// Try to parse an expression.
			if let Ok((after_first_elem, first_elem)) = parse_expr(tokens) {
				tokens = after_first_elem;
				elems.push(first_elem);
				// Try to parse additional comma-delimited expressions.
				loop {
					match tokens {
						[] => return Err("expected ',' or ']' in list expression".to_string()),
						// Additional list element
						[Token::Comma, after_comma @ ..] => {
							tokens = after_comma;
							let (after_next_elem, next_elem) = parse_expr(tokens).map_err(|_| "expected expression after ',' in list expression")?;
							tokens = after_next_elem;
							elems.push(next_elem);
						},
						// End of list
						[Token::Rsquare, rest @ ..] => {
							tokens = rest;
							break;
						},
						[invalid @ _, ..] => return Err(format!("expected ',' or ']' in list expression, found {}", invalid.to_string())),
					}
				}
			}
			// Parse close square bracket.
			tokens = parse_required_token(&tokens, &Token::Rsquare, "list expression")?;
			Ok((tokens, Expr::ListExpr(elems)))
		},
		// Block
		[Token::Lcurly, after_lcurly @ ..] => {
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
			tokens = parse_required_token(&tokens, &Token::Rcurly, "block")?;
			return Ok((tokens, Expr::Block { stmts: stmts }));
		},
		// Intrinsic function
		[Token::Intrinsic(f), tokens @ ..] => {
			let params = match f {
				Intrinsic::Top | Intrinsic::Pop => vec!(Symbol { name: "list".to_string() }),
				Intrinsic::Push => vec!(Symbol { name: "list".to_string() }, Symbol { name: "value".to_string() }),
				Intrinsic::Print | Intrinsic::Read => vec!(Symbol { name: "value".to_string() }),
			};
			Ok((tokens, Expr::Lambda(Lambda { params, body: LambdaBody::Intrinsic(f.clone()) })))
		},
		// Symbol or lambda
		[Token::Symbol(symbol), tokens @ ..] => {
			// Could be a parentheses-less unary lambda. Try to parse a lambda body.
			if let Ok((tokens, body_expr)) = parse_body(tokens) {
				// Assemble lambda from the parameter wrapped in a tuple and the body.
				Ok((tokens, Expr::Lambda(Lambda {
					params: vec!(symbol.clone()),
					body: LambdaBody::UserDefined(Box::new(body_expr)),
				})))
			} else {
				// It's just a symbol.
				Ok((tokens, Expr::Symbol(symbol.clone())))
			}
		},
		// Literal
		[Token::Literal(literal), tokens @ ..] => Ok((tokens, Expr::Literal(literal.clone()))),
		[t, ..] => Err(format!("unexpected token in expression: {}", t.to_string())),
	}
}

/// Parses a single `required` token from `tokens` and returns the remaining tokens.
/// `context` - The expression in which the token is required, for the purpose of error reporting.
fn parse_required_token<'a>(tokens: &'a [Token], required: &Token, context: &'static str) -> Result<&'a [Token], String> {
	match tokens {
		[] => Err(format!("expected '{}' in {}", required.to_string(), context)),
		[t, rest @ ..] if (t == required) => Ok(rest),
		[invalid @ _, ..] => Err(format!("expected '{}' in {}, found {}", required.to_string(), context, invalid.to_string())),
	}
}

/// Parses a single `optional` token from `tokens` and returns the remaining tokens and whether the token was parsed.
fn parse_optional_token<'a>(tokens: &'a [Token], optional: &Token) -> (&'a [Token], bool) {
	match tokens {
		[t, rest @ ..] if (t == optional) => (rest, true),
		_ => (tokens, false),
	}
}

/// Parses a cluster item after an operator or parenthesis.
fn parse_mandatory_cluster_item(tokens: &[Token], connector: ClusterConnector) -> Result<(&[Token], ClusterItem), String> {
	let (after_minus, negated) = parse_optional_token(tokens, &Token::Minus);
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
fn parse_cluster(tokens: &[Token]) -> ParseExprResult {
	// Parse the first cluster item.
	let (tokens, first_negated) = parse_optional_token(tokens, &Token::Minus);
	let (tokens, first_expr) = parse_value(tokens)?;
	let items = vec!(ClusterItem {
		expr: Box::new(first_expr),
		negated: first_negated,
		connector: ClusterConnector::None,
	});
	// Parse subsequent cluster items.
	let mut tokens = tokens;
	loop {
		match tokens {
			[] => break,
			[Token::Mul, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Mul)?;
				tokens = after_item;
				items.push(item);
			},
			[Token::Div, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Div)?;
				tokens = after_item;
				items.push(item);
			},
			[Token::Exp, after_connector @ ..] => {
				let (after_item, item) = parse_mandatory_cluster_item(after_connector, ClusterConnector::Exp)?;
				tokens = after_item;
				items.push(item);
			},
			[Token::Lparen, ..] => {
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
				}
			},
		}
	}
	if items.len() == 1 && !items[0].negated {
		// Found a single non-negated value. Just extract it here.
		Ok((tokens, *items[0].expr))
	} else {
		Ok((tokens, Expr::Cluster(Cluster { items })))
	}
}

/// Parses a series of additions and subtractions.
fn parse_terms(tokens: &[Token]) -> ParseExprResult {
	// Parse first term.
	let (mut tokens, mut terms) = parse_cluster(tokens)?;
	// Parse subsequent terms.
	let process_term = |rest, op: BinaryOp| -> Result<(), String> {
		let (after_term, next_term) = parse_cluster(rest)
			.map_err(|_| format!("expected a term after '{}'", op.to_string()))?;
		tokens = after_term;
		// Incorporate next term into expression.
		terms = Expr::BinaryExpr(BinaryExpr {
			op: op,
			left: Box::new(terms),
			right: Box::new(next_term),
		});
		Ok(())
	};
	loop {
		match tokens {
			[] => break,
			[Token::Plus, rest @ ..] => process_term(rest, BinaryOp::Add)?,
			[Token::Minus, rest @ ..] => process_term(rest, BinaryOp::Sub)?,
			[_, ..] => break,
		}
	}
	Ok((tokens, terms))
}

/// Parses a series of comparison checks (not including equals or not equals).
fn parse_comparisons(tokens: &[Token]) -> ParseExprResult {
	// Parse first comparison.
	let (mut tokens, mut comparisons) = parse_terms(tokens)?;
	// Parse subsequent comparisons.
	let process_comparison = |rest, op: BinaryOp| -> Result<(), String> {
		let (after_term, next_comparison) = parse_terms(rest)
			.map_err(|_| format!("expected a comparison after '{}'", op.to_string()))?;
		tokens = after_term;
		// Incorporate next comparison into expression.
		comparisons = Expr::BinaryExpr(BinaryExpr {
			op: op,
			left: Box::new(comparisons),
			right: Box::new(next_comparison),
		});
		Ok(())
	};
	loop {
		match tokens {
			[] => break,
			[Token::Lt, rest @ ..] => process_comparison(rest, BinaryOp::Lt)?,
			[Token::Leq, rest @ ..] => process_comparison(rest, BinaryOp::Leq)?,
			[Token::Gt, rest @ ..] => process_comparison(rest, BinaryOp::Gt)?,
			[Token::Geq, rest @ ..] => process_comparison(rest, BinaryOp::Geq)?,
			[_, ..] => break,
		}
	}
	Ok((tokens, comparisons))
}

/// Parses a series of equality, inequality, or approximate equality checks.
fn parse_eq_checks(tokens: &[Token]) -> ParseExprResult {
	// Parse first checks.
	let (mut tokens, mut checks) = parse_terms(tokens)?;
	// Parse subsequent checks.
	let process_check = |rest, op: BinaryOp| -> Result<(), String> {
		let (after_term, next_check) = parse_terms(rest)
			.map_err(|_| format!("expected an equality check after '{}'", op.to_string()))?;
		tokens = after_term;
		// Incorporate next check into expression.
		checks = Expr::BinaryExpr(BinaryExpr {
			op: op,
			left: Box::new(checks),
			right: Box::new(next_check),
		});
		Ok(())
	};
	loop {
		match tokens {
			[] => break,
			[Token::Lt, rest @ ..] => process_check(rest, BinaryOp::Lt)?,
			[Token::Leq, rest @ ..] => process_check(rest, BinaryOp::Leq)?,
			[Token::Gt, rest @ ..] => process_check(rest, BinaryOp::Gt)?,
			[Token::Geq, rest @ ..] => process_check(rest, BinaryOp::Geq)?,
			[_, ..] => break,
		}
	}
	Ok((tokens, checks))
}

/// Parses a series of logical conjunctions.
fn parse_conjunctions(tokens: &[Token]) -> ParseExprResult {
	let (mut tokens, mut conjunctions) = parse_eq_checks(tokens)?;
	loop {
		match tokens {
			[Token::Or, after_or @ ..] => {
				// Parse next conjunction.
				let (after_conjunction, next_conjunction) = parse_eq_checks(after_or).map_err(|_| "expected conjunction")?;
				tokens = after_conjunction;
				// Incorporate into expression.
				conjunctions = Expr::BinaryExpr(BinaryExpr {				
					op: BinaryOp::Or,
					left: Box::new(conjunctions),
					right: Box::new(next_conjunction),
				})
			},
			_ => break,
		}
	}
	Ok((tokens, conjunctions))
}

/// Parses a series of logical disjunctions.
fn parse_disjunctions(tokens: &[Token]) -> ParseExprResult {
	let (mut tokens, mut disjunctions) = parse_conjunctions(tokens)?;
	loop {
		match tokens {
			[Token::Or, after_or @ ..] => {
				// Parse next disjunction.
				let (after_disjunction, next_disjunction) = parse_conjunctions(after_or).map_err(|_| "expected disjunction")?;
				tokens = after_disjunction;
				// Incorporate into expression.
				disjunctions = Expr::BinaryExpr(BinaryExpr {				
					op: BinaryOp::Or,
					left: Box::new(disjunctions),
					right: Box::new(next_disjunction),
				})
			},
			_ => break,
		}
	}
	Ok((tokens, disjunctions))
}

/// Parses a logical negation. Note that negation is right-associative.
fn parse_negation(tokens: &[Token]) -> ParseExprResult {
	match tokens {
		[] => Err("expected expression".to_string()),
		[Token::Not, tokens @ ..] => {
			let (tokens, expr) = parse_negation(tokens)?;
			Ok((tokens, Expr::Not { expr: Box::new(expr) }))
		},
		_ => parse_disjunctions(tokens),
	}
}

/// Parses a return statement, starting after "return".
fn parse_ret(tokens: &[Token]) -> ParseStmtResult {
	let (tokens, expr) = parse_expr(tokens).map_err(|_| "expected return expression")?;
	Ok((tokens, Stmt::Return { result: Box::new(expr) }))
}

/// Parses a for-loop, starting after "for".
fn parse_for_loop(tokens: &[Token]) -> ParseStmtResult {
	match tokens {
		// Parse loop variable and "in".
		[Token::Symbol(loop_var), Token::In, tokens @ ..] => {
			// Parse range expression.
			let (tokens, range) = parse_expr(tokens).map_err(|_| "expected range expression in for-loop")?;
			// Parse "do".
			let tokens = parse_required_token(tokens, &Token::Do, "for-loop")?;
			// Parse body.
			let (tokens, body) = parse_stmt(tokens)?;
			// Assemble for-loop.
			Ok((tokens, Stmt::ForLoop { loop_var: loop_var.clone(), range: Box::new(range), body: Box::new(body) }))
		},
		_ => Err("expected loop variable followed by \"in\" in for-loop".to_string()),
	}
}

/// Parses a while-loop, starting after "while".
fn parse_while_loop(tokens: &[Token]) -> ParseStmtResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens).map_err(|_| "expected test expression in while-loop")?;
	// Parse "do".
	let tokens = parse_required_token(&tokens, &Token::Do, "while-loop")?;
	// Parse body.
	let (tokens, body_stmt) = parse_stmt(tokens).map_err(|_| "expected body in while-loop")?;
	// Assemble while-loop.
	Ok((tokens, Stmt::WhileLoop {
		test: Box::new(test_expr),
		body: Box::new(body_stmt),
	}))
}

/// Parses a branch statment - if-then or if-then-else - starting after "if".
fn parse_branch(tokens: &[Token]) -> ParseStmtResult {
	// Parse test expression.
	let (tokens, test_expr) = parse_expr(&tokens).map_err(|_| "expected test expression in branch statment")?;
	// Parse "then".
	let tokens = parse_required_token(&tokens, &Token::Then, "branch statement")?;
	// Parse "then" branch.
	let (tokens, then_stmt) = parse_stmt(&tokens).map_err(|_| "expected true case in branch statement")?;
	// Check for "else" branch.
	let (tokens, has_else_branch) = parse_optional_token(&tokens, &Token::Else);
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
fn parse_assignment(tokens: &[Token]) -> ParseStmtResult {
	// Parse LHS.
	match tokens {
		[] => Err("expected assignment".to_string()),
		[Token::Symbol(lhs), after_symbol @ ..] => {
			// Parse "=".
			let after_eq = parse_required_token(&after_symbol, &Token::Eq, "assignment")?;
			// Parse RHS.
			let (after_rhs, rhs) = parse_expr(after_eq)?;
			// Assemble assignment from symbol and RHS.
			Ok((after_rhs, Stmt::Assign { lhs: lhs.clone(), rhs: Box::new(rhs) }))
		},
		[invalid @ _, ..] => Err(format!("expected loop variable in for-loop, found {}", invalid.to_string())),
	}
}

/// Parses an import statement, starting after "import".
fn parse_import(tokens: &[Token]) -> ParseStmtResult {
	match tokens {
		[] => Err("expected import target".to_string()),
		[Token::Symbol(Symbol { name }), tokens @ ..] => Ok((tokens, Stmt::Import { filename : name.clone() })),
		[Token::Literal(Literal::String(filename)), tokens @ ..] => Ok((tokens, Stmt::Import { filename: filename.clone() })),
		[first, ..] => Err(format!("expected filename (symbol or string) in import statement, found {}", first.to_string())),
	}
}
