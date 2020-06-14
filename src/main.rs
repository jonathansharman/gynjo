#![feature(exclusive_range_pattern)]

#[macro_use]
mod values;

mod env;
mod error;
mod exprs;
mod interpreter;
mod intrinsics;
mod lexer;
mod number;
mod parser;
mod primitives;
mod symbol;
mod tokens;
mod types;

#[macro_use]
extern crate lazy_static;

use std::io::{self, Write};

fn main() {
	// Create environment with core libs.
	let mut env = env::Env::with_core_libs();
	// Try to load user's profile script.
	env::import_lib_from_path(&mut env, "\"profile.gynj\"");
	// REPL
	loop {
		if let Err(error) = repl_iter(&mut env) {
			println!("{}", error);
		}
	}
}

fn repl_iter(mut env: &mut env::SharedEnv) -> Result<(), error::Error> {
	// Evaluate.
	let value = interpreter::eval_expr(&mut env, get_expr()?).map_err(error::Error::runtime)?;
	if value != values::Val::empty() {
		// Print the computed value.
		println!("{}", value.to_string(&mut env));
		// Assign the value to "ans".
		env.lock().unwrap().assign("ans".into(), value);
	}
	Ok(())
}

fn get_expr() -> Result<exprs::Expr, error::Error> {
	// Read first line of input.
	let mut tokens = get_token_line(">> ")?;
	loop {
		// Keep reading as long as the last token is the line continuation token.
		while let Some(tokens::Tok::LineContinuation) = tokens[..].last() {
			tokens.pop();
			tokens.append(&mut get_token_line("   ")?);
		}
		// Now try to parse the result.
		match parser::parse(&tokens[..]) {
			Ok(expr) => return Ok(expr),
			// Try reading more input if there's an end-of-input error.
			Err(error::ParseError::EndOfInput { .. }) => {
				tokens.append(&mut get_token_line("   ")?);
			},
			// Unrecoverable parse error
			Err(parse_error) => return Err(error::Error::parse(parse_error)),
		}
	}
}

fn get_token_line(prompt: &str) -> Result<Vec<tokens::Tok>, error::Error> {
	print!("{}", prompt);
	io::stdout().flush().unwrap();
	let mut input = String::new();
	io::stdin().read_line(&mut input).unwrap();
	lexer::lex(&input).map_err(error::Error::lex)
}
