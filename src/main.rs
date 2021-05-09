mod env;
mod errors;
mod expressions;
mod format_with_env;
mod interpreter;
mod intrinsics;
mod lexer;
mod parser;
mod primitives;
mod symbol;
mod tokens;
#[macro_use]
mod values;

#[macro_use]
extern crate lazy_static;

use format_with_env::FormatWithEnv;

use std::io::{self, Write};

fn main() {
	// Create environment with core libs.
	let mut env = env::Env::with_core_libs();
	// Try to load user's profile script.
	env::import_lib_from_path(&mut env, "\"profile.gynj\"");
	// Initial value of "ans" is ().
	env.lock()
		.unwrap()
		.set_var("ans".into(), values::Val::empty());
	// REPL
	loop {
		if let Err(error) = repl_iter(&mut env) {
			println!("{}", error);
		}
	}
}

fn repl_iter(mut env: &mut env::SharedEnv) -> Result<(), errors::GynjoErr> {
	// Evaluate.
	let value = interpreter::eval_expr(&mut env, get_expr()?).map_err(errors::GynjoErr::rt)?;
	if value != values::Val::empty() {
		// Print the computed value.
		println!("{}", value.format_with_env(&env));
		// Assign the value to "ans".
		env.lock().unwrap().set_var("ans".into(), value);
	}
	Ok(())
}

fn get_expr() -> Result<expressions::Expr, errors::GynjoErr> {
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
			Err(errors::ParseErr::EndOfInput { .. }) => {
				tokens.append(&mut get_token_line("   ")?);
			}
			// Unrecoverable parse error
			Err(parse_error) => return Err(errors::GynjoErr::parse(parse_error)),
		}
	}
}

fn get_token_line(prompt: &str) -> Result<Vec<tokens::Tok>, errors::GynjoErr> {
	print!("{}", prompt);
	io::stdout().flush().unwrap();
	let mut input = String::new();
	io::stdin().read_line(&mut input).unwrap();
	lexer::lex(&input).map_err(errors::GynjoErr::lex)
}
