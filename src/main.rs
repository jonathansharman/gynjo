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
mod stmts;
mod symbol;
mod tokens;
mod types;

#[macro_use]
extern crate lazy_static;

use error::Error;
use interpreter::{exec, eval};

use std::io::{self, Write};

fn main() {
	// Create environment with core libs.
	let mut env = env::Env::with_core_libs();

	// Try to load user's profile script.
	env::import_lib(&mut env, "\"profile.gynj\"");

	// REPL
	loop {
		print!(">> ");
		io::stdout().flush().unwrap();
		let mut input = String::new();
		io::stdin().read_line(&mut input).unwrap();
		while let Some('\\') = input.chars().last() {
			// Continue line. Add a space to ensure new token on next line.
			input.pop();
			input.push(' ');
			print!("   ");
			io::stdout().flush().unwrap();
			io::stdin().read_line(&mut input).unwrap();
		}
		// First try to execute the input as a statement.
		if let Err(exec_error) = exec(&mut env, &input) {
			// Execution failed. Try to evaluate the input as an expression.
			match eval(&mut env, &input) {
				Ok(value) => {
					// Print the computed value.
					println!("{}", value.to_string(&mut env));
				},
				Err(eval_error) => {
					// Both failed. Display the highest-level error.
					let error = match (exec_error, eval_error) {
						(Error::Runtime(runtime_error), _) => Error::Runtime(runtime_error),
						(_, Error::Runtime(runtime_error)) => Error::Runtime(runtime_error),
						(Error::Parse(parse_error), _) => Error::Parse(parse_error),
						(_, Error::Parse(parse_error)) => Error::Parse(parse_error),
						// It's a tie. Display evaluation error by default.
						(_, eval_error @ _) => eval_error,
					};
					println!("{}", error);
				}
			}
		}
	}
}
