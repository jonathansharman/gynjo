#![feature(exclusive_range_pattern)]

#[macro_use]
mod values;

mod env;
mod exprs;
mod interpreter;
mod intrinsics;
mod lexer;
mod literals;
mod parser;
mod stmts;
mod symbol;
mod tokens;

#[macro_use]
extern crate lazy_static;

use std::io;

fn main() {
	// Create environment with core libs.
	let mut env = env::Env::make_with_core_libs();

	// Try to load user's profile script.
	env::import_lib(&mut env, "\"profile.gynj\"");

	// REPL
	loop {
		print!(">> ");
		let mut input = String::new();
		io::stdin().read_line(&mut input);
		while let Some('\\') = input.chars().last() {
			// Continue line. Add a space to ensure new token on next line.
			input.pop();
			input.push(' ');
			print!("   ");
			io::stdin().read_line(&mut input);
		}
		// First try to interpret the line as an expression.
		let eval_result = interpreter::eval(&mut env, &input);
		match eval_result {
			Ok(value) => {
				// Print the computed value.
				println!("{}", value.to_string(&mut env));
			},
			Err(_) => {
				// Invalid expression. Try a statement instead.
				if let Err(err) = interpreter::exec(&mut env, &input) {
					// Still didn't work; report statement error.
					println!("{}", err);
				}
			}
		}
	}
}
