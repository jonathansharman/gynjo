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

use interpreter::eval;
use values::Val;

use std::io::{self, Write};

fn main() {
	// Create environment with core libs.
	let mut env = env::Env::with_core_libs();

	// Try to load user's profile script.
	env::import_lib_from_path(&mut env, "\"profile.gynj\"");

	// REPL
	loop {
		// Read input.
		print!(">> ");
		io::stdout().flush().unwrap();
		let mut input = String::new();
		io::stdin().read_line(&mut input).unwrap();
		input = input.trim().into();
		while let Some('\\') = input.chars().last() {
			// Continue line. Add a space to ensure new token on next line.
			input.pop();
			input.push(' ');
			print!("   ");
			io::stdout().flush().unwrap();
			io::stdin().read_line(&mut input).unwrap();
		}
		// Evaluate.
		match eval(&mut env, &input) {
			Ok(value) => {
				if value != Val::empty() {
					// Print the computed value.
					println!("{}", value.to_string(&mut env));
					// Assign the value to "ans".
					env.lock().unwrap().assign("ans".into(), value);
				}
			},
			Err(error) => println!("{}", error),
		}
	}
}
