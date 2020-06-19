use super::numbers::NumError;
use super::tokens::Tok;
use super::types::Type;

use std::fmt;

/// An error that occurs while lexing a string into Gynjo tokens.
#[derive(Eq, PartialEq, Debug)]
pub struct LexError {
	pub unrecognized_token: String,
}

impl fmt::Display for LexError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Unrecognized token: \"{}\"", self.unrecognized_token)
	}
}

/// An error that occurs while parsing tokens into a Gynjo AST.
#[derive(Eq, PartialEq, Debug)]
pub enum ParseError {
	InvalidInput {
		context: &'static str,
		expected: Option<String>,
		actual: Tok,
	},
	EndOfInput {
		context: &'static str,
		expected: String,
	},
	UnusedInput {
		first_unused: Tok,
	},
}

impl fmt::Display for ParseError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParseError::InvalidInput { context, expected, actual } => {
				match expected {
					Some(expected) => write!(f, "Found \"{}\" in {}, expected \"{}\"", actual, context, expected),
					None => write!(f, "Unexpected \"{}\" in {}", actual, context),
				}
			},
			ParseError::EndOfInput { context, expected } => {
				write!(f, "End of input in {}, expected {}", context, expected)
			},
			ParseError::UnusedInput { first_unused } => {
				write!(f, "Unused input starting at \"{}\"", first_unused)
			}
		}
	}
}

/// An error that occurs while evaluating a Gynjo AST.
#[derive(Eq, PartialEq, Debug)]
pub enum RuntimeError {
	Numeric(NumError),
	UnaryTypeMismatch {
		context: &'static str,
		expected: Type,
		actual: Type,
	},
	BinaryTypeMismatch {
		context: &'static str,
		left: Type,
		right: Type,
	},
	InvalidTypeCast {
		from: Type,
		to: Type,
	},
	OutOfBounds,
	InvalidIndex { idx: String },
	ArgCountMismatch {
		required: usize,
		received: usize,
	},
	Undefined(String),
	UnusedResult(String),
	CouldNotOpenFile {
		filename: String,
		file_error: String,
	},
	LibError {
		lib_name: String,
		nested_error: Box<Error>,
	},
}

impl RuntimeError {
	pub fn numeric(err: NumError) -> RuntimeError {
		RuntimeError::Numeric(err)
	}
}

impl fmt::Display for RuntimeError {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RuntimeError::Numeric(err) => err.fmt(f),
			RuntimeError::UnaryTypeMismatch { context, expected, actual } => {
				write!(f, "Expected {} in {}, found {}", expected.to_string(), context, actual)
			},
			RuntimeError::BinaryTypeMismatch { context, left, right } => {
				write!(f, "Cannot perform {} with {} and {}", context, left, right)
			},
			RuntimeError::InvalidTypeCast { from, to } => {
				write!(f, "Cannot cast {} to {}", from, to)
			},
			RuntimeError::OutOfBounds => write!(f, "Out of bounds"),
			RuntimeError::InvalidIndex { idx } => write!(f, "Invalid index: {}", idx),
			RuntimeError::ArgCountMismatch { required, received } => {
				write!(f, "Function requires {} argument{}, received {}", required, if *required == 1 { "" } else { "s" }, received)
			},
			RuntimeError::Undefined(name) => write!(f, "\"{}\" is undefined", name),
			RuntimeError::UnusedResult(value) => write!(f, "Unused result: {}", value),
			RuntimeError::CouldNotOpenFile { filename, file_error } => {
				write!(f, "Could not open \"{}\" ({})", filename, file_error)
			},
			RuntimeError::LibError { lib_name, nested_error } => {
				write!(f, "Error in library \"{}\": {}", lib_name, nested_error)
			},
		}
	}
}

/// A lex, parse, or runtime error.
#[derive(Eq, PartialEq, Debug)]
pub enum Error {
	Lex(LexError),
	Parse(ParseError),
	Runtime(RuntimeError),
}

impl Error {
	pub fn lex(err: LexError) -> Error {
		Error::Lex(err)
	}
	
	pub fn parse(err: ParseError) -> Error {
		Error::Parse(err)
	}
	
	pub fn runtime(err: RuntimeError) -> Error {
		Error::Runtime(err)
	}
}

impl fmt::Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Error::Lex(lex_error) => write!(f, "(lex error) {}", lex_error),
			Error::Parse(parse_error) => write!(f, "(parse error) {}", parse_error),
			Error::Runtime(runtime_error) => write!(f, "(runtime error) {}", runtime_error),
		}
	}
}
