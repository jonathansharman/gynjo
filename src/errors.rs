use super::quantity::QuantErr;
use super::tokens::Tok;
use super::types::Type;

use itertools::Itertools;

use std::fmt;

/// An error that occurs while lexing a string into Gynjo tokens.
#[derive(Eq, PartialEq, Debug)]
pub struct LexErr {
	pub unrecognized_token: String,
}

impl fmt::Display for LexErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "Unrecognized token: \"{}\"", self.unrecognized_token)
	}
}

/// An error that occurs while parsing tokens into a Gynjo AST.
#[derive(Eq, PartialEq, Debug)]
pub enum ParseErr {
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

impl fmt::Display for ParseErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			ParseErr::InvalidInput { context, expected, actual } => {
				match expected {
					Some(expected) => write!(f, "Found \"{}\" in {}, expected \"{}\"", actual, context, expected),
					None => write!(f, "Unexpected \"{}\" in {}", actual, context),
				}
			},
			ParseErr::EndOfInput { context, expected } => {
				write!(f, "End of input in {}, expected {}", context, expected)
			},
			ParseErr::UnusedInput { first_unused } => {
				write!(f, "Unused input starting at \"{}\"", first_unused)
			}
		}
	}
}

/// An error that occurs while evaluating a Gynjo AST.
#[derive(Eq, PartialEq, Debug)]
pub enum RtErr {
	Quant(QuantErr),
	UnaryTypeMismatch {
		context: &'static str,
		expected: Vec<Type>,
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
	LibErr {
		lib_name: String,
		nested_error: Box<GynjoErr>,
	},
}

impl RtErr {
	pub fn quant(err: QuantErr) -> RtErr {
		RtErr::Quant(err)
	}
}

impl fmt::Display for RtErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			RtErr::Quant(err) => err.fmt(f),
			RtErr::UnaryTypeMismatch { context, expected, actual } => {
				write!(f, "Expected {} in {}, found {}", expected.iter().map(|t| t.to_string()).join(" or "), context, actual)
			},
			RtErr::BinaryTypeMismatch { context, left, right } => {
				write!(f, "Cannot perform {} with {} and {}", context, left, right)
			},
			RtErr::InvalidTypeCast { from, to } => {
				write!(f, "Cannot cast {} to {}", from, to)
			},
			RtErr::OutOfBounds => write!(f, "Out of bounds"),
			RtErr::InvalidIndex { idx } => write!(f, "Invalid index: {}", idx),
			RtErr::ArgCountMismatch { required, received } => {
				write!(f, "Function requires {} argument{}, received {}", required, if *required == 1 { "" } else { "s" }, received)
			},
			RtErr::Undefined(name) => write!(f, "\"{}\" is undefined", name),
			RtErr::UnusedResult(value) => write!(f, "Unused result: {}", value),
			RtErr::CouldNotOpenFile { filename, file_error } => {
				write!(f, "Could not open \"{}\" ({})", filename, file_error)
			},
			RtErr::LibErr { lib_name, nested_error } => {
				write!(f, "Error in library \"{}\": {}", lib_name, nested_error)
			},
		}
	}
}

/// A lex, parse, or runtime error.
#[derive(Eq, PartialEq, Debug)]
pub enum GynjoErr {
	Lex(LexErr),
	Parse(ParseErr),
	Rt(RtErr),
}

impl GynjoErr {
	pub fn lex(err: LexErr) -> GynjoErr {
		GynjoErr::Lex(err)
	}
	
	pub fn parse(err: ParseErr) -> GynjoErr {
		GynjoErr::Parse(err)
	}
	
	pub fn rt(err: RtErr) -> GynjoErr {
		GynjoErr::Rt(err)
	}
}

impl fmt::Display for GynjoErr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			GynjoErr::Lex(lex_error) => write!(f, "(lex error) {}", lex_error),
			GynjoErr::Parse(parse_error) => write!(f, "(parse error) {}", parse_error),
			GynjoErr::Rt(runtime_error) => write!(f, "(runtime error) {}", runtime_error),
		}
	}
}

impl From<LexErr> for GynjoErr {
    fn from(err: LexErr) -> Self {
        GynjoErr::lex(err)
    }
}

impl From<ParseErr> for GynjoErr {
    fn from(err: ParseErr) -> Self {
        GynjoErr::parse(err)
    }
}

impl From<RtErr> for GynjoErr {
    fn from(err: RtErr) -> Self {
        GynjoErr::rt(err)
    }
}
