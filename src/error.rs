use super::number::NumError;
use super::tokens::Tok;
use super::types::Type;

use std::fmt;

/// An error that occurs while lexing a string into Gynjo tokens.
#[derive(Debug)]
pub struct LexError {
    pub unrecognized_token: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Unrecognized token: \"{}\"", self.unrecognized_token)
    }
}

/// An error that occurs while parsing tokens into a Gynjo AST.
#[derive(Debug)]
pub enum ParseError {
    Expected {
        context: &'static str,
        expected: String,
    },
    Unexpected {
        context: &'static str,
        unexpected: String,
    },
    Swapped {
        context: &'static str,
        expected: String,
        actual: String,
    },
    UnusedInput {
        first_unused: Tok,
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::Expected { context, expected } => {
                write!(f, "Expected \"{}\" in {}", expected, context)
            },
            ParseError::Unexpected { context, unexpected } => {
                write!(f, "Unexpected \"{}\" in {}", unexpected, context)
            },
            ParseError::Swapped { context, expected, actual } => {
                write!(f, "Expected \"{}\" in {}, found \"{}\"", expected, context, actual)
            },
            ParseError::UnusedInput { first_unused } => {
                write!(f, "Unused input starting at \"{}\"", first_unused.to_string())
            }
        }
    }
}

/// An error that occurs while evaluating a Gynjo AST.
#[derive(Debug)]
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
    ArgCountMismatch {
        required: usize,
        received: usize,
    },
    Undefined(String),
    UnusedResult(String),
    ReturnOutsideBlock,
    FileError {
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
            RuntimeError::Numeric(err) => write!(f, "{}", err),
            RuntimeError::UnaryTypeMismatch { context, expected, actual } => {
                write!(f, "Expected {} in {}, found {}", expected, context, actual)
            },
            RuntimeError::BinaryTypeMismatch { context, left, right } => {
                write!(f, "Cannot perform {} with {} and {}", context, left, right)
            },
            RuntimeError::ArgCountMismatch { required, received } => {
                write!(f, "Function requires {} argument{}, received {}", required, if *required == 1 { "" } else { "s" }, received)
            },
            RuntimeError::Undefined(name) => write!(f, "\"{}\" is undefined", name),
            RuntimeError::UnusedResult(value) => write!(f, "Unused result: {}", value),
            RuntimeError::ReturnOutsideBlock => write!(f, "Cannot return outside statement block"),
            RuntimeError::FileError { filename, file_error } => {
                write!(f, "Error with file \"{}\" ({})", filename, file_error)
            },
            RuntimeError::LibError { lib_name, nested_error } => {
                write!(f, "Error in library \"{}\": {}", lib_name, nested_error)
            },
        }
    }
}

/// A lex, parse, or runtime error.
#[derive(Debug)]
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
            Error::Lex(lex_error) => write!(f, "(Lex error) {}", lex_error),
            Error::Parse(parse_error) => write!(f, "(Parse error) {}", parse_error),
            Error::Runtime(runtime_error) => write!(f, "(Runtime error) {}", runtime_error),
        }
    }
}