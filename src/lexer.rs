use super::error::LexError;
use super::intrinsics::Intrinsic;
use super::number::Num;
use super::primitives::{Prim, Bool};
use super::tokens::Tok;

use bigdecimal::BigDecimal;
use fancy_regex::{Regex};
use num_bigint::BigInt;

use std::str::FromStr;

/// Result of lexing: either a vector of tokens or an error message.
type LexResult = Result<Vec<Tok>, LexError>;

/// Surrounds a keyword with regex to ensure it's at the start of the line and isn't followed by symbol characters.
macro_rules! keyword {
	($text:expr) => {
		concat!("^", $text, "(?![a-zA-Z_])")
	};
}

/// Lexes `input` into a vector of tokens, if possible.
pub fn lex(input: &str) -> LexResult {
	lazy_static! {
		static ref BAD_TOKEN_REGEX: Regex = Regex::new(r"\W+").unwrap();
		static ref SINGLE_LEXERS: [SingleLexer; 50] = [
			// Whitespace (ignored)
			SingleLexer::new(r"^\s+", |_| None),
			// Line comment (ignored)
			SingleLexer::new(r"^//.*", |_| None),
			// Operators/separators
			SingleLexer::new(r"^=", |_| Some(Tok::Eq)),
			SingleLexer::new(r"^!=", |_| Some(Tok::Neq)),
			SingleLexer::new(r"^~", |_| Some(Tok::Approx)),
			SingleLexer::new(r"^<=", |_| Some(Tok::Leq)),
			SingleLexer::new(r"^<", |_| Some(Tok::Lt)),
			SingleLexer::new(r"^>=", |_| Some(Tok::Geq)),
			SingleLexer::new(r"^>", |_| Some(Tok::Gt)),
			SingleLexer::new(r"^\+", |_| Some(Tok::Plus)),
			SingleLexer::new(r"^->", |_| Some(Tok::Arrow)),
			SingleLexer::new(r"^-", |_| Some(Tok::Minus)),
			SingleLexer::new(r"^((\*\*)|\^)", |_| Some(Tok::Exp)),
			SingleLexer::new(r"^\*", |_| Some(Tok::Mul)),
			SingleLexer::new(r"^/", |_| Some(Tok::Div)),
			SingleLexer::new(r"^\(", |_| Some(Tok::Lparen)),
			SingleLexer::new(r"^\)", |_| Some(Tok::Rparen)),
			SingleLexer::new(r"^\[", |_| Some(Tok::Lsquare)),
			SingleLexer::new(r"^\]", |_| Some(Tok::Rsquare)),
			SingleLexer::new(r"^\{", |_| Some(Tok::Lcurly)),
			SingleLexer::new(r"^\}", |_| Some(Tok::Rcurly)),
			SingleLexer::new(r"^,", |_| Some(Tok::Comma)),
			SingleLexer::new(r"^;", |_| Some(Tok::Semicolon)),
			SingleLexer::new(r"^\?", |_| Some(Tok::Question)),
			SingleLexer::new(r"^:", |_| Some(Tok::Colon)),
			// Value literals
			SingleLexer::new(r"^((0|([1-9]\d*))?\.\d+)", |match_text| {
				Some(Tok::Prim(Prim::Num(Num::Real(BigDecimal::from_str(match_text).unwrap()))))
			}),
			SingleLexer::new(r"^(0|([1-9]\d*))", |match_text| {
				Some(Tok::Prim(Prim::Num(Num::Integer(BigInt::from_str(match_text).unwrap()))))
			}),
			SingleLexer::new(keyword!("true"), |_| Some(Tok::Prim(Prim::Bool(Bool::True)))),
			SingleLexer::new(keyword!("false"), |_| Some(Tok::Prim(Prim::Bool(Bool::False)))),
			SingleLexer::new(r#"^("([^"\\]|\\["\\])*")"#, |match_text| {
				// Strip enclosing quotes and escape characters.
				Some(Tok::from_string(match_text[1..match_text.len() - 1].replace(r#"\""#, r#"""#).replace(r"\\", r"\")))
			}),
			// Intrinsic functions
			SingleLexer::new(keyword!("top"), |_| Some(Tok::Intrinsic(Intrinsic::Top))),
			SingleLexer::new(keyword!("pop"), |_| Some(Tok::Intrinsic(Intrinsic::Pop))),
			SingleLexer::new(keyword!("push"), |_| Some(Tok::Intrinsic(Intrinsic::Push))),
			SingleLexer::new(keyword!("print"), |_| Some(Tok::Intrinsic(Intrinsic::Print))),
			SingleLexer::new(keyword!("read"), |_| Some(Tok::Intrinsic(Intrinsic::Read))),
			SingleLexer::new(keyword!("real"), |_| Some(Tok::Intrinsic(Intrinsic::ToReal))),
			// Keywords
			SingleLexer::new(keyword!("import"), |_| Some(Tok::Import)),
			SingleLexer::new(keyword!("let"), |_| Some(Tok::Let)),
			SingleLexer::new(keyword!("if"), |_| Some(Tok::If)),
			SingleLexer::new(keyword!("then"), |_| Some(Tok::Then)),
			SingleLexer::new(keyword!("else"), |_| Some(Tok::Else)),
			SingleLexer::new(keyword!("while"), |_| Some(Tok::While)),
			SingleLexer::new(keyword!("for"), |_| Some(Tok::For)),
			SingleLexer::new(keyword!("in"), |_| Some(Tok::In)),
			SingleLexer::new(keyword!("do"), |_| Some(Tok::Do)),
			SingleLexer::new(keyword!("return"), |_| Some(Tok::Return)),
			SingleLexer::new(keyword!("and"), |_| Some(Tok::And)),
			SingleLexer::new(keyword!("or"), |_| Some(Tok::Or)),
			SingleLexer::new(keyword!("not"), |_| Some(Tok::Not)),
			// Symbol
			SingleLexer::new(r"^[a-zA-Z_]+", |match_text| Some(Tok::from_symbol(match_text.to_string())))
		];
	}

	let mut next_input = input;
	let mut tokens: Vec<Tok> = Vec::new();
	while !next_input.is_empty() {
		let mut no_matches = true;
		for single_lexer in SINGLE_LEXERS.iter() {
			if let Some(match_result) = single_lexer.regex.find(next_input).unwrap() {
				if let Some(token) = (single_lexer.generator)(match_result.as_str()) {
					tokens.push(token);
				}
				next_input = &next_input[match_result.end()..];
				no_matches = false;
				break;
			}
		}
		if no_matches {
			return Err(LexError { unrecognized_token: BAD_TOKEN_REGEX.find(next_input).unwrap().unwrap().as_str().into() });
		}
	}
	Ok(tokens)
}

// Generates the token from a pattern, if there is one.
type Generator = fn(&str) -> Option<Tok>;

/// Capable of matching and producing a single token.
struct SingleLexer {
	regex: Regex,
	generator: Generator,
}

impl SingleLexer {
	fn new(pattern: &str, generator: Generator) -> SingleLexer {
		SingleLexer { regex: Regex::new(pattern).unwrap(), generator }
	}
}

#[cfg(test)]
mod tests {
	use crate::error::LexError;
	use crate::lexer::lex;
	use crate::tokens::Tok;

	#[test]
	fn whitespace() -> Result<(), LexError> {
		let expected: Vec<Tok> = vec!(Tok::from(1), Tok::Plus, Tok::from(2), Tok::Plus, Tok::from(3));
		let actual = lex(" \t \n 1 \n + \t 2+3 \t \n ")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn numbers_operators_and_separators() -> Result<(), LexError> {
		let expected = vec!(
			Tok::Let,
			Tok::Eq,
			Tok::Neq,
			Tok::Lt,
			Tok::Leq,
			Tok::Gt,
			Tok::Geq,
			Tok::Approx,
			Tok::Mul,
			Tok::Lparen,
			Tok::Plus,
			Tok::Minus,
			Tok::Arrow,
			Tok::Rparen,
			Tok::Lsquare,
			Tok::Rsquare,
			Tok::Exp,
			Tok::Exp,
			Tok::Mul,
			Tok::Div,
			Tok::from(0.1),
			Tok::from(0),
			Tok::from(0.1),
			Tok::Comma,
			Tok::Question,
			Tok::Colon,
		);
		let actual = lex("let=!=<<=>>=~*(+-->)[]^***/.1 0 0.1,?:")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn line_comments() -> Result<(), LexError> {
		let expected = vec!(Tok::from(1), Tok::Plus, Tok::from(2));
		let actual = lex("1+2 // This is a line comment.")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn key_words() -> Result<(), LexError> {
		let expected = vec!(
			Tok::Import, Tok::from(1), Tok::from_symbol("imports"),
			Tok::If, Tok::from(1), Tok::from_symbol("ifs"),
			Tok::Then, Tok::from(1), Tok::from_symbol("thens"),
			Tok::Else, Tok::from(1), Tok::from_symbol("elses"),
			Tok::While, Tok::from(1), Tok::from_symbol("whiles"),
			Tok::For, Tok::from(1), Tok::from_symbol("fors"),
			Tok::In, Tok::from(1), Tok::from_symbol("ins"),
			Tok::Do, Tok::from(1), Tok::from_symbol("dos"),
			Tok::Return, Tok::from(1), Tok::from_symbol("returns"),
			Tok::And, Tok::from(1), Tok::from_symbol("ands"),
			Tok::Or, Tok::from(1), Tok::from_symbol("ors"),
			Tok::Not, Tok::from(1), Tok::from_symbol("nots"),
		);
		let actual = lex(r#"
			import1 imports
			if1 ifs
			then1 thens
			else1 elses
			while1 whiles
			for1 fors
			in1 ins
			do1 dos
			return1 returns
			and1 ands
			or1 ors
			not1 nots
		"#)?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn valid_strings() -> Result<(), LexError> {
		assert_eq!(Tok::from_string(""), lex(r#""""#)?[0]);
		assert_eq!(Tok::from_string("abc"), lex(r#""abc""#)?[0]);
		assert_eq!(Tok::from_string(r#""abc""#), lex(r#""\"abc\"""#)?[0]);
		assert_eq!(Tok::from_string(r#"a\b\c"#), lex(r#""a\\b\\c""#)?[0]);
		Ok(())
	}

	#[test]
	fn invalid_strings() -> Result<(), LexError> {
		assert!(lex(r#"""#).is_err());
		assert!(lex(r#"""""#).is_err());
		assert!(lex(r#""\""#).is_err());
		assert!(lex(r#""\a""#).is_err());
		Ok(())
	}
}
