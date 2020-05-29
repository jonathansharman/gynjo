use super::intrinsics::Intrinsic;
use super::primitives::{Primitive, Boolean};
use super::symbol::Symbol;
use super::tokens::Token;

use bigdecimal::BigDecimal;
use fancy_regex::{Regex};

use std::str::FromStr;

/// Result of lexing: either a vector of tokens or an error message.
type LexResult = Result<Vec<Token>, String>;

/// Surrounds a keyword with regex to ensure it's at the start of the line and isn't followed by symbol characters.
macro_rules! keyword {
	($text:expr) => {
		concat!("^", $text, "(?![a-zA-Z_])")
	};
}

/// Lexes `input` into a vector of tokens, if possible.
pub fn lex(input: &str) -> LexResult {
	lazy_static! {
		static ref SINGLE_LEXERS: [SingleLexer; 48] = [
			// Whitespace (ignored)
			SingleLexer::new(r"^\s+", |_| None),
			// Line comment (ignored)
			SingleLexer::new(r"^//.*$", |_| None),
			// Operators/separators
			SingleLexer::new(r"^=", |_| Some(Token::Eq)),
			SingleLexer::new(r"^!=", |_| Some(Token::Neq)),
			SingleLexer::new(r"^~", |_| Some(Token::Approx)),
			SingleLexer::new(r"^<=", |_| Some(Token::Leq)),
			SingleLexer::new(r"^<", |_| Some(Token::Lt)),
			SingleLexer::new(r"^>=", |_| Some(Token::Geq)),
			SingleLexer::new(r"^>", |_| Some(Token::Gt)),
			SingleLexer::new(r"^\+", |_| Some(Token::Plus)),
			SingleLexer::new(r"^->", |_| Some(Token::Arrow)),
			SingleLexer::new(r"^-", |_| Some(Token::Minus)),
			SingleLexer::new(r"^((\*\*)|\^)", |_| Some(Token::Exp)),
			SingleLexer::new(r"^\*", |_| Some(Token::Mul)),
			SingleLexer::new(r"^/", |_| Some(Token::Div)),
			SingleLexer::new(r"^\(", |_| Some(Token::Lparen)),
			SingleLexer::new(r"^\)", |_| Some(Token::Rparen)),
			SingleLexer::new(r"^\[", |_| Some(Token::Lsquare)),
			SingleLexer::new(r"^\]", |_| Some(Token::Rsquare)),
			SingleLexer::new(r"^\{", |_| Some(Token::Lcurly)),
			SingleLexer::new(r"^\}", |_| Some(Token::Rcurly)),
			SingleLexer::new(r"^,", |_| Some(Token::Comma)),
			SingleLexer::new(r"^;", |_| Some(Token::Semicolon)),
			SingleLexer::new(r"^\?", |_| Some(Token::Question)),
			SingleLexer::new(r"^:", |_| Some(Token::Colon)),
			// Value literals
			SingleLexer::new(r"^((\.\d+)|(0|[1-9]\d*)(\.\d+)?)", |match_text| Some(Token::Primitive(Primitive::Number(BigDecimal::from_str(match_text).unwrap())))),
			SingleLexer::new(keyword!("true"), |_| Some(Token::Primitive(Primitive::Boolean(Boolean::True)))),
			SingleLexer::new(keyword!("false"), |_| Some(Token::Primitive(Primitive::Boolean(Boolean::False)))),
			SingleLexer::new(r#"^("([^"\\]|\\["\\])*")"#, |match_text| {
				// Strip enclosing quotes and escape characters.
				Some(Token::from_string(match_text[1..match_text.len() - 1].replace(r#"\""#, r#"""#).replace(r"\\", r"\")))
			}),
			// Intrinsic functions
			SingleLexer::new(keyword!("top"), |_| Some(Token::Intrinsic(Intrinsic::Top))),
			SingleLexer::new(keyword!("pop"), |_| Some(Token::Intrinsic(Intrinsic::Pop))),
			SingleLexer::new(keyword!("push"), |_| Some(Token::Intrinsic(Intrinsic::Push))),
			SingleLexer::new(keyword!("print"), |_| Some(Token::Intrinsic(Intrinsic::Print))),
			SingleLexer::new(keyword!("read"), |_| Some(Token::Intrinsic(Intrinsic::Read))),
			// Keywords
			SingleLexer::new(keyword!("import"), |_| Some(Token::Import)),
			SingleLexer::new(keyword!("let"), |_| Some(Token::Let)),
			SingleLexer::new(keyword!("if"), |_| Some(Token::If)),
			SingleLexer::new(keyword!("then"), |_| Some(Token::Then)),
			SingleLexer::new(keyword!("else"), |_| Some(Token::Else)),
			SingleLexer::new(keyword!("while"), |_| Some(Token::While)),
			SingleLexer::new(keyword!("for"), |_| Some(Token::For)),
			SingleLexer::new(keyword!("in"), |_| Some(Token::In)),
			SingleLexer::new(keyword!("do"), |_| Some(Token::Do)),
			SingleLexer::new(keyword!("return"), |_| Some(Token::Return)),
			SingleLexer::new(keyword!("and"), |_| Some(Token::And)),
			SingleLexer::new(keyword!("or"), |_| Some(Token::Or)),
			SingleLexer::new(keyword!("not"), |_| Some(Token::Not)),
			// Symbol
			SingleLexer::new(r"^[a-zA-Z_]+", |match_text| Some(Token::Symbol(Symbol{ name: match_text.to_string() })))
		];
	}

	let mut next_input = input;
	let mut tokens: Vec<Token> = Vec::new();
	while !next_input.is_empty() {
		println!("Input: {}", next_input);
		let mut no_matches = true;
		for single_lexer in SINGLE_LEXERS.iter() {
			if let Some(match_result) = single_lexer.regex.find(next_input).unwrap() {
				println!("Matched '{}' at index {}", match_result.as_str(), match_result.start());
				if let Some(token) = (single_lexer.generator)(match_result.as_str()) {
					println!("Found token {}", token.to_string());
					tokens.push(token);
				}
				next_input = &next_input[match_result.end()..];
				no_matches = false;
				break;
			}
		}
		if no_matches {
			return Err(format!("unrecognized token: '{}'", Regex::new(r"\W+").unwrap().find(next_input).unwrap().unwrap().as_str()));
		}
	}
	Ok(tokens)
}

// Generates the token from a pattern, if there is one.
type Generator = fn(&str) -> Option<Token>;

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
	use crate::lexer::lex;
	use crate::tokens::Token;

	#[test]
	fn whitespace() -> Result<(), String> {
		let expected: Vec<Token> = vec!(Token::from(1), Token::Plus, Token::from(2), Token::Plus, Token::from(3));
		let actual = lex(" \t \n 1 \n + \t 2+3 \t \n ")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn numbers_operators_and_separators() -> Result<(), String> {
		let expected = vec!(
			Token::Let,
			Token::Eq,
			Token::Neq,
			Token::Lt,
			Token::Leq,
			Token::Gt,
			Token::Geq,
			Token::Approx,
			Token::Mul,
			Token::Lparen,
			Token::Plus,
			Token::Minus,
			Token::Arrow,
			Token::Rparen,
			Token::Lsquare,
			Token::Rsquare,
			Token::Exp,
			Token::Exp,
			Token::Mul,
			Token::Div,
			Token::from(0.1),
			Token::from(0),
			Token::from(0.1),
			Token::Comma,
			Token::Question,
			Token::Colon,
		);
		let actual = lex("let=!=<<=>>=~*(+-->)[]^***/.1 0 0.1,?:")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn line_comments() -> Result<(), String> {
		let expected = vec!(Token::from(1), Token::Plus, Token::from(2));
		let actual = lex("1+2 // This is a line comment.")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn key_words() -> Result<(), String> {
		let expected = vec!(
			Token::Import, Token::from(1), Token::from_symbol("imports"),
			Token::If, Token::from(1), Token::from_symbol("ifs"),
			Token::Then, Token::from(1), Token::from_symbol("thens"),
			Token::Else, Token::from(1), Token::from_symbol("elses"),
			Token::While, Token::from(1), Token::from_symbol("whiles"),
			Token::For, Token::from(1), Token::from_symbol("fors"),
			Token::In, Token::from(1), Token::from_symbol("ins"),
			Token::Do, Token::from(1), Token::from_symbol("dos"),
			Token::Return, Token::from(1), Token::from_symbol("returns"),
			Token::And, Token::from(1), Token::from_symbol("ands"),
			Token::Or, Token::from(1), Token::from_symbol("ors"),
			Token::Not, Token::from(1), Token::from_symbol("nots"),
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
	fn valid_strings() -> Result<(), String> {
		assert_eq!(Token::from_string(""), lex(r#""""#)?[0]);
		assert_eq!(Token::from_string("abc"), lex(r#""abc""#)?[0]);
		assert_eq!(Token::from_string(r#""abc""#), lex(r#""\"abc\"""#)?[0]);
		assert_eq!(Token::from_string(r#"a\b\c"#), lex(r#""a\\b\\c""#)?[0]);
		Ok(())
	}

	#[test]
	fn invalid_strings() -> Result<(), String> {
		assert!(lex(r#"""#).is_err());
		assert!(lex(r#"""""#).is_err());
		assert!(lex(r#""\""#).is_err());
		assert!(lex(r#""\a""#).is_err());
		Ok(())
	}
}
