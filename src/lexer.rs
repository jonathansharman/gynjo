use super::intrinsics::Intrinsic;
use super::primitives::{Primitive, Boolean};
use super::symbol::Symbol;
use super::tokens::Token;

use bigdecimal::BigDecimal;
use regex::{Regex, RegexSet, Match};

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
			SingleLexer::new(r"^//.*", |_| None),
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
			SingleLexer::new(r"^(\*\*)|\^", |_| Some(Token::Exp)),
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
			SingleLexer::new(r"^(\.\d+)|(0|[1-9]\d*)(\.\d+)?", |match_text| Some(Token::Primitive(Primitive::Number(BigDecimal::from_str(match_text).unwrap())))),
			SingleLexer::new(keyword!("true"), |_| Some(Token::Primitive(Primitive::Boolean(Boolean::True)))),
			SingleLexer::new(keyword!("false"), |_| Some(Token::Primitive(Primitive::Boolean(Boolean::False)))),
			SingleLexer::new(r#"^"([^"\\]|\\["\\])*""#, |match_text| {
				// Strip enclosing quotes and escape characters.
				Some(Token::Primitive(Primitive::String(match_text[1..match_text.len() - 1].replace(r"\\", r"\"))))
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
		static ref PATTERNS: Vec<&'static str> = SINGLE_LEXERS.iter().map(|single_lexer| single_lexer.pattern).collect();
		static ref REGEX_SET: RegexSet = RegexSet::new(PATTERNS.iter()).unwrap();
	}

	let mut next_input = input;
	let mut tokens: Vec<Token> = Vec::new();
	while !next_input.is_empty() {
		let matches = REGEX_SET.matches(next_input);
		if matches.matched_any() {
			let single_lexer = &SINGLE_LEXERS[matches.into_iter().next().unwrap()];
			// This search is guaranteed to succeed. Only doing this to find the match indices.
			let match_result: Match = single_lexer.regex.find(next_input).unwrap();
			if let Some(token) = (single_lexer.generator)(match_result.as_str()) {
				tokens.push(token);
			}
			next_input = &next_input[match_result.end()..];
		} else {
			return Err(format!("unrecognized token: '{}'", Regex::new(r"\W+").unwrap().find(next_input).unwrap().as_str()));
		}
	}
	Ok(tokens)
}

// Generates the token from a pattern, if there is one.
type Generator = fn(&str) -> Option<Token>;

/// Capable of matching and producing a single token.
struct SingleLexer {
	pattern: &'static str,
	regex: Regex,
	generator: Generator,
}

impl SingleLexer {
	fn new(pattern: &'static str, generator: Generator) -> SingleLexer {
		SingleLexer { pattern, regex: Regex::new(pattern).unwrap(), generator }
	}
}
