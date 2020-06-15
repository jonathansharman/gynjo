use super::errors::LexError;
use super::tokens::Tok;

use logos::Logos;

/// Result of lexing: either a vector of tokens or an error message.
type LexResult = Result<Vec<Tok>, LexError>;

/// Lexes `input` into a vector of tokens, if possible.
pub fn lex(input: &str) -> LexResult {
	let mut tokens: Vec<Tok> = Vec::new();
	for (token, span) in Tok::lexer(input).spanned() {
		if token == Tok::Error {
			return Err(LexError { unrecognized_token: input[span].to_string() });
		} else {
			tokens.push(token)
		}
	}
	Ok(tokens)
}

#[cfg(test)]
mod tests {
	use crate::errors::LexError;
	use crate::lexer::lex;
	use crate::primitives::Prim;
	use crate::symbol::Sym;
	use crate::tokens::Tok;
	use crate::types::{Type, ListType};

	impl Tok {
		fn from_string<S>(s: S) -> Tok where S: Into<String> {
			Tok::Prim(Prim::from(s.into()))
		}
	
		fn from_symbol<S>(s: S) -> Tok where S: Into<String> {
			Tok::Sym(Sym::from(s))
		}
	}
	
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
			Tok::Prim(Prim::Type(Type::Type)), Tok::from(1), Tok::from_symbol("types"),
			Tok::Prim(Prim::Type(Type::Boolean)), Tok::from(1), Tok::from_symbol("booleans"),
			Tok::Prim(Prim::Type(Type::Integer)), Tok::from(1), Tok::from_symbol("integers"),
			Tok::Prim(Prim::Type(Type::Rational)), Tok::from(1), Tok::from_symbol("rationals"),
			Tok::Prim(Prim::Type(Type::Real)), Tok::from(1), Tok::from_symbol("reals"),
			Tok::Prim(Prim::Type(Type::String)), Tok::from(1), Tok::from_symbol("strings"),
			Tok::Prim(Prim::Type(Type::Tuple)), Tok::from(1), Tok::from_symbol("tuples"),
			Tok::Prim(Prim::Type(Type::List(ListType::Empty))), Tok::from(1), Tok::from_symbol("empty_lists"),
			Tok::Prim(Prim::Type(Type::List(ListType::Cons))), Tok::from(1), Tok::from_symbol("nonempty_lists"),
			Tok::Prim(Prim::Type(Type::Closure)), Tok::from(1), Tok::from_symbol("closures"),
			Tok::Prim(Prim::Type(Type::Returned)), Tok::from(1), Tok::from_symbol("returned_values"),
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
			type1 types
			boolean1 booleans
			integer1 integers
			rational1 rationals
			real1 reals
			string1 strings
			tuple1 tuples
			empty_list1 empty_lists
			nonempty_list1 nonempty_lists
			closure1 closures
			returned_value1 returned_values
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
