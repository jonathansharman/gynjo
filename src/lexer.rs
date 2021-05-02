use crate::errors::LexErr;
use crate::tokens::Tok;

use logos::Logos;

/// Result of lexing: either a vector of tokens or an error message.
type LexResult = Result<Vec<Tok>, LexErr>;

/// Lexes `input` into a vector of tokens, if possible.
pub fn lex(input: &str) -> LexResult {
	let mut tokens: Vec<Tok> = Vec::new();
	for (token, span) in Tok::lexer(input).spanned() {
		if token == Tok::Err {
			return Err(LexErr {
				unrecognized_token: input[span].to_string(),
			});
		} else {
			tokens.push(token)
		}
	}
	Ok(tokens)
}

#[cfg(test)]
mod tests {
	use crate::errors::LexErr;
	use crate::lexer::lex;
	use crate::primitives::Prim;
	use crate::symbol::Sym;
	use crate::tokens::Tok;

	impl Tok {
		fn from_string<S>(s: S) -> Tok
		where
			S: Into<String>,
		{
			Tok::Prim(Prim::from(s.into()))
		}

		fn from_symbol<S>(s: S) -> Tok
		where
			S: Into<String>,
		{
			Tok::Sym(Sym::from(s))
		}
	}

	#[test]
	fn whitespace() -> Result<(), LexErr> {
		let expected: Vec<Tok> = vec![
			Tok::from(1),
			Tok::Plus,
			Tok::from(2),
			Tok::Plus,
			Tok::from(3),
		];
		let actual = lex(" \t \n 1 \n + \t 2+3 \t \n ")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn numbers_operators_and_separators() -> Result<(), LexErr> {
		let expected = vec![
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
			Tok::Concat,
		];
		let actual = lex("let=!=<<=>>=~*(+-->)[]^***/.1 0 0.1,?:|")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn line_comments() -> Result<(), LexErr> {
		let expected = vec![Tok::from(1), Tok::Plus, Tok::from(2)];
		let actual = lex("1+2 // This is a line comment.")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn symbol_with_keyword_prefix() -> Result<(), LexErr> {
		let expected = vec![
			Tok::Import,
			Tok::from(1),
			Tok::from_symbol("importa"),
			Tok::If,
			Tok::from(1),
			Tok::from_symbol("ifb"),
			Tok::Then,
			Tok::from(1),
			Tok::from_symbol("thenc"),
		];
		let actual = lex("import1 importa if1 ifb then1 thenc")?;
		assert_eq!(expected, actual);
		Ok(())
	}

	#[test]
	fn valid_strings() -> Result<(), LexErr> {
		assert_eq!(Tok::from_string(""), lex(r#""""#)?[0]);
		assert_eq!(Tok::from_string("abc"), lex(r#""abc""#)?[0]);
		assert_eq!(Tok::from_string(r#""abc""#), lex(r#""\"abc\"""#)?[0]);
		assert_eq!(Tok::from_string(r#"a\b\c"#), lex(r#""a\\b\\c""#)?[0]);
		Ok(())
	}

	#[test]
	fn invalid_strings() -> Result<(), LexErr> {
		assert!(lex(r#"""#).is_err());
		assert!(lex(r#"""""#).is_err());
		assert!(lex(r#""\""#).is_err());
		assert!(lex(r#""\a""#).is_err());
		Ok(())
	}
}
