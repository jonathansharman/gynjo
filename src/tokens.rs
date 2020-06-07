use super::intrinsics::Intrinsic;
use super::primitives::Prim;
use super::symbol::Sym;

use std::fmt;

/// Gynjo tokens.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Tok {
	Import,
	Let,
	// Branch
	If,
	Then,
	Else,
	// Loops/blocks
	While,
	For,
	In,
	Do,
	Return,
	// Boolean ops
	And,
	Or,
	Not,
	// Comparison ops
	Eq,
	Neq,
	Lt,
	Leq,
	Gt,
	Geq,
	Approx,
	// Arithmetic ops
	Plus,
	Minus,
	Mul,
	Div,
	Exp,
	// Brackets
	Lparen,
	Rparen,
	Lsquare,
	Rsquare,
	Lcurly,
	Rcurly,
	// Punctuation
	Comma,
	Semicolon,
	Arrow,
	Question,
	Colon,
	// Intrinsic function
	Intrinsic(Intrinsic),
	// Symbol
	Sym(Sym),
	// Primitive
	Prim(Prim),
}

impl From<bool> for Tok {
	fn from(b: bool) -> Tok {
		Tok::Prim(Prim::from(b))
	}
}

impl From<i64> for Tok {
	fn from(n: i64) -> Tok {
		Tok::Prim(Prim::from(n))
	}
}

impl From<f64> for Tok {
	fn from(n: f64) -> Tok {
		Tok::Prim(Prim::from(n))
	}
}

impl Tok {
	pub fn from_string<S>(s: S) -> Tok where S: Into<String> {
		Tok::Prim(Prim::from(s.into()))
	}

	pub fn from_symbol<S>(s: S) -> Tok where S: Into<String> {
		Tok::Sym(Sym::from(s))
	}
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Tok::Import => write!(f, "import"),
			Tok::Let => write!(f, "let"),
			Tok::If => write!(f, "if"),
			Tok::Then => write!(f, "then"),
			Tok::Else => write!(f, "else"),
			Tok::While => write!(f, "while"),
			Tok::For => write!(f, "for"),
			Tok::In => write!(f, "in"),
			Tok::Do => write!(f, "do"),
			Tok::Return => write!(f, "return"),
			Tok::And => write!(f, "and"),
			Tok::Or => write!(f, "or"),
			Tok::Not => write!(f, "not"),
			Tok::Eq => write!(f, "="),
			Tok::Neq => write!(f, "!="),
			Tok::Approx => write!(f, "~"),
			Tok::Lt => write!(f, "<"),
			Tok::Leq => write!(f, "<="),
			Tok::Gt => write!(f, ">"),
			Tok::Geq => write!(f, ">="),
			Tok::Plus => write!(f, "+"),
			Tok::Minus => write!(f, "-"),
			Tok::Mul => write!(f, "*"),
			Tok::Div => write!(f, "/"),
			Tok::Exp => write!(f, "^"),
			Tok::Lparen => write!(f, "("),
			Tok::Rparen => write!(f, ")"),
			Tok::Lsquare => write!(f, "["),
			Tok::Rsquare => write!(f, "]"),
			Tok::Lcurly => write!(f, "{{"),
			Tok::Rcurly => write!(f, "}}"),
			Tok::Comma => write!(f, ","),
			Tok::Semicolon => write!(f, ";"),
			Tok::Arrow => write!(f, "->"),
			Tok::Question => write!(f, "?"),
			Tok::Colon => write!(f, ":"),
			Tok::Intrinsic(intrinsic) => write!(f, "{}", intrinsic),
			Tok::Sym(symbol) => write!(f, "{}", symbol),
			Tok::Prim(primitive) => write!(f, "{}", primitive),
		}
    }
}
