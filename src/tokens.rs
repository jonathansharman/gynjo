use super::intrinsics::Intrinsic;
use super::numbers::Num;
use super::primitives::{Prim, Bool};
use super::symbol::Sym;
use super::types::Type;

use bigdecimal::BigDecimal;
use logos::Logos;
use num_bigint::BigInt;

use std::fmt;
use std::str::FromStr;

/// Gynjo tokens.
#[derive(Logos, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Tok {
	#[token("import")]
	Import,
	#[token("let")]
	Let,
	// Branch
	#[token("if")]
	If,
	#[token("then")]
	Then,
	#[token("else")]
	Else,
	// Loops/blocks
	#[token("while")]
	While,
	#[token("for")]
	For,
	#[token("in")]
	In,
	#[token("do")]
	Do,
	#[token("break")]
	Break,
	#[token("return")]
	Return,
	// Type ops
	#[token("as")]
	As,
	#[token("get_type")]
	GetType,
	// Boolean ops
	#[token("and")]
	And,
	#[token("or")]
	Or,
	#[token("not")]
	Not,
	// Comparison ops
	#[token("=")]
	Eq,
	#[token("!=")]
	Neq,
	#[token("<")]
	Lt,
	#[token("<=")]
	Leq,
	#[token(">")]
	Gt,
	#[token(">=")]
	Geq,
	#[token("~")]
	Approx,
	// Arithmetic ops
	#[token("+")]
	Plus,
	#[token("-")]
	Minus,
	#[token("*")]
	Mul,
	#[token("/")]
	Div,
	#[regex(r"(\*\*)|\^")]
	Exp,
	// Concatenation
	#[token("|")]
	Concat,
	// I/O ops
	#[token("read")]
	Read,
	#[token("write")]
	Write,
	// Brackets
	#[token("(")]
	Lparen,
	#[token(")")]
	Rparen,
	#[token("[")]
	Lsquare,
	#[token("]")]
	Rsquare,
	#[token("{")]
	Lcurly,
	#[token("}")]
	Rcurly,
	// Punctuation
	#[token(",")]
	Comma,
	#[token(";")]
	Semicolon,
	#[token("->")]
	Arrow,
	#[token("?")]
	Question,
	#[token(":")]
	Colon,
	// Intrinsic functions
	#[token("pop", |_| Some(Intrinsic::Pop))]
	Intrinsic(Intrinsic),
	// Symbol
	#[regex("[a-zA-Z_]+", |lex| Some(Sym::from(lex.slice())))]
	Sym(Sym),
	// Boolean literals
	#[token("true", |_| Some(Prim::Bool(Bool::True)))]
	#[token("false", |_| Some(Prim::Bool(Bool::False)))]
	// Types
	#[token("type", |_| Some(Prim::Type(Type::Type)))]
	#[token("boolean", |_| Some(Prim::Type(Type::Boolean)))]
	#[token("integer", |_| Some(Prim::Type(Type::Integer)))]
	#[token("rational", |_| Some(Prim::Type(Type::Rational)))]
	#[token("real", |_| Some(Prim::Type(Type::Real)))]
	#[token("string", |_| Some(Prim::Type(Type::String)))]
	#[token("tuple", |_| Some(Prim::Type(Type::Tuple)))]
	#[token("list", |_| Some(Prim::Type(Type::List)))]
	#[token("closure", |_| Some(Prim::Type(Type::Closure)))]
	#[token("returned_value", |_| Some(Prim::Type(Type::Returned)))]
	// Real literal
	#[regex(r"(0|([1-9]\d*))?\.\d+", |lex| Some(Prim::Num(Num::Real(BigDecimal::from_str(lex.slice()).unwrap()))))]
	// Integer literal
	#[regex(r"0|([1-9]\d*)", |lex| Some(Prim::Num(Num::Integer(BigInt::from_str(lex.slice()).unwrap()))))]
	// String literal
	#[regex(r#""([^"\\]|\\["\\])*""#, |lex| {
		// Strip enclosing quotes and escape characters.
		Some(Prim::from(lex.slice()[1..lex.slice().len() - 1].replace(r#"\""#, r#"""#).replace(r"\\", r"\")))
	})]
	Prim(Prim),
	#[token("\\")]
	LineContinuation,
	#[error]
	// Whitespace (ignored)
	#[regex(r"\s+", logos::skip)]
	// Line comment (ignored)
	#[regex(r"//.*", logos::skip)]
	Error,
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
			Tok::Break => write!(f, "break"),
			Tok::Return => write!(f, "return"),
			Tok::As => write!(f, "as"),
			Tok::GetType => write!(f, "get_type"),
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
			Tok::Concat => write!(f, "|"),
			Tok::Read => write!(f, "read"),
			Tok::Write => write!(f, "write"),
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
			Tok::Intrinsic(intrinsic) => intrinsic.fmt(f),
			Tok::Sym(symbol) => symbol.fmt(f),
			Tok::Prim(primitive) => primitive.fmt(f),
			Tok::LineContinuation => write!(f, "\\"),
			Tok::Error => write!(f, "error"),
		}
	}
}
