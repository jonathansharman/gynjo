use super::intrinsics::Intrinsic;
use super::primitives::Prim;
use super::symbol::Sym;
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
	/// Converts this token to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Tok::Import => "import".to_string(),
			Tok::Let => "let".to_string(),
			Tok::If => "if".to_string(),
			Tok::Then => "then".to_string(),
			Tok::Else => "else".to_string(),
			Tok::While => "while".to_string(),
			Tok::For => "for".to_string(),
			Tok::In => "in".to_string(),
			Tok::Do => "do".to_string(),
			Tok::Return => "return".to_string(),
			Tok::And => "and".to_string(),
			Tok::Or => "or".to_string(),
			Tok::Not => "not".to_string(),
			Tok::Eq => "=".to_string(),
			Tok::Neq => "!=".to_string(),
			Tok::Approx => "~".to_string(),
			Tok::Lt => "<".to_string(),
			Tok::Leq => "<=".to_string(),
			Tok::Gt => ">".to_string(),
			Tok::Geq => ">=".to_string(),
			Tok::Plus => "+".to_string(),
			Tok::Minus => "-".to_string(),
			Tok::Mul => "*".to_string(),
			Tok::Div => "/".to_string(),
			Tok::Exp => "^".to_string(),
			Tok::Lparen => "(".to_string(),
			Tok::Rparen => ")".to_string(),
			Tok::Lsquare => "[".to_string(),
			Tok::Rsquare => "]".to_string(),
			Tok::Lcurly => "{".to_string(),
			Tok::Rcurly => "}".to_string(),
			Tok::Comma => ",".to_string(),
			Tok::Semicolon => ";".to_string(),
			Tok::Arrow => "->".to_string(),
			Tok::Question => "?".to_string(),
			Tok::Colon => ":".to_string(),
			Tok::Intrinsic(f) => f.name(),
			Tok::Sym(symbol) => symbol.to_string(),
			Tok::Prim(primitive) => primitive.to_string(),
		}
	}

	pub fn from_string<S>(s: S) -> Tok where S: Into<String> {
		Tok::Prim(Prim::from(s.into()))
	}

	pub fn from_symbol<S>(s: S) -> Tok where S: Into<String> {
		Tok::Sym(Sym::from(s))
	}
}
