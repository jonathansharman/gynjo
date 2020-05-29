use super::intrinsics::Intrinsic;
use super::primitives::Primitive;
use super::symbol::Symbol;
/// Sum type of all valid Gynjo tokens.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Token {
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
	Symbol(Symbol),
	// Primitive
	Primitive(Primitive),
}

impl From<i32> for Token {
	fn from(n: i32) -> Token {
		Token::Primitive(Primitive::from(n))
	}
}

impl From<f32> for Token {
	fn from(n: f32) -> Token {
		Token::Primitive(Primitive::from(n))
	}
}

impl Token {
	/// Converts this token to a user-readable string.
	pub fn to_string(&self) -> String {
		match self {
			Token::Import => "import".to_string(),
			Token::Let => "let".to_string(),
			Token::If => "if".to_string(),
			Token::Then => "then".to_string(),
			Token::Else => "else".to_string(),
			Token::While => "while".to_string(),
			Token::For => "for".to_string(),
			Token::In => "in".to_string(),
			Token::Do => "do".to_string(),
			Token::Return => "return".to_string(),
			Token::And => "and".to_string(),
			Token::Or => "or".to_string(),
			Token::Not => "not".to_string(),
			Token::Eq => "=".to_string(),
			Token::Neq => "!=".to_string(),
			Token::Approx => "~".to_string(),
			Token::Lt => "<".to_string(),
			Token::Leq => "<=".to_string(),
			Token::Gt => ">".to_string(),
			Token::Geq => ">=".to_string(),
			Token::Plus => "+".to_string(),
			Token::Minus => "-".to_string(),
			Token::Mul => "*".to_string(),
			Token::Div => "/".to_string(),
			Token::Exp => "^".to_string(),
			Token::Lparen => "(".to_string(),
			Token::Rparen => ")".to_string(),
			Token::Lsquare => "[".to_string(),
			Token::Rsquare => "]".to_string(),
			Token::Lcurly => "{".to_string(),
			Token::Rcurly => "}".to_string(),
			Token::Comma => ",".to_string(),
			Token::Semicolon => ";".to_string(),
			Token::Arrow => "->".to_string(),
			Token::Question => "?".to_string(),
			Token::Colon => ":".to_string(),
			Token::Intrinsic(f) => f.name(),
			Token::Symbol(symbol) => symbol.to_string(),
			Token::Primitive(primitive) => primitive.to_string(),
		}
	}

	pub fn from_string<S>(s: S) -> Token where S: Into<String> {
		Token::Primitive(Primitive::from(s.into()))
	}

	pub fn from_symbol<S>(s: S) -> Token where S: Into<String> {
		Token::Symbol(Symbol::from(s))
	}
}
