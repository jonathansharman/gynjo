use super::{Closure, List, Quant, Range, Tuple};

use crate::env::SharedEnv;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::{Bool, Num, Prim, Type};

use std::boxed::Box;

/// Gynjo value types.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Val {
	Prim(Prim),
	Quant(Quant),
	Tuple(Tuple),
	List(List),
	Range(Box<Range>),
	Closure(Closure),
	Break,
	Return { result: Box<Val> },
}

impl From<bool> for Val {
	fn from(b: bool) -> Val {
		Val::Prim(Prim::from(b))
	}
}

impl From<Bool> for Val {
	fn from(boolean: Bool) -> Val {
		Val::Prim(Prim::Bool(boolean))
	}
}

impl From<Num> for Val {
	fn from(number: Num) -> Val {
		Val::Prim(Prim::Num(number))
	}
}

impl From<Quant> for Val {
	fn from(quant: Quant) -> Val {
		Val::Quant(quant)
	}
}

impl From<i64> for Val {
	fn from(n: i64) -> Val {
		Val::Prim(n.into())
	}
}

impl From<f64> for Val {
	fn from(n: f64) -> Val {
		Val::Prim(n.into())
	}
}

impl From<String> for Val {
	fn from(s: String) -> Val {
		Val::Prim(Prim::Text(s.into()))
	}
}

impl Val {
	/// An empty tuple value.
	pub fn empty() -> Val {
		Val::Tuple(Tuple::empty())
	}

	/// Constructs a dimensionless quantity value from `n`.
	pub fn scalar<N>(n: N) -> Val
	where
		N: Into<Num>,
	{
		Val::Quant(Quant::scalar(n.into()))
	}

	/// Retrives the Gynjo type of this value.
	pub fn get_type(&self) -> Type {
		match self {
			Val::Prim(prim) => match prim {
				Prim::Bool(_) => Type::Boolean,
				Prim::Num(number) => Type::Quant(number.get_type()),
				Prim::Text(_) => Type::Text,
				Prim::Type(_) => Type::Type,
			},
			Val::Quant(quant) => Type::Quant(quant.value().get_type()),
			Val::Tuple(_) => Type::Tuple,
			Val::List(_) => Type::List,
			Val::Range(_) => Type::Range,
			Val::Closure(_) => Type::Closure,
			Val::Break => Type::Break,
			Val::Return { .. } => Type::Return,
		}
	}

	/// Gets `self` as an `i64` if it's integral, otherwise returns `None`.
	pub fn as_i64(&self) -> Option<i64> {
		match self {
			Val::Prim(Prim::Num(n)) => n.as_i64(),
			Val::Quant(quant) => quant.as_i64(),
			_ => None,
		}
	}
}

impl FormatWithEnv for Val {
	fn format_with_env(&self, env: &SharedEnv) -> String {
		match self {
			// Can't just use Primitive::to_string() because Value::to_string() needs to respect the current precision.
			Val::Prim(primitive) => match primitive {
				Prim::Bool(b) => b.to_string(),
				Prim::Num(number) => number.format_with_env(env),
				Prim::Text(t) => t.to_string(),
				Prim::Type(t) => t.to_string(),
			},
			Val::Quant(quant) => quant.format_with_env(env),
			Val::Tuple(tuple) => tuple.format_with_env(env),
			Val::List(list) => list.format_with_env(env),
			Val::Range(range) => range.format_with_env(env),
			Val::Closure(c) => c.format_with_env(env),
			Val::Break => "break".to_string(),
			Val::Return { result } => format!("(return {})", result.format_with_env(env)),
		}
	}
}
