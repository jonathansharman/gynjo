use super::env::SharedEnv;
use super::exprs::Lambda;
use super::list::List;
use super::numbers::Num;
use super::primitives::{Prim, Bool};
use super::symbol::Sym;
use super::types::Type;
use super::tuple::Tuple;

use bigdecimal::BigDecimal;
use num_traits::cast::ToPrimitive;
use num_traits::sign::Signed;

use std::boxed::Box;

#[derive(Clone, Debug)]
pub struct Closure {
	pub f: Lambda,
	pub env: SharedEnv,
}

impl PartialEq for Closure {
	fn eq(&self, other: &Self) -> bool {
		self.f == other.f
	}
}

impl Eq for Closure {}

/// Gynjo value types.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Val {
	Prim(Prim),
	Tuple(Tuple),
	List(List),
	Closure(Closure),
	Returned { result: Box<Val> },
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
		Val::Prim(Prim::String(s))
	}
}

impl Val {
	/// An empty tuple value.
	pub fn empty() -> Val {
		Val::Tuple(Tuple::empty())
	}

	/// Retrives the type of this value.
	pub fn get_type(&self) -> Type {
		match self {
			Val::Prim(prim) => match prim {
				Prim::Bool(_) => Type::Boolean,
				Prim::Num(number) => match number {
					Num::Integer(_) => Type::Integer,
					Num::Rational(_) => Type::Rational,
					Num::Real(_) => Type::Real,
				},
				Prim::String(_) => Type::String,
				Prim::Type(_) => Type::Type,
			},
			Val::Tuple(_) => Type::Tuple,
			Val::List(_) => Type::List,
			Val::Closure(_) => Type::Closure,
			Val::Returned { .. } => Type::Returned,
		}
	}

	/// Converts this value to a user-readable string.
	/// `env` - Used for values whose string representation is environment-dependent.
	pub fn to_string(&self, env: &SharedEnv) -> String {
		match self {
			// Can't just use Primitive::to_string() because Value::to_string() needs to respect the current precision.
			Val::Prim(primitive) => match primitive {
				Prim::Bool(b) => b.to_string(),
				Prim::Num(number) => match number {
					Num::Integer(integer) => integer.to_string(),
					Num::Rational(rational) => {
						// Rational numbers are displayed both in rational and real form.
						let real_string = Val::from(Num::Real(BigDecimal::from(rational.numer().clone()) / BigDecimal::from(rational.denom().clone()))).to_string(env);
						// Rationals are displayed in proper form.
						let whole_part = rational.trunc();
						let fractional_part = rational.fract();
						if whole_part == rational.clone() {
							// No fractional part.
							format!("{} ({})", whole_part, real_string)
						} else if fractional_part == rational.clone() {
							// No whole part.
							format!("{} ({})", fractional_part, real_string)
						} else {
							// Whole and fractional parts. Ensure fractional part is displayed as positive.
							format!("{} {} ({})", whole_part, fractional_part.abs(), real_string)
						}
					},
					Num::Real(real) => {
						let precision = env.lock().unwrap()
							// Look up precision setting.
							.lookup(&Sym { name: "precision".to_string() })
							// Interpret as an integer.
							.and_then(|v| v.as_i64())
							// Interpret as a non-negative integer.
							.and_then(|v| if v < 1 { None } else { Some(v as u64) })
							// If something failed, use default precision setting.
							.unwrap_or(12);
						format!("{}", real.with_prec(precision))
					},
				},
				Prim::String(s) => format!("\"{}\"", s),
				Prim::Type(t) => t.to_string(),
			},
			Val::Tuple(tuple) => tuple.to_string(&env),
			Val::List(list) => list.to_string(&env),
			Val::Closure(c) => c.f.to_string(),
			Val::Returned { result } => format!("(result: {})", result.to_string(&env)),
		}
	}

	/// Converts this value to `i64` if it's integral, otherwise returns `None`.
	pub fn as_i64(&self) -> Option<i64> {
		if let Val::Prim(Prim::Num(Num::Integer(integer))) = self {
			integer.to_i64()
		} else {
			None
		}
	}
}
