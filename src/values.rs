use super::env::Env;
use super::exprs::Lambda;
use super::literals::{Literal, Boolean};
use super::symbol::Symbol;

use num_traits::cast::ToPrimitive;
use itertools::Itertools;

use std::boxed::Box;
use std::rc::Rc;

/// Functional linked list of Gynjo values.
#[derive(Clone, Eq, PartialEq)]
pub enum List {
	Empty,
	Cons { head: Box<Value>, tail: Rc<List> },
}

impl List {
	pub fn to_string(&self, env: &Rc<Env>) -> String {
		format!("[{}]", self.iter().map(|elem| elem.to_string(&env)).join(", "))
	}

	/// A `ref` iterator over this list.
	pub fn iter(&self) -> ListIter {
		ListIter(self)
	}
}

struct ListIter<'a>(&'a List);

impl <'a> Iterator for ListIter<'a> {
	type Item = &'a Value;

	fn next(&mut self) -> Option<Self::Item> {
		match self.0 {
			List::Empty => None,
			List::Cons { head, tail } => {
				self.0 = tail;
				Some(head)
			}
		}
	}
}

/// Convenience macro for turning a comma-separated list of values into a linked value list.
#[macro_export]
macro_rules! make_list {
	() => { List::Empty };
	($head:expr $(, $tail:expr)*) => {
		List::Cons {
			head: Box::new($head),
			tail: Rc::new(make_list!($($tail),*)),
		}
	}
}

#[derive(Clone, Eq, PartialEq)]
pub struct Tuple {
	pub elems: Box<Vec<Value>>,
}

impl Tuple {
	pub fn empty() -> Tuple {
		Tuple { elems: Box::new(Vec::new()) }
	}

	pub fn to_string(&self, env: &Rc<Env>) -> String {
		format!("({})", self.elems.iter().map(|elem| elem.to_string(env)).join(", "))
	}
}

#[derive(Clone)]
pub struct Closure {
	pub f: Lambda,
	pub env: Rc<Env>,
}

impl PartialEq for Closure {
	fn eq(&self, other: &Self) -> bool {
		self.f == other.f
	}
}

impl Eq for Closure {}

/// Sum type of all Gynjo values.
#[derive(Clone, Eq, PartialEq)]
pub enum Value {
	Literal(Literal),
	Tuple(Tuple),
	List(List),
	Closure(Closure),
}

impl From<Boolean> for Value {
	fn from(boolean: Boolean) -> Value {
		Value::Literal(Literal::Boolean(boolean))
	}
}

impl Value {
	/// Converts this value to a user-readable string.
	/// `env` - Used for values whose string representation is environment-dependent.
	pub fn to_string(&self, env: &Rc<Env>) -> String {
		match self {
			// Can't just use Literal::to_string() because Value::to_string() needs to respect the current precision.
			Value::Literal(literal) => match literal {
				Literal::Boolean(b) => b.to_string(),
				Literal::Number(number) => {
					let precision = env
						// Look up precision setting.
						.lookup(&Symbol { name: "precision".to_string() })
						// Interpret as an integer.
						.and_then(|v| v.as_i64())
						// Interpret as a non-negative integer.
						.and_then(|v| if v < 0 { None } else { Some(v as u64) })
						// If something failed, use default precision setting.
						.unwrap_or(12);
					format!("{}", number.with_prec(precision))
				},
				Literal::String(value) => value.clone(),
			},
			Value::Tuple(tuple) => tuple.to_string(&env),
			Value::List(list) => list.to_string(&env),
			Value::Closure(c) => c.f.to_string(),
		}
	}

	/// Converts this value to `i64` if it's integral, otherwise returns `None`.
	fn as_i64(&self) -> Option<i64> {
		if let Value::Literal(Literal::Number(number)) = self {
			let (mantissa, exponent) = number.clone().into_bigint_and_exponent();
			if exponent == 0 {
				mantissa.to_i64()
			} else {
				None
			}
		} else {
			None
		}
	}
}
