use super::env::Env;
use super::exprs::Lambda;
use super::number::Number;
use super::primitives::{Primitive, Boolean};
use super::symbol::Symbol;

use num_traits::cast::ToPrimitive;
use itertools::Itertools;

use std::boxed::Box;
use std::cell::RefCell;
use std::rc::Rc;

/// Functional linked list of Gynjo values.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum List {
	Empty,
	Cons { head: Box<Value>, tail: Rc<List> },
}

impl List {
	pub fn to_string(&self, env: &Rc<RefCell<Env>>) -> String {
		format!("[{}]", self.iter().map(|elem| elem.to_string(&env)).join(", "))
	}

	/// A `ref` iterator over this list.
	pub fn iter(&self) -> ListIter {
		ListIter(self)
	}
}

pub struct ListIter<'a>(&'a List);

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

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Tuple {
	pub elems: Box<Vec<Value>>,
}

/// Convenience macro for turning a comma-separated list of values into a value tuple.
#[macro_export]
macro_rules! make_tuple {
	($($exprs:expr),*) => {
		Value::Tuple(Tuple { elems: Box::new(vec!($($exprs),*)) })
	}
}

impl Tuple {
	pub fn empty() -> Tuple {
		Tuple { elems: Box::new(Vec::new()) }
	}

	pub fn to_string(&self, env: &Rc<RefCell<Env>>) -> String {
		format!("({})", self.elems.iter().map(|elem| elem.to_string(env)).join(", "))
	}
}

#[derive(Clone, Debug)]
pub struct Closure {
	pub f: Lambda,
	pub env: Rc<RefCell<Env>>,
}

impl PartialEq for Closure {
	fn eq(&self, other: &Self) -> bool {
		self.f == other.f
	}
}

impl Eq for Closure {}

/// Gynjo value types.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Value {
	Primitive(Primitive),
	Tuple(Tuple),
	List(List),
	Closure(Closure),
}

impl From<bool> for Value {
	fn from(b: bool) -> Value {
		Value::Primitive(Primitive::from(b))
	}
}

impl From<Boolean> for Value {
	fn from(boolean: Boolean) -> Value {
		Value::Primitive(Primitive::Boolean(boolean))
	}
}

impl From<Number> for Value {
	fn from(number: Number) -> Value {
		Value::Primitive(Primitive::Number(number))
	}
}

impl From<i64> for Value {
	fn from(n: i64) -> Value {
		Value::Primitive(Primitive::from(n))
	}
}

impl From<f64> for Value {
	fn from(n: f64) -> Value {
		Value::Primitive(Primitive::from(n))
	}
}

impl From<String> for Value {
	fn from(s: String) -> Value {
		Value::Primitive(Primitive::String(s))
	}
}

impl Value {
	/// Converts this value to a user-readable string.
	/// `env` - Used for values whose string representation is environment-dependent.
	pub fn to_string(&self, env: &Rc<RefCell<Env>>) -> String {
		match self {
			// Can't just use Primitive::to_string() because Value::to_string() needs to respect the current precision.
			Value::Primitive(primitive) => match primitive {
				Primitive::Boolean(b) => b.to_string(),
				Primitive::Number(number) => match number {
					Number::Integer(integer) => integer.to_string(),
					Number::Rational(rational) => rational.to_string(),
					Number::Real(real) => {
						let precision = env.borrow()
							// Look up precision setting.
							.lookup(&Symbol { name: "precision".to_string() })
							// Interpret as an integer.
							.and_then(|v| v.as_i64())
							// Interpret as a non-negative integer.
							.and_then(|v| if v < 0 { None } else { Some(v as u64) })
							// If something failed, use default precision setting.
							.unwrap_or(12);
						format!("{}", real.with_prec(precision))
					}
				},
				Primitive::String(s) => format!("\"{}\"", s),
			},
			Value::Tuple(tuple) => tuple.to_string(&env),
			Value::List(list) => list.to_string(&env),
			Value::Closure(c) => c.f.to_string(),
		}
	}

	/// Converts this value to `i64` if it's integral, otherwise returns `None`.
	pub fn as_i64(&self) -> Option<i64> {
		if let Value::Primitive(Primitive::Number(Number::Integer(integer))) = self {
			integer.to_i64()
		} else {
			None
		}
	}
}
