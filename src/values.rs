use super::env::Env;
use super::exprs::Lambda;
use super::number::Num;
use super::primitives::{Prim, Bool};
use super::symbol::Sym;
use super::types::Type;

use num_traits::cast::ToPrimitive;
use itertools::Itertools;

use std::boxed::Box;
use std::cell::RefCell;
use std::rc::Rc;

/// Functional linked list of Gynjo values.
#[derive(Clone, Eq, PartialEq, Debug)]
pub enum List {
	Empty,
	Cons { head: Box<Val>, tail: Rc<List> },
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
	type Item = &'a Val;

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
	pub elems: Box<Vec<Val>>,
}

/// Convenience macro for turning a comma-separated list of values into a value tuple.
#[macro_export]
macro_rules! make_tuple {
	($($exprs:expr),*) => {
		Val::Tuple(Tuple { elems: Box::new(vec!($($exprs),*)) })
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
pub enum Val {
	Prim(Prim),
	Tuple(Tuple),
	List(List),
	Closure(Closure),
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
			},
			Val::Tuple(_) => Type::Tuple,
			Val::List(list) => match list {
				List::Empty => Type::Empty,
				List::Cons { .. } => Type::Cons,
			},
			Val::Closure(_) => Type::Closure,
		}
	}

	/// Converts this value to a user-readable string.
	/// `env` - Used for values whose string representation is environment-dependent.
	pub fn to_string(&self, env: &Rc<RefCell<Env>>) -> String {
		match self {
			// Can't just use Primitive::to_string() because Value::to_string() needs to respect the current precision.
			Val::Prim(primitive) => match primitive {
				Prim::Bool(b) => b.to_string(),
				Prim::Num(number) => match number {
					Num::Integer(integer) => integer.to_string(),
					Num::Rational(rational) => rational.to_string(),
					Num::Real(real) => {
						let precision = env.borrow()
							// Look up precision setting.
							.lookup(&Sym { name: "precision".to_string() })
							// Interpret as an integer.
							.and_then(|v| v.as_i64())
							// Interpret as a non-negative integer.
							.and_then(|v| if v < 0 { None } else { Some(v as u64) })
							// If something failed, use default precision setting.
							.unwrap_or(12);
						format!("{}", real.with_prec(precision))
					}
				},
				Prim::String(s) => format!("\"{}\"", s),
			},
			Val::Tuple(tuple) => tuple.to_string(&env),
			Val::List(list) => list.to_string(&env),
			Val::Closure(c) => c.f.to_string(),
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
