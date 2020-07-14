// Functional list implementation based on https://rust-unofficial.github.io/too-many-lists/third-final.html.

use super::Range;
use super::Val;
use super::Quant;

use crate::env::SharedEnv;
use crate::errors::RtErr;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::Num;

use itertools::Itertools;
use num_traits::cast::ToPrimitive;

use std::sync::Arc;

/// A functional linked list of Gynjo values.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct List {
	head: Link,
	len: usize,
}

type Link = Option<Arc<Node>>;

#[derive(Clone, Eq, PartialEq, Debug)]
struct Node {
	elem: Val,
	next: Link,
}

impl Node {
	/// Produces a new node by applying `f` to each element.
	pub fn map<F, E>(&self, mut f: F) -> Result<Node, E> where F: FnMut(&Val) -> Result<Val, E> {
		Ok(Node {
			elem: f(&self.elem)?,
			next: match self.next.clone() {
				Some(next) => Some(Arc::new(next.map(f)?)),
				None => None,
			},
		})
	}
}

impl List {
	/// An empty list.
	pub fn empty() -> Self {
		Self { head: None, len: 0 }
	}

	/// Whether this list has no elements.
	pub fn is_empty(&self) -> bool {
		self.head.is_none()
	}

	/// The number of elements in this list.
	pub fn len(&self) -> usize {
		self.len
	}

	/// A shallow copy of this list with `elem` added as the head.
	pub fn push(&self, elem: Val) -> Self {
		Self {
			head: Some(Arc::new(Node { elem, next: self.head.clone(), })),
			len: self.len + 1,
		}
	}

	/// A shallow copy of this list's tail.
	pub fn tail(&self) -> Option<Self> {
		self.head.as_ref().map(|node| Self { head: node.next.clone(), len: self.len - 1 })
	}

	/// A reference to the first element of this list, if there is one.
	pub fn head(&self) -> Option<&Val> {
		self.head.as_ref().map(|node| &node.elem)
	}

	/// Concatenates `self` with `other`. The head of `self` is the head of the result.
	pub fn concat(&self, other: Self) -> Self {
		match &self.head {
			Some(node) => Self { head: node.next.clone(), len: self.len - 1, }
				.concat(other)
				.push(node.elem.clone()),
			None => other,
		}
	}

	/// Creates a reference iterator into this list's elements.
	pub fn iter(&self) -> Iter<'_> {
		Iter { next: self.head.as_ref().map(|node| &**node) }
	}

	/// Produces a new list by applying `f` to each element.
	pub fn map<F, E>(&self, f: F) -> Result<Self, E> where F: FnMut(&Val) -> Result<Val, E> {
		Ok(Self {
			head: match self.head.clone() {
				Some(node) => Some(Arc::new(node.map(f)?)),
				None => None,
			},
			len: self.len,
		})
	}

	/// Gets the element of this list at index `idx` modulo the list length.
	///
	/// `env`: Used for formatting error messages if the index is not a scalar integer.
	/// `idx`: The index to retrieve. Must be an integer.
	pub fn nth(&self, env: &SharedEnv, idx: Quant) -> Result<Val, RtErr> {
		if self.is_empty() {
			return Err(RtErr::OutOfBounds);
		}
		let idx = idx.into_scalar().map_err(RtErr::quant)?;
		if let Num::Integer(idx) = idx {
			let idx = idx.to_i64().ok_or(RtErr::OutOfBounds)?;
			let signed_len = self.len() as i64;
			let mut idx = ((idx % signed_len) + signed_len) % signed_len;
			let mut iter = self.iter();
			let mut result = iter.next();
			while result.is_some() && idx != 0 {
				result = iter.next();
				idx -= 1;
			}
			result.map(|val| val.clone()).ok_or(RtErr::OutOfBounds)
		} else {
			Err(RtErr::InvalidIndex { idx: idx.format_with_env(&env) })
		}
	}

	/// Copies a slice from this list, based on `index`.
	///
	/// `env`: Used for formatting error messages if an error occurs.
	/// `index`: Either an single integral index or a `Range` of indices.
	///
	/// Empty lower/upper bounds use the beginning/end of the list.
	///
	/// Individual indices are copied modulo the length of the list.
	pub fn slice(&self, env: &SharedEnv, idx: List) -> Result<Val, RtErr> {
		if idx.len() != 1 {
			return Err(RtErr::InvalidIndex { idx: idx.format_with_env(&env) });
		}
		match idx.head().unwrap().clone() {
			Val::Quant(idx) => Ok(self.nth(&env, idx)?),
			Val::Range(range) => {
				let mut result = Self::empty();
				let (start, end, mut stride) = range.into_start_end_stride();
				let start = start.or(Some(Quant::scalar(Num::from(0))));
				let end = end.or(Some(Quant::scalar(Num::from((self.len() - 1) as i64))));
				// Check if the stride needs to be reversed.
				if let (Some(start), Some(end)) = (&start, &end) {
					if (start < end && stride.value().is_negative()) || (start > end && stride.value().is_positive()) {
						stride = -stride;
					}
				}
				let range = Range::new(start, end, Some(stride));
				// Iterate in reverse since lists are built last-in-first-out.
				for idx in range.reverse().into_iter() {
					result = result.push(self.nth(&env, idx.map_err(RtErr::quant)?)?);
				}
				Ok(Val::List(result))
			},
			invalid @ _ => Err(RtErr::InvalidIndex { idx: invalid.format_with_env(&env) }),
		}
	}

	/// A new list with this list's elements in reverse order.
	pub fn reverse(&self) -> Self {
		let mut result = Self::empty();
		for val in self.iter() {
			result = result.push(val.clone());
		}
		result
	}
}

impl Drop for List {
	fn drop(&mut self) {
		let mut head = self.head.take();
		while let Some(node) = head {
			if let Ok(mut node) = Arc::try_unwrap(node) {
				head = node.next.take();
			} else {
				break;
			}
		}
	}
}

pub struct Iter<'a> {
	next: Option<&'a Node>,
}

impl<'a> Iterator for Iter<'a> {
	type Item = &'a Val;

	fn next(&mut self) -> Option<Self::Item> {
		self.next.map(|node| {
			self.next = node.next.as_ref().map(|node| &**node);
			&node.elem
		})
	}
}

impl FormatWithEnv for List {
	fn format_with_env(&self, env: &SharedEnv) -> String {
		format!("[{}]", self.iter().map(|elem| elem.format_with_env(&env)).join(", "))
	}
}

/// Convenience macro for turning a comma-separated list of values into a list.
#[macro_export]
macro_rules! make_list {
	() => { List::empty() };
	($head:expr $(, $tail:expr)*) => {
		make_list!($($tail),*).push($head)
	}
}

/// Convenience macro for turning a comma-separated list of values into a list value.
#[macro_export]
macro_rules! make_list_value {
	($($values:expr),*) => {
		Val::List(make_list!($($values),*))
	}
}

#[cfg(test)]
mod tests {
	use super::super::val::Val;
	use super::super::list::List;
	#[test]
	fn head_and_tail() {
		let list = List::empty();
		assert_eq!(list.head(), None);
		let list = make_list!(Val::from(1), Val::from(2), Val::from(3));
		assert_eq!(list.head(), Some(&Val::from(1)));
		let list = list.tail().unwrap();
		assert_eq!(list.head(), Some(&Val::from(2)));
		let list = list.tail().unwrap();
		assert_eq!(list.head(), Some(&Val::from(3)));
		let list = list.tail().unwrap();
		assert_eq!(list.head(), None);
		assert_eq!(None, list.tail());
	}
	#[test]
	fn len() {
		let zero = List::empty();
		let one = zero.push(Val::empty());
		let two = one.push(Val::empty());
		assert_eq!(0, zero.len());
		assert_eq!(1, one.len());
		assert_eq!(2, two.len());
		assert_eq!(3, one.concat(two.clone()).len());
		assert_eq!(1, two.tail().unwrap().len());
	}
	#[test]
	fn iter() {
		let list = make_list!(Val::from(1), Val::from(2), Val::from(3));
		let mut iter = list.iter();
		assert_eq!(iter.next(), Some(&Val::from(1)));
		assert_eq!(iter.next(), Some(&Val::from(2)));
		assert_eq!(iter.next(), Some(&Val::from(3)));
	}
	#[test]
	fn map() {
		assert_eq!(List::empty(), List::empty().map(|_| -> Result<Val, ()> { Ok(Val::empty()) }).unwrap());
		assert_eq!(make_list!(Val::empty()), make_list!(Val::from(1)).map(|_| -> Result<Val, ()> { Ok(Val::empty()) }).unwrap());
	}
	#[test]
	fn nth() -> Result<(), crate::errors::RtErr> {
		use crate::env::Env;
		use crate::values::Quant;
		let env = Env::new(None);
		assert!(List::empty().nth(&env, Quant::scalar(0.into())).is_err());
		assert_eq!(Val::from(2), make_list!(Val::from(1), Val::from(2)).nth(&env, Quant::scalar(1.into()))?);
		Ok(())
	}
	#[test]
	fn reverse() {
		assert_eq!(List::empty(), List::empty().reverse());
		assert_eq!(make_list!(Val::from(1), Val::from(2)), make_list!(Val::from(2), Val::from(1)).reverse());
	}
}
