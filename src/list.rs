// Functional list implementation based on https://rust-unofficial.github.io/too-many-lists/third-final.html.

use super::env::SharedEnv;
use super::format_with_env::FormatWithEnv;
use super::values::Val;

use itertools::Itertools;

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
		List { head: None, len: 0 }
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
	pub fn push(&self, elem: Val) -> List {
		List {
			head: Some(Arc::new(Node {
				elem: elem,
				next: self.head.clone(),
			})),
			len: self.len + 1,
		}
	}

	/// A shallow copy of this list's tail.
	pub fn tail(&self) -> Option<List> {
		self.head.as_ref().map(|node| List { head: node.next.clone(), len: self.len - 1 })
	}

	/// A reference to the first element of this list, if there is one.
	pub fn head(&self) -> Option<&Val> {
		self.head.as_ref().map(|node| &node.elem)
	}

	/// Concatenates `self` with `other`. The head of `self` is the head of the result.
	pub fn concat(&self, other: Self) -> List {
		match &self.head {
			Some(node) => List {
				head: node.next.clone(),
				len: self.len - 1,
			}.concat(other).push(node.elem.clone()),
			None => other,
		}
	}

	/// Creates a reference iterator into this list's elements.
	pub fn iter(&self) -> Iter<'_> {
		Iter { next: self.head.as_ref().map(|node| &**node) }
	}

	/// Produces a new list by applying `f` to each element.
	pub fn map<F, E>(&self, f: F) -> Result<List, E> where F: FnMut(&Val) -> Result<Val, E> {
		Ok(List {
			head: match self.head.clone() {
				Some(node) => Some(Arc::new(node.map(f)?)),
				None => None,
			},
			len: self.len,
		})
	}

	/// Gets the nth element of this list or `None` if out of bounds.
	pub fn nth(&self, mut idx: usize) -> Option<Val> {
		let mut iter = self.iter();
		let mut result = iter.next();
		while result.is_some() && idx != 0 {
			result = iter.next();
			idx -= 1;
		}
		result.map(|val| val.clone())
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
	use super::Val;
	use super::List;
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
}
