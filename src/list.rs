// Functional list implementation based on https://rust-unofficial.github.io/too-many-lists/third-final.html.

use super::env::SharedEnv;
use super::values::Val;

use itertools::Itertools;

use std::sync::Arc;

/// A functional linked list of Gynjo values.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct List(Link);

type Link = Option<Arc<Node>>;

#[derive(Clone, Eq, PartialEq, Debug)]
struct Node {
    elem: Val,
    next: Link,
}

impl Node {
	pub fn map<F, E>(&self, f: F) -> Result<Node, E> where F: Fn(&Val) -> Result<Val, E> {
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
    pub fn empty() -> Self {
        List(None)
    }

    pub fn push(&self, elem: Val) -> List {
        List(Some(Arc::new(Node {
            elem: elem,
            next: self.0.clone(),
        })))
    }

    pub fn tail(&self) -> Option<List> {
		self.0.as_ref().map(|node| List(node.next.clone()))
    }

    pub fn head(&self) -> Option<&Val> {
        self.0.as_ref().map(|node| &node.elem)
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter { next: self.0.as_ref().map(|node| &**node) }
	}

	pub fn map<F, E>(&self, f: F) -> Result<List, E> where F: Fn(&Val) -> Result<Val, E> {
		Ok(List(match self.0.clone() {
			Some(node) => Some(Arc::new(node.map(f)?)),
			None => None,
		}))
	}

	pub fn to_string(&self, env: &SharedEnv) -> String {
		format!("[{}]", self.iter().map(|elem| elem.to_string(&env)).join(", "))
	}
}

impl Drop for List {
    fn drop(&mut self) {
        let mut head = self.0.take();
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
