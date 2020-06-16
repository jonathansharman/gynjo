use super::env::SharedEnv;
use super::values::Val;

use itertools::Itertools;

use std::boxed::Box;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Tuple {
	pub elems: Box<Vec<Val>>,
}

/// Convenience macro for turning a comma-separated list of values into a value tuple.
#[macro_export]
macro_rules! make_tuple_value {
	($($exprs:expr),*) => {
		Val::Tuple(Tuple { elems: Box::new(vec!($($exprs),*)) })
	}
}

impl Tuple {
	pub fn empty() -> Tuple {
		Tuple { elems: Box::new(Vec::new()) }
	}

	pub fn to_string(&self, env: &SharedEnv) -> String {
		format!("({})", self.elems.iter().map(|elem| elem.to_string(env)).join(", "))
	}
}
