use super::Val;

use crate::env::SharedEnv;
use crate::format_with_env::FormatWithEnv;

use itertools::Itertools;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Tuple {
	pub elems: Vec<Val>,
}

/// Convenience macro for turning a comma-separated list of values into a value tuple.
#[macro_export]
macro_rules! make_tuple_value {
	($($exprs:expr),*) => {
		Val::Tuple(Tuple { elems: vec!($($exprs),*) })
	}
}

impl Tuple {
	/// Creates a tuple with no elements.
	pub fn empty() -> Tuple {
		Tuple { elems: Vec::new() }
	}
}

impl FormatWithEnv for Tuple {
	fn format_with_env(&self, env: &SharedEnv) -> String {
		format!(
			"({})",
			self.elems
				.iter()
				.map(|elem| elem.format_with_env(env))
				.join(", ")
		)
	}
}
