use super::numbers::Num;

use std::fmt;

/// A unit of a dimension.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Unit {
	pub name: String,
	pub scale: Num,
}

impl fmt::Display for Unit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.name.fmt(f)
	}
}
