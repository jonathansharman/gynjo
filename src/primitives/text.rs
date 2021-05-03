use crate::env::SharedEnv;
use crate::errors::RtErr;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::{Num, Prim};
use crate::values::{Index, List, Quant, Val};

use num_traits::ToPrimitive;

use std::fmt;

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Text(String);

impl Text {
	pub fn empty() -> Self {
		Self("".into())
	}

	/// The underlying string of this text.
	pub fn string(&self) -> &String {
		&self.0
	}

	/// The number of characters in this text.
	pub fn len(&self) -> usize {
		return self.0.len();
	}

	/// Copies one or more characters from this text, based on `idx`.
	///
	/// `env`: Used for formatting error messages if an error occurs.
	pub fn slice(&self, _env: &SharedEnv, _idx: Index) -> Result<Val, RtErr> {
		Ok(Val::empty())
		// match idx {
		// 	Index::Element(idx) => self
		// 		.0
		// 		.chars()
		// 		.nth(idx)
		// 		.ok_or(RtErr::OutOfBounds)
		// 		.map(|c| Val::Prim(Prim::Text(Text(c.into())))),
		// 	Index::Slice { start, end, stride } => {
		// 		let iter = self
		// 			.0
		// 			.chars()
		// 			.skip(start)
		// 			.step_by(stride)
		// 			.take((end - start) / stride);
		// 		let mut result_string = "".to_string();
		// 		for c in iter {
		// 			result_string.push(c);
		// 		}
		// 		Ok(Val::Prim(Prim::Text(Text(result_string))))
		// 	}
		// }
	}

	/// Gets the character of this text at index `idx` modulo the text length.
	///
	/// `env`: Used for formatting error messages if the index is not a scalar integer.
	/// `idx`: The index to retrieve. Must be an integer.
	pub fn nth(&self, env: &SharedEnv, idx: Quant) -> Result<char, RtErr> {
		if self.0.is_empty() {
			return Err(RtErr::OutOfBounds);
		}
		let idx = idx.into_scalar().map_err(RtErr::quant)?;
		if let Num::Integer(idx) = idx {
			let idx = idx.to_i64().ok_or(RtErr::OutOfBounds)?;
			let signed_len = self.0.len() as i64;
			let mut idx = ((idx % signed_len) + signed_len) % signed_len;
			let mut iter = self.0.chars();
			let mut c = iter.next();
			while c.is_some() && idx != 0 {
				c = iter.next();
				idx -= 1;
			}
			c.ok_or(RtErr::OutOfBounds)
		} else {
			Err(RtErr::InvalidIndex {
				idx: idx.format_with_env(&env),
			})
		}
	}
}

impl fmt::Display for Text {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}

impl From<String> for Text {
	fn from(string: String) -> Self {
		Self(string)
	}
}

impl From<Text> for String {
	fn from(text: Text) -> Self {
		text.0
	}
}
