use crate::env::SharedEnv;
use crate::errors::RtErr;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::{Num, Prim};
use crate::values::{Index, Quant, Val};

use num::integer::div_ceil;
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
	pub fn slice(&self, idx: Index) -> Result<Val, RtErr> {
		match idx {
			Index::Element(idx) => {
				if self.0.len() == 0 {
					Err(RtErr::OutOfBounds)
				} else {
					self.0
						.chars()
						.nth(idx.rem_euclid(self.len() as i64) as usize)
						.ok_or(RtErr::OutOfBounds)
						.map(|c| Val::Prim(Prim::Text(Text(c.into()))))
				}
			}
			Index::Slice {
				mut start,
				mut end,
				mut stride,
			} => {
				let len = self.len() as i64;
				// Compute the text, start, end, and stride, making any adjustments required to ensure forward iteration.
				let text;
				if stride.is_positive() {
					text = self.0.clone();
				} else {
					// Iterate over the reversed text.
					text = self.0.chars().rev().collect::<String>();
					// Reverse start/end/stride with respect to the text length.
					start = len - start;
					end = len - end;
					stride = -stride;
				};
				// Ensure non-negative start.
				while start < 0 {
					start += stride;
				}
				// Ensure end doesn't go past the length.
				end = end.min(len);
				// Compute the number of characters in the slice.
				let count = div_ceil((end - start).max(0), stride);
				// Build the result text.
				let iter = text
					.chars()
					// Skip ahead to the start index.
					.skip(start as usize)
					// Step by the stride.
					.step_by(stride as usize)
					// Take count characters.
					.take(count as usize);
				let mut result = String::new();
				for c in iter {
					result.push(c);
				}
				Ok(Val::Prim(Prim::Text(result.into())))
			}
		}
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
