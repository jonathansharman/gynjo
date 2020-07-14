use crate::env::SharedEnv;
use crate::errors::RtErr;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::{Num, Prim};
use crate::values::{List, Quant, Range, Val};

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
			Err(RtErr::InvalidIndex { idx: idx.format_with_env(&env) })
		}
	}

	/// Copies a slice from this text, based on `index`.
	///
	/// `env`: Used for formatting error messages if an error occurs.
	/// `index`: Either an single integral index or a `Range` of indices.
	///
	/// Empty lower/upper bounds use the beginning/end of the string.
	///
	/// Individual indices are copied modulo the length of the string.
	pub fn slice(&self, env: &SharedEnv, idx: List) -> Result<Val, RtErr> {
		if idx.len() != 1 {
			return Err(RtErr::InvalidIndex { idx: idx.format_with_env(&env) });
		}
		match idx.head().unwrap().clone() {
			Val::Quant(idx) => Ok(Val::Prim(Prim::Text(Text(self.nth(&env, idx)?.to_string())))),
			Val::Range(range) => {
				let mut result_string = "".to_string();
				let (start, end, mut stride) = range.into_start_end_stride();
				let start = start.or(Some(Quant::scalar(Num::from(0))));
				let end = end.or(Some(Quant::scalar(Num::from((self.0.len() - 1) as i64))));
				// Check if the stride needs to be reversed.
				if let (Some(start), Some(end)) = (&start, &end) {
					if (start < end && stride.value().is_negative()) || (start > end && stride.value().is_positive()) {
						stride = -stride;
					}
				}
				let range = Range::new(start, end, Some(stride));
				for idx in range.into_iter() {
					result_string.push(self.nth(&env, idx.map_err(RtErr::quant)?)?);
				}
				Ok(Val::Prim(Prim::Text(Text(result_string))))
			},
			invalid @ _ => Err(RtErr::InvalidIndex { idx: invalid.format_with_env(&env) }),
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
