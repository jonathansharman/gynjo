use super::quantity::{Quant, QuantErr};
use crate::env::SharedEnv;
use crate::errors::RtErr;
use crate::format_with_env::FormatWithEnv;

/// A half-open range of quantities with optional stride and bounds.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Range {
	pub start: Option<Quant>,
	pub end: Option<Quant>,
	pub stride: Option<Quant>,
}

impl Range {
	/// Computes `start`, `end`, and/or `stride` from available values and `length`.
	///
	/// Tries to infer a missing `stride` to move from `start` towards `end` by 1.
	/// Tries to infer a missing `start` and/or `end` as an extremum.
	pub fn into_start_end_stride(
		self,
		env: &SharedEnv,
		length: i64,
	) -> Result<(i64, i64, i64), RtErr> {
		let get_option_i64 = |quant: Option<Quant>| -> Result<Option<i64>, RtErr> {
			quant
				.map(|quant| {
					quant.as_i64().ok_or(RtErr::InvalidIndex {
						idx: quant.format_with_env(&env),
					})
				})
				.transpose()
		};
		let start = get_option_i64(self.start)?;
		let end = get_option_i64(self.end)?;
		let stride = get_option_i64(self.stride)?;
		// Infer missing fields and perform some validation.
		match (start, end, stride) {
			(_, _, Some(stride)) if stride == 0 => Err(RtErr::ZeroStrideSlice),
			(None, None, None) => Ok((0.into(), length.into(), 1.into())),
			(None, None, Some(stride)) => {
				if stride < 0 {
					Ok((length.into(), 0.into(), stride))
				} else {
					Ok((0.into(), length.into(), stride))
				}
			}
			(None, Some(end), None) => Ok((0.into(), end, 1.into())),
			(None, Some(end), Some(stride)) => {
				if stride < 0 {
					Ok((length.into(), end, stride))
				} else {
					Ok((0.into(), end, stride))
				}
			}
			(Some(start), None, None) => Ok((start, length.into(), 1.into())),
			(Some(start), None, Some(stride)) => {
				if stride < 0 {
					Ok((start, 0.into(), stride))
				} else {
					Ok((start, length.into(), stride))
				}
			}
			(Some(start), Some(end), None) => {
				if start <= end {
					Ok((start, end, 1.into()))
				} else {
					Ok((start, end, (-1).into()))
				}
			}
			(Some(start), Some(end), Some(stride)) => Ok((start, end, stride)),
		}
	}

	pub fn reverse(self) -> Self {
		Self {
			start: self.end,
			end: self.start,
			stride: self.stride.map(|stride| -stride),
		}
	}
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
enum Direction {
	Ascending,
	Descending,
	Stuck,
}

pub struct RangeIter {
	range: Range,
	direction: Direction,
}

impl Iterator for RangeIter {
	type Item = Result<Quant, QuantErr>;

	fn next(&mut self) -> Option<Self::Item> {
		self.range.start.clone().and_then(|start| {
			let direction = self.direction;
			let mut ascending_descending = |ascending| match &self.range.end {
				Some(end) => {
					if (ascending && &start >= end) || (!ascending && &start <= end) {
						return None;
					}
					self.range
						.stride
						.clone()
						.map(|stride| match start.clone() + stride {
							Ok(sum) => {
								self.range.start = Some(sum);
								Ok(start.clone())
							}
							Err(err) => Err(err),
						})
				}
				None => self
					.range
					.stride
					.clone()
					.map(|stride| match start.clone() + stride {
						Ok(sum) => {
							self.range.start = Some(sum);
							Ok(start.clone())
						}
						Err(err) => Err(err),
					}),
			};
			match direction {
				Direction::Ascending => ascending_descending(true),
				Direction::Descending => ascending_descending(false),
				Direction::Stuck => Some(Ok(start.clone())),
			}
		})
	}
}

impl IntoIterator for Range {
	type Item = Result<Quant, QuantErr>;
	type IntoIter = RangeIter;

	fn into_iter(self) -> Self::IntoIter {
		let direction = match &self.stride {
			None => Direction::Ascending,
			Some(stride) if stride.value().is_positive() => Direction::Ascending,
			Some(stride) if stride.value().is_negative() => Direction::Descending,
			_ => Direction::Stuck,
		};
		RangeIter {
			range: self,
			direction,
		}
	}
}

impl FormatWithEnv for Range {
	fn format_with_env(&self, env: &crate::env::SharedEnv) -> String {
		let start = self
			.start
			.as_ref()
			.map_or(String::new(), |start| start.format_with_env(&env));
		let end = self
			.end
			.as_ref()
			.map_or(String::new(), |end| end.format_with_env(&env));
		let stride = self.stride.as_ref().map_or(String::new(), |stride| {
			format!(" by {}", stride.format_with_env(&env))
		});
		format!("({}..{}{})", start, end, stride)
	}
}
