use super::quantity::{Quant, QuantErr};

use crate::format_with_env::FormatWithEnv;

#[derive(Clone, Eq, PartialEq, Debug)]
enum Direction { Ascending, Descending, Stuck }

/// An inclusive range of numbers with stride and optional bounds.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Range {
	start: Option<Quant>,
	end: Option<Quant>,
	stride: Quant,
	direction: Direction,
}

impl Range {
	pub fn new(start: Option<Quant>, end: Option<Quant>, stride: Quant) -> Range {
		let direction = match &stride {
			value if value.value().is_positive() => Direction::Ascending,
			value if value.value().is_negative() => Direction::Descending,
			_ => Direction::Stuck,
		};
		Range { start, end, stride, direction }
	}

	pub fn start(&self) -> &Option<Quant> {
		&self.start
	}

	pub fn end(&self) -> &Option<Quant> {
		&self.end
	}

	pub fn stride(&self) -> &Quant {
		&self.stride
	}
}

pub struct RangeIter(Range);

impl Iterator for RangeIter {
	type Item = Result<Quant, QuantErr>;
	fn next(&mut self) -> Option<Self::Item> {
		match self.0.start.clone() {
			Some(start) => {
				match self.0.direction {
					Direction::Ascending => match &self.0.end {
						Some(end) => {
							if &start <= end {
								match start.clone() + self.0.stride.clone() {
									Ok(sum) => {
										self.0.start = Some(sum);
										Some(Ok(start.clone()))
									},
									Err(err) => Some(Err(err)),
								}
							} else {
								None
							}
						},
						None => {
							match start.clone() + self.0.stride.clone() {
								Ok(sum) => {
									self.0.start = Some(sum);
									Some(Ok(start.clone()))
								},
								Err(err) => Some(Err(err)),
							}
						},
					},
					Direction::Descending => match &self.0.end {
						Some(end) => {
							if &start >= end {
								match start.clone() + self.0.stride.clone() {
									Ok(sum) => {
										self.0.start = Some(sum);
										Some(Ok(start.clone()))
									},
									Err(err) => Some(Err(err)),
								}
							} else {
								None
							}
						},
						None => {
							match start.clone() + self.0.stride.clone() {
								Ok(sum) => {
									self.0.start = Some(sum);
									Some(Ok(start.clone()))
								},
								Err(err) => Some(Err(err)),
							}
						},
					},
					Direction::Stuck => Some(Ok(start.clone())),
				}
			},
			None => None,
		}
	}
}

impl IntoIterator for Range {
	type Item = Result<Quant, QuantErr>;
	type IntoIter = RangeIter;
	fn into_iter(self) -> Self::IntoIter {
		RangeIter(self)
	}
}

impl FormatWithEnv for Range {
	fn format_with_env(&self, env: &crate::env::SharedEnv) -> String {
		let start = self.start.as_ref().map_or(String::new(), |start| start.format_with_env(&env));
		let end = self.end.as_ref().map_or(String::new(), |start| start.format_with_env(&env));
		format!("({}..{} by {})", start, end, self.stride().format_with_env(&env))
	}
}
