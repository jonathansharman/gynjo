use super::env::SharedEnv;
use super::format_with_env::FormatWithEnv;
use super::numbers::{Num, NumErr};
use super::unit_map::UnitMap;

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Eq, PartialEq, Debug)]
pub enum QuantErr {
	Num(NumErr),
	UnitErr,
	DimensionedExponent,
}

impl QuantErr {
	pub fn num(err: NumErr) -> QuantErr {
		QuantErr::Num(err)
	}
}

impl fmt::Display for QuantErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
			QuantErr::Num(num_err) => num_err.fmt(f),
			QuantErr::UnitErr => write!(f, "Units are incompatible"),
			QuantErr::DimensionedExponent => write!(f, "Exponents must be dimensionless"),
		}
    }
}

/// Numeric Gynjo types.
#[derive(Clone, Hash, Debug)]
pub struct Quant {
	pub val: Num,
	pub units: UnitMap,
}

impl Quant {
	/// Whether this quantity is dimensionless, i.e. has no units.
	pub fn is_dimensionless(&self) -> bool {
		self.units.is_empty()
	}

	/// Converts this quantity to a scalar value, if it's dimensionless.
	pub fn to_scalar(self) -> Result<Num, QuantErr> {
		if self.is_dimensionless() {
			Ok(self.val)
		} else {
			Err(QuantErr::UnitErr)
		}
	}

	/// Converts this quantity to use `units`, if the dimensions (including powers) match.
	pub fn convert(self, to_units: UnitMap) -> Result<Quant, QuantErr> {
		if self.units.map.len() != to_units.map.len() {
			Err(QuantErr::UnitErr)
		} else {
			let mut val = self.val;
			let mut units = self.units;
			for (dimension, (to_unit, to_power)) in to_units.map.into_iter() {
				if let Some((from_unit, from_power)) = units.map.remove(&dimension) {
					if from_power != to_power {
						// Powers for this dimension do not match.
						return Err(QuantErr::UnitErr);
					}
					if from_unit.scale != to_unit.scale {
						// Need to adjust the value for the difference in scale.
						val = (val * from_unit.scale / to_unit.scale.clone()).map_err(QuantErr::num)?;
					}
					// Insert new unit.
					units.map.insert(dimension, (to_unit, to_power));
				} else {
					// Missing a dimension.
					return Err(QuantErr::UnitErr);
				}
			}
			Ok(Quant { val, units })
		}
	}

	/// Computes `self` to the power of `exponent`.
	pub fn pow(self, rhs: Quant) -> Result<Quant, QuantErr> {
		if !rhs.units.is_empty() {
			Err(QuantErr::DimensionedExponent)
		} else {
			Ok(Quant {
				val: self.val.pow(rhs.val.clone()).map_err(QuantErr::num)?,
				units: self.units.pow(&rhs.val).map_err(QuantErr::num)?,
			})
		}
	}
}

impl FormatWithEnv for Quant {
    fn format_with_env(&self, env: &SharedEnv) -> String {
		format!("{}{}", self.val.format_with_env(env), self.units)
    }
}

impl From<Num> for Quant {
    fn from(val: Num) -> Self {
        Quant { val, units: UnitMap::empty() }
    }
}

impl From<UnitMap> for Quant {
	fn from(units: UnitMap) -> Self {
		Quant { val: 1.into(), units }
	}
}

impl PartialEq for Quant {
    fn eq(&self, other: &Self) -> bool {
		other.clone()
			.convert(self.units.clone())
			.map_or(false, |other| self.val.eq(&other.val))
    }
}

impl Eq for Quant {}

impl PartialOrd for Quant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		other.clone()
			.convert(self.units.clone())
			.map(|same_units| self.val.cmp(&same_units.val)).ok()
	}
}

impl Add for Quant {
	type Output = Result<Quant, QuantErr>;
	fn add(self, rhs: Self) -> Self::Output {
		Ok(Quant {
			val: self.val + rhs.convert(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Sub for Quant {
	type Output = Result<Quant, QuantErr>;
	fn sub(self, rhs: Self) -> Self::Output {
		Ok(Quant {
			val: self.val - rhs.convert(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Mul for Quant {
	type Output = Result<Quant, QuantErr>;
	fn mul(self, rhs: Self) -> Self::Output {
		let mut val = self.val * rhs.val;
		let mut units = self.units;
		for (dimension, (rhs_unit, rhs_power)) in rhs.units.map.into_iter() {
			if let Some((lhs_unit, lhs_power)) = units.map.remove(&dimension) {
				// Need to combine with existing unit.
				if lhs_unit.scale != rhs_unit.scale {
					// Need to adjust the value for the difference in scale.
					val = (val * rhs_unit.scale / lhs_unit.scale.clone()).map_err(QuantErr::num)?;
				}
				// Add powers and include if non-zero.
				let power = lhs_power + rhs_power;
				if !power.is_zero() {
					units.map.insert(dimension, (lhs_unit, power));
				}
			} else {
				// Dimension not present on LHS.
				units.map.insert(dimension, (rhs_unit, rhs_power));
			}
		}
		Ok(Quant { val, units })
	}
}

impl Div for Quant {
	type Output = Result<Quant, QuantErr>;
	fn div(self, rhs: Self) -> Self::Output {
		let mut val = (self.val / rhs.val).map_err(QuantErr::num)?;
		let mut units = self.units;
		for (dimension, (rhs_unit, rhs_power)) in rhs.units.map.into_iter() {
			if let Some((lhs_unit, lhs_power)) = units.map.remove(&dimension) {
				// Need to combine with existing unit.
				if lhs_unit.scale != rhs_unit.scale {
					// Need to adjust the value for the difference in scale.
					val = (val * lhs_unit.scale.clone() / rhs_unit.scale).map_err(QuantErr::num)?;
				}
				// Subtract powers and include if non-zero.
				let power = lhs_power - rhs_power;
				if !power.is_zero() {
					units.map.insert(dimension, (lhs_unit, power));
				}
			} else {
				// Dimension not present on LHS.
				units.map.insert(dimension, (rhs_unit, -rhs_power));
			}
		}
		Ok(Quant { val, units })
	}
}

impl Neg for Quant {
    type Output = Quant;
    fn neg(self) -> Self::Output {
        Quant { val: -self.val, units: self.units }
    }
}
