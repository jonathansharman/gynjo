use super::env::SharedEnv;
use super::format_with_env::FormatWithEnv;
use super::numbers::{Num, NumErr};
use super::units::Units;

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Eq, PartialEq, Debug)]
pub enum QuantErr {
	Num(NumErr),
	IncompatibleUnits,
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
			QuantErr::IncompatibleUnits => write!(f, "Units are incompatible"),
			QuantErr::DimensionedExponent => write!(f, "Exponents must be dimensionless"),
		}
	}
}

/// Numeric Gynjo types.
#[derive(Clone, Hash, Debug)]
pub struct Quant {
	val: Num,
	units: Units,
}

impl Quant {
	/// Constructs a quantity with the given `value` and `units`.
	pub fn new(value: Num, units: Units) -> Self {
		Quant { val: value, units }
	}

	/// Constructs a dimensionless quantity with `val`.
	pub fn scalar(val: Num) -> Self {
		Quant { val, units: Units::empty() }
	}

	/// The numeric value of this quantity.
	pub fn value(&self) -> &Num {
		&self.val
	}

	/// The units of this quantity.
	pub fn units(&self) -> &Units {
		&self.units
	}

	/// Converts `self` into its numeric value and units.
	pub fn into_value_and_units(self) -> (Num, Units) {
		(self.val, self.units)
	}

	/// Whether `self` is dimensionless, i.e. has no units.
	pub fn is_dimensionless(&self) -> bool {
		self.units.is_empty()
	}

	/// Converts `self` to a scalar value, if it's dimensionless.
	pub fn to_scalar(self) -> Result<Num, QuantErr> {
		if self.is_dimensionless() {
			Ok(self.val)
		} else {
			Err(QuantErr::IncompatibleUnits)
		}
	}

	/// Converts `self` to a quantity that uses only base units.
	pub fn with_base_units(self) -> Result<Self, QuantErr> {
		let (units, factor) = self.units.to_base_units_and_factor().map_err(QuantErr::num)?;
		Ok(Self { val: self.val * factor, units })
	}

	/// Converts this quantity to use `units`, if the dimensions match.
	pub fn convert(self, to_units: Units) -> Result<Self, QuantErr> {
		let (from_base_units, from_factor) = self.units.to_base_units_and_factor().map_err(QuantErr::num)?;
		let (to_base_units, to_factor) = to_units.clone().to_base_units_and_factor().map_err(QuantErr::num)?;
		if from_base_units != to_base_units {
			Err(QuantErr::IncompatibleUnits)
		} else {
			let val = (self.val * from_factor / to_factor).map_err(QuantErr::num)?;
			Ok(Self { val, units: to_units })
		}
	}

	/// Computes `self` to the power of `exponent`.
	pub fn pow(self, rhs: Self) -> Result<Self, QuantErr> {
		if !rhs.units().is_empty() {
			Err(QuantErr::DimensionedExponent)
		} else {
			Ok(Self {
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
		Self::scalar(val)
	}
}

impl From<Units> for Quant {
	fn from(units: Units) -> Self {
		Self { val: 1.into(), units }
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
	type Output = Result<Self, QuantErr>;
	fn add(self, rhs: Self) -> Self::Output {
		Ok(Self {
			val: self.val + rhs.convert(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Sub for Quant {
	type Output = Result<Self, QuantErr>;
	fn sub(self, rhs: Self) -> Self::Output {
		Ok(Self {
			val: self.val - rhs.convert(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Mul for Quant {
	type Output = Self;
	fn mul(self, rhs: Self) -> Self::Output {
		Self { val: self.val * rhs.val, units: self.units.merge_units(rhs.units) }
	}
}

impl Div for Quant {
	type Output = Result<Self, QuantErr>;
	fn div(self, rhs: Self) -> Self::Output {
		Ok(Self {
			val: (self.val / rhs.val).map_err(QuantErr::num)?,
			units: self.units.merge_units(rhs.units.inverse()),
		})
	}
}

impl Neg for Quant {
	type Output = Self;
	fn neg(self) -> Self::Output {
		Self { val: -self.val, units: self.units }
	}
}
