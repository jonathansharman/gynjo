use super::{Units, UnitErr};

use crate::env::SharedEnv;
use crate::format_with_env::FormatWithEnv;
use crate::primitives::{Num, NumErr};

use std::cmp::{Ord, PartialOrd};
use std::fmt;
use std::ops::{Add, Sub, Mul, Div, Neg};

#[derive(Eq, PartialEq, Debug)]
pub enum QuantErr {
	Num(NumErr),
	Unit(UnitErr),
	DimensionedExponent,
}

impl QuantErr {
	pub fn num(err: NumErr) -> QuantErr {
		QuantErr::Num(err)
	}

	pub fn unit(err: UnitErr) -> QuantErr {
		QuantErr::Unit(err)
	}
}

impl fmt::Display for QuantErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			QuantErr::Num(err) => err.fmt(f),
			QuantErr::Unit(err) => err.fmt(f),
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

	/// Converts `self` into a scalar value, if it's dimensionless.
	pub fn into_scalar(self) -> Result<Num, QuantErr> {
		Ok(self.convert_into(Units::empty())?.val)
	}

	/// Converts `self` into a quantity that uses only base units.
	pub fn convert_into_base(self) -> Result<Self, QuantErr> {
		let (units, factor) = self.units.to_base_units_and_factor().map_err(QuantErr::num)?;
		Ok(Self { val: self.val * factor, units })
	}

	/// Converts `self` into a quantity that uses `to_units`, if the dimensions match.
	pub fn convert_into(self, to_units: Units) -> Result<Self, QuantErr> {
		Ok(Self {
			val: self.val * self.units.conversion_factor(to_units.clone()).map_err(QuantErr::unit)?,
			units: to_units,
		})
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
			.convert_into(self.units.clone())
			.map_or(false, |other| self.val.eq(&other.val))
	}
}

impl Eq for Quant {}

impl PartialOrd for Quant {
	fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
		other.clone()
			.convert_into(self.units.clone())
			.map(|same_units| self.val.cmp(&same_units.val)).ok()
	}
}

impl Add for Quant {
	type Output = Result<Self, QuantErr>;
	fn add(self, rhs: Self) -> Self::Output {
		Ok(Self {
			val: self.val + rhs.convert_into(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Sub for Quant {
	type Output = Result<Self, QuantErr>;
	fn sub(self, rhs: Self) -> Self::Output {
		Ok(Self {
			val: self.val - rhs.convert_into(self.units.clone())?.val,
			units: self.units,
		})
	}
}

impl Mul for Quant {
	type Output = Result<Self, QuantErr>;
	fn mul(self, rhs: Self) -> Self::Output {
		let (units, factor) = self.units.merge_units(rhs.units).map_err(QuantErr::num)?;
		Ok(Self {
			val: (self.val * rhs.val / factor).map_err(QuantErr::num)?,
			units,
		})
	}
}

impl Div for Quant {
	type Output = Result<Self, QuantErr>;
	fn div(self, rhs: Self) -> Self::Output {
		let (units, factor) = self.units.merge_units(rhs.units.inverse()).map_err(QuantErr::num)?;
		Ok(Self {
			val: (self.val / (rhs.val * factor)).map_err(QuantErr::num)?,
			units,
		})
	}
}

impl Neg for Quant {
	type Output = Self;
	fn neg(self) -> Self::Output {
		Self { val: -self.val, units: self.units }
	}
}
