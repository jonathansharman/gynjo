use super::numbers::{Num, NumErr};
use super::quantity::Quant;

use itertools::Itertools;

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

#[derive(Eq, PartialEq, Debug)]
pub enum UnitErr {
	Num(NumErr),
	Incompatible,
}

impl UnitErr {
	pub fn num(err: NumErr) -> UnitErr {
		UnitErr::Num(err)
	}
}

impl fmt::Display for UnitErr {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			UnitErr::Num(err) => err.fmt(f),
			UnitErr::Incompatible => write!(f, "Units are incompatible"),
		}
	}
}

/// A unit of some dimension.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Unit {
	Base { name: String },
	NonBase { name: String, base_quantity: Box<Quant> },
}

impl Unit {
	pub fn name(&self) -> &String {
		match self {
			Unit::Base { name } => name,
			Unit::NonBase { name, .. } => name,
		}
	}

	pub fn base<S>(name: S) -> Self where S: Into<String> {
		Unit::Base { name: name.into() }
	}

	pub fn non_base<S>(name: S, base: Quant) -> Self where S: Into<String> {
		Unit::NonBase { name: name.into(), base_quantity: Box::new(base) }
	}
}

impl fmt::Display for Unit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.name().fmt(f)
	}
}

/// A mapping from dimensions to their units and powers.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Units {
	map: HashMap<Unit, Num>
}

impl Units {
	/// Creates an empty (dimensionless) unit set.
	pub fn empty() -> Self {
		Self { map: HashMap::new() }
	}

	/// Whether this unit map is empty, i.e. dimensionless.
	pub fn is_empty(&self) -> bool {
		self.map.is_empty()
	}

	/// The conversion factor for converting `self` to `other`, if the dimensions match.
	pub fn conversion_factor(self, other: Units) -> Result<Num, UnitErr> {
		let (from_base_units, from_factor) = self.to_base_units_and_factor().map_err(UnitErr::num)?;
		let (to_base_units, to_factor) = other.to_base_units_and_factor().map_err(UnitErr::num)?;
		if from_base_units != to_base_units {
			Err(UnitErr::Incompatible)
		} else {
			Ok((from_factor / to_factor).map_err(UnitErr::num)?)
		}
	}

	/// Merges `unit` into `self`, combining powers and canceling if needed.
	///
	/// Returns the merged units and a factor to account for merged units of the same dimension.
	pub fn merge_unit(mut self, unit: Unit, power: Num) -> Result<(Self, Num), NumErr> {
		let mut factor = Num::from(1);
		if let Some(existing_power) = self.map.remove(&unit) {
			// Unit already present; combine and insert if powers didn't cancel.
			let combined_power = power + existing_power;
			if !combined_power.is_zero() {
				self.map.insert(unit, combined_power);
			}
			Ok((self, factor))
		} else {
			// Unit not present. Check if a different unit of the same dimension is present.
			let mut units = Self::empty();
			let mut added = false;
			for (existing_unit, existing_power) in self.map.into_iter() {
				// Attempt to convert the unit to each unit in `self`.
				if let Ok(conversion_factor) = Units::from(unit.clone()).conversion_factor(existing_unit.clone().into()) {
					// Found a compatible unit.
					added = true;
					factor = (factor / conversion_factor.pow(power.clone())?)?;
					let combined_power = power.clone() + existing_power;
					if !combined_power.is_zero() {
						units.map.insert(existing_unit, combined_power);
					}
				} else {
					units.map.insert(existing_unit, existing_power);
				}
			}
			if !added {
				// New unit; just insert.
				units.map.insert(unit, power);
			}
			Ok((units, factor))
		}
	}

	/// Merges `other` into `self`, combining powers and canceling where needed.
	///
	/// Returns the merged units and a factor to account for merged units of the same dimension.
	pub fn merge_units(mut self, other: Self) -> Result<(Self, Num), NumErr> {
		let mut factor = Num::from(1);
		for (unit, power) in other.into_iter() {
			let (next_self, next_factor) = self.merge_unit(unit, power)?;
			self = next_self;
			factor = factor * next_factor;
		}
		Ok((self, factor))
	}

	/// Converts `self` to base units and a conversion factor.
	pub fn to_base_units_and_factor(self) -> Result<(Self, Num), NumErr> {
		let mut factor = Num::from(1);
		let mut base_units = Self::empty();
		// Break each unit into its base units and merge into resulting unit map.
		for (from_unit, from_power) in self.map.into_iter() {
			match from_unit {
				from_unit @ Unit::Base { .. } => {
					// Already a base unit; absorb without modifying scale.
					let (next_base_units, next_factor) = base_units.merge_unit(from_unit, from_power)?;
					base_units = next_base_units;
					factor = factor * next_factor;
				},
				Unit::NonBase { base_quantity: from_base_quantity, .. } => {
					let (from_base_value, from_base_units) = from_base_quantity.into_value_and_units();
					// Merge all base units and adjust conversion factor.
					let (next_base_units, next_factor) = base_units.merge_units(from_base_units.pow(&from_power)?)?;
					base_units = next_base_units;
					factor = factor * from_base_value.pow(from_power)? * next_factor;
				},
			}
		}
		Ok((base_units, factor))
	}

	/// Negates the unit powers of `self`.
	pub fn inverse(self) -> Self {
		Self { map: self.map.into_iter().map(|(unit, power)| (unit, -power)).collect() }
	}

	/// Computes `self` to the power of `exponent`.
	pub fn pow(self, rhs: &Num) -> Result<Self, NumErr> {
		let mut map = HashMap::new();
		for (unit, power) in self.map.into_iter() {
			map.insert(unit, power * rhs.clone());
		}
		Ok(Self { map })
	}
}

impl IntoIterator for Units {
	type Item = (Unit, Num);
	type IntoIter = std::collections::hash_map::IntoIter<Unit, Num>;
	fn into_iter(self) -> Self::IntoIter {
		self.map.into_iter()
	}
}

impl Hash for Units {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		// Hash based on all keys and values.
		for (k, v) in self.map.iter() {
			k.hash(state);
			v.hash(state);
		}
	}
}

impl From<Unit> for Units {
	fn from(unit: Unit) -> Self {
		Self { map: [(unit, Num::from(1))].iter().cloned().collect() }
	}
}

impl fmt::Display for Units {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.map.iter()
			.sorted_by(|a, b| {
				// Sort first by decreasing power and then alphabetically.
				a.1.cmp(&b.1).reverse().then(a.0.name().cmp(&b.0.name()))
			})
			.map(|(unit, power)| {
				if power == &Num::from(1) {
					unit.name().clone()
				} else {
					format!("{}^{}", unit.name(), power)
				}
			})
			.join("").fmt(f)
	}
}
