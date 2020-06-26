use super::numbers::{Num, NumErr};
use super::quantity::Quant;

use itertools::Itertools;

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

/// A unit of some dimension.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Unit {
	/// The unit's name.
	name: String,
	/// This unit's value in base units or `None` if this is already a base unit.
	base: Option<Box<Quant>>
}

impl Unit {
	pub fn base<S>(name: S) -> Self where S: Into<String> {
		Unit { name: name.into(), base: None }
	}

	pub fn non_base<S>(name: S, base: Quant) -> Self where S: Into<String> {
		Unit { name: name.into(), base: Some(Box::new(base)) }
	}
}

impl fmt::Display for Unit {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.name.fmt(f)
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

	/// Merges `unit` into `self`, combining powers and canceling if needed.
	pub fn merge_unit(mut self, unit: Unit, power: Num) -> Self {
		if let Some(existing_power) = self.map.remove(&unit) {
			// Unit already present; combine and insert if powers didn't cancel.
			let combined_power = power + existing_power;
			if !combined_power.is_zero() {
				self.map.insert(unit, combined_power);
			}
		} else {
			// New unit; just insert.
			self.map.insert(unit, power);
		}
		self
	}

	/// Merges `other` into `self`, combining powers and canceling where needed.
	pub fn merge_units(mut self, other: Self) -> Self {
		for (unit, power) in other.into_iter() {
			self = self.merge_unit(unit, power);
		}
		self
	}

	/// Converts `self` to base units and a conversion factor.
	pub fn to_base_units_and_factor(self) -> Result<(Self, Num), NumErr> {
		let mut factor = Num::from(1);
		let mut base_units = Self::empty();
		// Break each unit into its base units and merge into resulting unit map.
		for (from_unit, from_power) in self.map.into_iter() {
			if let Some(from_base) = from_unit.base {
				let (from_base_value, from_base_units) = from_base.into_value_and_units();
				// Merge all base units.
				base_units = base_units.merge_units(from_base_units.pow(&from_power)?);
				// Adjust conversion factor.
				factor = factor * from_base_value.pow(from_power)?;
			} else {
				// Already a base unit; absorb without modifying scale.
				base_units = base_units.merge_unit(from_unit, from_power);
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
				a.1.cmp(&b.1).reverse().then(a.0.name.cmp(&b.0.name))
			})
			.map(|(unit, power)| {
				if power == &Num::from(1) {
					unit.name.clone()
				} else {
					format!("{}^{}", unit.name, power)
				}
			})
			.join("").fmt(f)
	}
}
