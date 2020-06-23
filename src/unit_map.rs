use super::numbers::{Num, NumErr};
use super::symbol::Sym;
use super::unit::Unit;

use itertools::Itertools;

use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

/// A mapping from dimensions to their units and powers.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct UnitMap {
	pub map: HashMap<Sym, (Unit, Num)>
}

impl UnitMap {
	/// Creates an empty (dimensionless) unit set.
	pub fn empty() -> UnitMap {
		UnitMap { map: HashMap::new() }
	}

	/// Computes `self` to the power of `exponent`.
	pub fn pow(self, rhs: &Num) -> Result<UnitMap, NumErr> {
		let mut map = HashMap::new();
		for (dimension, (unit, power)) in self.map.into_iter() {
			map.insert(dimension, (unit, power * rhs.clone()));
		}
		Ok(UnitMap { map })
	}

	/// Whether this unit map is empty, i.e. dimensionless.
	pub fn is_empty(&self) -> bool {
		self.map.is_empty()
	}
}

impl Hash for UnitMap {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		// Hash based on all keys and values.
		for (k, v) in self.map.iter() {
			k.hash(state);
			v.hash(state);
		}
    }
}

impl From<(Sym, Unit)> for UnitMap {
    fn from((dimension, unit): (Sym, Unit)) -> Self {
        UnitMap { map: [(dimension, (unit, Num::from(1)))].iter().cloned().collect() }
    }
}

impl fmt::Display for UnitMap {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		self.map.values()
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
