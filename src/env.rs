use super::interpreter::eval;
use super::numbers::Num;
use super::symbol::Sym;
use super::values::Val;
use super::unit::Unit;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

macro_rules! import_core_lib {
	($env:ident, $core_lib:literal) => {
		if let Err(err) = eval(&mut $env, include_str!($core_lib)) {
			println!("Error importing core library \"{}\": {}", $core_lib, err);
		}
	};
}

/// A chained set of variable mappings.
#[derive(Debug)]
pub struct Env {
	/// Variables mappings created within the local scope.
	local_vars: HashMap<Sym, Val>,
	/// Unit name to (dimension, unit) mappings created within the local scope.
	local_units: HashMap<String, (Sym, Unit)>,
	/// Reference to the parent environment, if any.
	parent_env: Option<SharedEnv>,
}

pub type SharedEnv = Arc<Mutex<Env>>;

impl Env {
	/// `parent_env` - The parent environment, if any.
	pub fn new(parent_env: Option<SharedEnv>) -> SharedEnv {
		Arc::new(Mutex::new(Env {
			local_vars: HashMap::new(),
			local_units: HashMap::new(),
			parent_env: parent_env,
		}))
	}

	/// Creates a new ref-counted environment with core libs loaded.
	pub fn with_core_libs() -> SharedEnv {
		lazy_static! {
			static ref CORE_LIBS: SharedEnv = {
				let mut parent = Env::new(None);
				import_core_lib!(parent, "../core/constants.gynj");
				import_core_lib!(parent, "../core/units.gynj");
				import_core_lib!(parent, "../core/core.gynj");
				parent
			};
		}
		Env::new(Some(CORE_LIBS.clone()))
	}

	/// Sets variable `name` to `value` in this environment.
	pub fn set_var(&mut self, name: Sym, value: Val) {
		self.local_vars.insert(name, value);
	}

	/// Sets unit `name` to the given `dimension` and `scale` in this environment.
	pub fn set_unit(&mut self, dimension: Sym, name: String, scale: Num) {
		self.local_units.insert(name.clone(), (dimension, Unit { name, scale }));
	}

	/// Returns the value of the variable named `name` or `None` if the variable is undefined.
	pub fn get_var(&self, name: &Sym) -> Option<Val> {
		self.local_vars
			.get(name)
			.map(|v| v.clone())
			.or(self.parent_env.as_ref().and_then(|p| p.lock().unwrap().get_var(name).clone()))
	}

	/// Returns the dimension and unit corresponding to `name` or `None` if the unit is undefined.
	pub fn get_unit(&self, name: &String) -> Option<(Sym, Unit)> {
		self.local_units
			.get(name)
			.map(|(dimension, unit)| (dimension.clone(), unit.clone()))
			.or(self.parent_env.as_ref().and_then(|p| p.lock().unwrap().get_unit(name).clone()))
	}
}

/// Attempts to import library at `filename` into `env`. Displays an error message on failure.
pub fn import_lib_from_path(env: &mut SharedEnv, filename: &str) {
	if let Err(err) = eval(env, &format!("import {}", filename)) {
		println!("Error importing {}: {}", filename, err);
	}
}
