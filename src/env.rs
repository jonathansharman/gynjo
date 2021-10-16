use crate::interpreter::eval;
use crate::symbol::Sym;
use crate::values::Quant;
use crate::values::Val;

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
	/// Unit definitions created within the local scope.
	local_units: HashMap<String, Quant>,
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
			parent_env,
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

	/// Sets the value of `unit` to `value` in this environment.
	pub fn set_unit(&mut self, unit_name: String, value: Quant) {
		self.local_units.insert(unit_name, value);
	}

	/// Returns the value of the variable named `name` or `None` if the variable is undefined.
	pub fn get_var(&self, name: &Sym) -> Option<Val> {
		self.local_vars.get(name).cloned().or_else(|| {
			self.parent_env
				.as_ref()
				.and_then(|p| p.lock().unwrap().get_var(name))
		})
	}

	/// Returns the value corresponding to `unit` `None` if the unit is undefined.
	pub fn get_unit(&self, unit_name: &str) -> Option<Quant> {
		self.local_units.get(unit_name).cloned().or_else(|| {
			self.parent_env
				.as_ref()
				.and_then(|p| p.lock().unwrap().get_unit(unit_name))
		})
	}
}
