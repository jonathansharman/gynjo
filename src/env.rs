use super::interpreter::exec;
use super::symbol::Sym;
use super::values::Val;

use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// A chained set of variable mappings.
#[derive(Debug)]
pub struct Env {
	/// Variables mappings created within the local scope.
	local_vars: HashMap<Sym, Val>,
	/// Reference to the parent environment, if any.
	parent_env: Option<Arc<Mutex<Env>>>,
}

impl Env {
	/// `parent_env` - The parent environment, if any.
	pub fn new(parent_env: Option<Arc<Mutex<Env>>>) -> Arc<Mutex<Env>> {
		Arc::new(Mutex::new(Env {
			local_vars: HashMap::new(),
			parent_env: parent_env,
		}))
	}

	/// Creates a new ref-counted environment with core libs loaded.
	pub fn with_core_libs() -> Arc<Mutex<Env>> {
		lazy_static! {
			static ref CORE_LIBS: Arc<Mutex<Env>> = {
				let mut parent = Env::new(None);
				import_lib(&mut parent, r#""core/constants.gynj""#);
				import_lib(&mut parent, r#""core/core.gynj""#);
				parent
			};
		}
		Env::new(Some(CORE_LIBS.clone()))
	}

	/// Sets `variable` to `value` in this environment.
	pub fn assign(&mut self, variable: Sym, value: Val) {
		self.local_vars.insert(variable, value);
	}

	/// Returns the value of the variable named `name` or `None` if the variable is undefined.
	pub fn lookup(&self, variable: &Sym) -> Option<Val> {
		self.local_vars
			.get(variable)
			.map(|v| v.clone())
			.or(self.parent_env.as_ref().and_then(|p| p.lock().unwrap().lookup(variable).clone()))
	}
}

/// Attempts to import `lib` into `env`. Displays an error message on failure.
pub fn import_lib(env: &mut Arc<Mutex<Env>>, lib: &str) {
	if let Err(err) = exec(env, &format!("import {}", lib)) {
		println!("Error while importing {}: {}", lib, err);
	}
}
