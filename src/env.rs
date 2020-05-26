use super::interpreter::exec;
use super::symbol::Symbol;
use super::values::Value;

use std::collections::HashMap;
use std::rc::Rc;

/// A chained set of variable mappings.
pub struct Env {
	/// Variables mappings created within the local scope.
	local_vars: HashMap<Symbol, Value>,
	/// Reference to the parent environment, if any.
	parent_env: Option<Rc<Env>>,
}

impl Env {
	/// `parent_env` - The parent environment, if any.
	pub fn new(parent_env: Option<Rc<Env>>) -> Env {
		Env {
			local_vars: HashMap::new(),
			parent_env: parent_env,
		}
	}

	/// Creates a new ref-counted environment with core libs loaded.
	pub fn make_with_core_libs() -> Rc<Env> {
		lazy_static! {
			static ref CORE_LIBS_ENV: Rc<Env> = {
				let mut env = Env::make_empty();
				import_lib(&mut env, r#""core/constants.gynj""#);
				import_lib(&mut env, r#""core/core.gynj""#);
				env
			};
		}
		Rc::new(Env::new(Some(CORE_LIBS_ENV.clone())))
	}

	/// Creates a new empty ref-counted environment.
	pub fn make_empty() -> Rc<Env> {
		Rc::new(Env::new(None))
	}

	pub fn assign(&mut self, variable: Symbol, value: Value) {
		self.local_vars.insert(variable, value);
	}

	/// Returns the value of the variable named `name` or `None` if the variable is undefined.
	pub fn lookup(&self, variable: &Symbol) -> Option<&Value> {
		self.local_vars
			.get(variable)
			.or(self.parent_env.and_then(|p| p.lookup(variable)))
	}
}

/// Attempts to import `lib` into `env`. Displays an error message on failure.
pub fn import_lib(env: &mut Rc<Env>, lib: &str) {
	if let Err(err) = exec(env, &format!("import {}", lib)) {
		println!("Error while importing {}: {}", lib, err);
	}
}
