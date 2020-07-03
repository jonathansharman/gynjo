use crate::env::SharedEnv;

pub trait FormatWithEnv {
	/// Converts `self` to a user-readable string using `env` for formatting settings.
	fn format_with_env(&self, env: &SharedEnv) -> String;
}
