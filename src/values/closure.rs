use crate::env::SharedEnv;
use crate::expressions::Lambda;
use crate::format_with_env::FormatWithEnv;

#[derive(Clone, Debug)]
pub struct Closure {
	pub f: Lambda,
	pub env: SharedEnv,
}

impl PartialEq for Closure {
	fn eq(&self, other: &Self) -> bool {
		self.f == other.f
	}
}

impl Eq for Closure {}

impl FormatWithEnv for Closure {
	fn format_with_env(&self, _: &SharedEnv) -> String {
		self.f.to_string()
	}
}
