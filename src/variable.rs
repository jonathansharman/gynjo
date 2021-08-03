use crate::{symbol::Sym, types::Type};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Variable {
	pub name: Sym,
	pub type_: Type,
}
