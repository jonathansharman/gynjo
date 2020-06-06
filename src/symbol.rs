/// A Gynjo symbol, e.g. a variable name.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Sym { pub name: String }

impl <S> From<S> for Sym where S: Into<String> {
    fn from(name: S) -> Self {
        Sym { name: name.into() }
    }
}

impl Sym {
    pub fn to_string(&self) -> String {
        self.name.clone()
    }
}
