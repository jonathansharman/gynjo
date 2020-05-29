/// A Gynjo symbol, e.g. a variable name.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct Symbol { pub name: String }

impl <S> From<S> for Symbol where S: Into<String> {
    fn from(name: S) -> Self {
        Symbol { name: name.into() }
    }
}

impl Symbol {
    pub fn to_string(&self) -> String {
        self.name.clone()
    }
}
