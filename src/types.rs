use std::fmt;

/// Gynjo data types.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Boolean,
    Integer,
    Rational,
    Real,
    String,
    Tuple,
    List,
    Empty,
    Cons,
    Closure,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Type::Boolean => "boolean",
            Type::Integer => "integer",
            Type::Rational => "rational",
            Type::Real => "real",
            Type::String => "string",
            Type::Tuple => "tuple",
            Type::List => "list",
            Type::Empty => "empty list",
            Type::Cons => "non-empty list",
            Type::Closure => "closure",
        })
    }
}
