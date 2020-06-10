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
        match self {
            Type::Boolean => "boolean".fmt(f),
            Type::Integer => "integer".fmt(f),
            Type::Rational => "rational".fmt(f),
            Type::Real => "real".fmt(f),
            Type::String => "string".fmt(f),
            Type::Tuple => "tuple".fmt(f),
            Type::List => "list".fmt(f),
            Type::Empty => "empty list".fmt(f),
            Type::Cons => "non-empty list".fmt(f),
            Type::Closure => "closure".fmt(f),
        }
    }
}
