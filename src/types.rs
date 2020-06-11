use std::fmt;

/// Gynjo list type values.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ListType {
    Empty,
    Cons,
}

impl fmt::Display for ListType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ListType::Empty => "empty list".fmt(f),
            ListType::Cons => "non-empty list".fmt(f),
        }
    }
}

/// Gynjo type values.
#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum Type {
    Type,
    Boolean,
    Integer,
    Rational,
    Real,
    String,
    Tuple,
    List(ListType),
    Closure,
    Returned(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Type => "type".fmt(f),
            Type::Boolean => "boolean".fmt(f),
            Type::Integer => "integer".fmt(f),
            Type::Rational => "rational".fmt(f),
            Type::Real => "real".fmt(f),
            Type::String => "string".fmt(f),
            Type::Tuple => "tuple".fmt(f),
            Type::List(list_type) => list_type.fmt(f),
            Type::Closure => "closure".fmt(f),
            Type::Returned(val) => write!(f, "returned({})", val),
        }
    }
}
