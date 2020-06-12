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
            ListType::Empty => write!(f, "empty_list"),
            ListType::Cons => write!(f, "nonempty_list"),
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
    Returned,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Type => write!(f, "type"),
            Type::Boolean => write!(f, "boolean"),
            Type::Integer => write!(f, "integer"),
            Type::Rational => write!(f, "rational"),
            Type::Real => write!(f, "real"),
            Type::String => write!(f, "string"),
            Type::Tuple => write!(f, "tuple"),
            Type::List(list_type) => list_type.fmt(f),
            Type::Closure => write!(f, "closure"),
            Type::Returned => write!(f, "returned_value"),
        }
    }
}
