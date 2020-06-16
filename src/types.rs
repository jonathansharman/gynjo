use std::fmt;

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
    List,
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
            Type::List => write!(f, "list"),
            Type::Closure => write!(f, "closure"),
            Type::Returned => write!(f, "returned_value"),
        }
    }
}
