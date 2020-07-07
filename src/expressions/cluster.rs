use super::Expr;

use itertools::Itertools;

use std::fmt;

/// The way in which a cluster item is attached to the preceding element of the cluster.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ClusterConnector {
	/// Item is the first in the cluster.
	None,
	/// Adjacent value enclosed in parentheses
	AdjParen,
	/// Adjacent value not enclosed in parentheses
	AdjNonparen,
	/// Explicit multiplication
	Mul,
	/// Explicit division
	Div,
	/// Explicit exponentiation
	Exp,
}

/// A single item in an expression `Cluster`.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct ClusterItem {
	/// This item's expression.
	pub expr: Box<Expr>,
	/// Determines whether this item is negated.
	pub negated: bool,
	/// How this item is connected to the previous item.
	pub connector: ClusterConnector,
}

impl fmt::Display for ClusterItem {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}{}{}{}",
			if self.negated { "-" } else { "" },
			match self.connector {
				ClusterConnector::None => "",
				ClusterConnector::AdjParen => "(",
				ClusterConnector::AdjNonparen => " ",
				ClusterConnector::Mul => " * ",
				ClusterConnector::Div => " / ",
				ClusterConnector::Exp => " ^ ",
			},
			self.expr,
			if let ClusterConnector::AdjParen = self.connector { ")" } else { "" },
		)
	}
}

/// A cluster of function calls, exponentiations, (possibly implicit) multiplications, and/or divisions.
///
/// This large grouping of operations is as fine-grained as possible in the parsing stage. Breaking this down
/// into specific operations requires additional parsing in the evaluation stage since determining the order of
/// operations requires type info.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Cluster {
	pub negated: bool,
	pub items: Vec<ClusterItem>,
}

impl fmt::Display for Cluster {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "({})", self.items.iter().map(ClusterItem::to_string).join(""))
	}
}
