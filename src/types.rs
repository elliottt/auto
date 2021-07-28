use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty, RcDoc};

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Atom { name: String },

    Imp { left: Rc<Type>, right: Rc<Type> },

    And { left: Rc<Type>, right: Rc<Type> },

    Or { left: Rc<Type>, right: Rc<Type> },

    Bottom,
}

impl pretty::Pretty for Type {
    fn pp(&self, prec: usize) -> RcDoc {
        match self {
            Type::Atom { name } => name.pp(0),
            Type::Imp { left, right } => pretty::parens(
                prec >= 1,
                left.pp(1).append(RcDoc::text(" → ")).append(right.pp(0)),
            ),
            Type::And { left, right } => pretty::parens(
                prec >= 3,
                left.pp(3).append(RcDoc::text(" ∧ ")).append(right.pp(3)),
            ),
            Type::Or { left, right } => pretty::parens(
                prec >= 2,
                left.pp(2).append(RcDoc::text(" ∨ ")).append(right.pp(2)),
            ),
            Type::Bottom => RcDoc::text("⊥"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

impl Type {
    pub fn is_atomic(&self) -> bool {
        match self {
            Type::Atom { .. } => true,
            _ => false,
        }
    }

    pub fn atom(name: &str) -> Self {
        Type::Atom {
            name: name.to_string(),
        }
    }

    pub fn imp(left: Rc<Self>, right: Rc<Self>) -> Self {
        Type::Imp { left, right }
    }

    pub fn and(left: Rc<Self>, right: Rc<Self>) -> Self {
        Type::And { left, right }
    }

    pub fn or(left: Rc<Self>, right: Rc<Self>) -> Self {
        Type::Or { left, right }
    }
}
