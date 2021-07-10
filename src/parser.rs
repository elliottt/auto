
use std::fmt;
use std::rc::Rc;
use pest::{ Parser, iterators::Pair };

use crate::pretty::{self, Pretty};

#[derive(Parser)]
#[grammar = "type.pest"]
pub struct TypeParser;

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Var { name: String },

    Imp { left: Rc<Type>, right: Rc<Type> },

    And { left: Rc<Type>, right: Rc<Type> },

    Or { left: Rc<Type>, right: Rc<Type> },

    Bottom,
}

impl pretty::Pretty for Type {
    fn pp(&self, f: &mut fmt::Formatter<'_>, prec: usize) -> fmt::Result {
        match self {
            Type::Var { name } => name.pp(f, 0),
            Type::Imp { left, right } => pretty::parens(f, prec > 1, |f| {
                left.pp(f, 1)?;
                write!(f, " → ")?;
                right.pp(f, 1)
            }),
            Type::And { left, right } => pretty::parens(f, prec > 3, |f| {
                left.pp(f, 3)?;
                write!(f, " ∧ ")?;
                right.pp(f, 3)
            }),
            Type::Or { left, right } => pretty::parens(f, prec > 2, |f| {
                left.pp(f, 2)?;
                write!(f, " ∨ ")?;
                right.pp(f, 2)
            }),
            Type::Bottom => write!(f, "⊥"),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(f, 0)
    }
}

impl Type {
    pub fn is_atomic(&self) -> bool {
        match self {
            Type::Var { .. } => true,
            _ => false,
        }
    }

    pub fn var(name: &str) -> Self {
        Type::Var {
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

    pub fn bottom() -> Self {
        Type::Bottom
    }

    pub fn not(ty: Rc<Self>) -> Self {
        Self::imp(ty, Rc::new(Self::bottom()))
    }

    pub fn parse(input: &str) -> Rc<Self> {
        let res = TypeParser::parse(Rule::type_, input)
            .expect("Failed to parse type")
            .next().unwrap();


        Self::parse_value(res)
    }

    fn parse_value(pair: Pair<Rule>) -> Rc<Self> {
        match pair.as_rule() {
            Rule::type_ => {
                return Self::parse_value(pair.into_inner().nth(0).unwrap())
            }

            Rule::iexp => {
                let mut pairs = pair.into_inner();
                let lhs = Self::parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Rc::new(Type::imp(lhs, Self::parse_value(rhs)))
                } else {
                    return lhs
                }
            }

            Rule::aexp => {
                let mut pairs = pair.into_inner();
                let lhs = Self::parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Rc::new(Type::and(lhs, Self::parse_value(rhs)))
                } else {
                    return lhs
                }
            }

            Rule::oexp => {
                let mut pairs = pair.into_inner();
                let lhs = Self::parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Rc::new(Type::or(lhs, Self::parse_value(rhs)))
                } else {
                    return lhs
                }
            }

            Rule::atomic => {
                let mut pairs = pair.into_inner();
                return Self::parse_value(pairs.next().unwrap())
            }

            Rule::atom => {
                return Rc::new(Type::var(pair.as_str()))
            }

            Rule::bottom => {
                return Rc::new(Type::Bottom)
            }

            _ => {
                println!("UNHANDLED: {:?}", pair);
                return Rc::new(Type::Bottom)
            }
        }
    }
}
