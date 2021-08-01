use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty, RcDoc};

#[derive(Debug, Default)]
pub struct Data {
    pub name: String,
    pub vars: Vec<String>,
    pub constrs: Vec<(String, Rc<Type>)>,
}

impl Data {
    pub fn as_type(&self) -> Rc<Type> {
        Type::app(
            Type::atom(&self.name),
            self.vars.iter().map(|var| Type::atom(var)),
        )
    }
}

impl pretty::Pretty for Data {
    fn pp(&self, _prec: usize) -> RcDoc {
        let mut doc = RcDoc::text("data")
            .append(RcDoc::space())
            .append(RcDoc::text(&self.name))
            .append(RcDoc::space())
            .append(RcDoc::intersperse(
                self.vars.iter().map(RcDoc::text),
                RcDoc::space(),
            ));

        if !self.constrs.is_empty() {
            doc = doc.append(
                RcDoc::concat(self.constrs.iter().map(|(name, ty)| {
                    RcDoc::line()
                        .append(RcDoc::text(name))
                        .append(RcDoc::space())
                        .append(RcDoc::text("::"))
                        .append(RcDoc::space())
                        .append(ty.pp(0))
                }))
                .nest(2),
            )
        }

        doc
    }
}

impl fmt::Display for Data {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum Type {
    Atom { name: String },

    Imp { left: Rc<Type>, right: Rc<Type> },

    App { fun: Rc<Type>, args: Vec<Rc<Type>> },

    And { args: Vec<Rc<Type>> },

    Or { cases: Vec<Rc<Type>> },

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
            Type::App { fun, args } => pretty::parens(
                prec >= 1,
                fun.pp(0).append(RcDoc::space()).append(RcDoc::intersperse(
                    args.iter().map(|arg| arg.pp(1)),
                    RcDoc::space(),
                )),
            ),
            Type::And { args } => pretty::parens(
                prec >= 3,
                RcDoc::intersperse(args.iter().map(|arg| arg.pp(3)), RcDoc::text(" ∧ ")),
            ),
            Type::Or { cases } => pretty::parens(
                prec >= 2,
                RcDoc::intersperse(cases.iter().map(|case| case.pp(2)), RcDoc::text(" ∨ ")),
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

    pub fn app<Args>(fun: Rc<Type>, args: Args) -> Rc<Self>
    where
        Args: IntoIterator<Item = Rc<Self>>,
    {
        let args: Vec<Rc<Self>> = args.into_iter().collect();
        if args.is_empty() {
            fun
        } else {
            Rc::new(Type::App { fun, args })
        }
    }

    pub fn atom(name: &str) -> Rc<Self> {
        Rc::new(Type::Atom {
            name: String::from(name),
        })
    }

    pub fn imp(left: Rc<Self>, right: Rc<Self>) -> Rc<Self> {
        Rc::new(Type::Imp { left, right })
    }

    pub fn and<Args>(args: Args) -> Rc<Self>
    where
        Args: IntoIterator<Item = Rc<Self>>,
    {
        let args: Vec<Rc<Self>> = args.into_iter().collect();
        if args.len() == 1 {
            Rc::clone(&args[0])
        } else {
            Rc::new(Type::And { args })
        }
    }

    pub fn or<Cases>(cases: Cases) -> Rc<Self>
    where
        Cases: IntoIterator<Item = Rc<Self>>,
    {
        let cases: Vec<Rc<Self>> = cases.into_iter().collect();
        if cases.len() == 1 {
            Rc::clone(&cases[0])
        } else {
            Rc::new(Type::Or { cases })
        }
    }

    pub fn bottom() -> Rc<Self> {
        Rc::new(Type::Bottom)
    }
}
