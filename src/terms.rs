use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty, RcDoc};
use crate::prove::{Proof, Rule};
use crate::types::Type;

#[derive(Debug)]
pub struct Binding {
    pub lhs: Rc<Term>,
    pub rhs: Rc<Term>,
}

impl Pretty for Binding {
    fn pp(&self, _prec: usize) -> RcDoc {
        self.lhs
            .pp(10)
            .append(RcDoc::text(" = "))
            .append(self.rhs.pp(0))
    }
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

#[derive(Debug)]
pub enum Term {
    Lambda {
        var: String,
        ty: Rc<Type>,
        body: Rc<Term>,
    },
    App {
        fun: Rc<Term>,
        arg: Rc<Term>,
    },
    Var {
        var: String,
    },
    Tuple {
        elems: Vec<Rc<Term>>,
    },
    Let {
        lhs: Rc<Term>,
        rhs: Rc<Term>,
        body: Rc<Term>,
    },
}

impl Pretty for Term {
    fn pp(&self, prec: usize) -> RcDoc {
        match self {
            Term::Lambda { var, ty, body } => pretty::parens(
                prec >= 5,
                RcDoc::text("λ ")
                    .append(RcDoc::text(var))
                    .append(RcDoc::text(": "))
                    .append(ty.pp(0))
                    .append(RcDoc::text(". "))
                    .append(body.pp(0)),
            ),

            Term::App { fun, arg } => pretty::parens(
                prec >= 5,
                fun.pp(0).append(RcDoc::text(" ")).append(arg.pp(5)),
            ),

            Term::Var { var } => RcDoc::text(var),

            Term::Tuple { elems } => pretty::parens(
                true,
                RcDoc::intersperse(elems.iter().map(|term| term.pp(0)), RcDoc::text(", ")),
            ),

            Term::Let { lhs, rhs, body } => pretty::parens(
                prec >= 5,
                RcDoc::text("let ")
                    .append(lhs.pp(10))
                    .append(RcDoc::text(" = "))
                    .append(rhs.pp(0))
                    .append(RcDoc::text(" in "))
                    .append(body.pp(0)),
            ),
        }
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

impl Term {
    pub fn from_proof(proof: &Proof) -> Rc<Term> {
        let mut env = Env::new();
        env.from_proof(proof)
    }
}

struct Env {
    vars: HashMap<*const Type, String>,
    next: usize,
}

const LETTERS: &'static str = "abcdefghijklmnopqrstuvwxyz";

fn next_name(mut ix: usize) -> String {
    let mut name = String::new();
    loop {
        let chr = ix % 26;
        ix = ix / 26;
        name.push(LETTERS.chars().nth(chr).unwrap());
        if ix == 0 {
            return name;
        }
    }
}

impl Env {
    fn new() -> Self {
        Env {
            vars: HashMap::new(),
            next: 0,
        }
    }

    fn name(&mut self, ty: &Rc<Type>) -> String {
        let ptr = Rc::as_ptr(ty);
        if let Some(name) = self.vars.get(&ptr) {
            name.clone()
        } else {
            let name = next_name(self.next);
            self.next += 1;
            self.vars.insert(ptr, name.clone());
            name
        }
    }

    fn from_proof(&mut self, proof: &Proof) -> Rc<Term> {
        match proof.rule {
            Rule::Axiom { ref ty } => {
                let var = self.name(ty);
                return Rc::new(Term::Var { var });
            }
            Rule::ExFalso => {
                return Rc::new(Term::Var {
                    var: String::from("undefined"),
                });
            }
            Rule::ImpR => {
                // this will have one assumption with the variable introduced on the left, and the
                // body on the right
                let premise = &proof.premises[0];
                let ty = &premise.conclusion.antecedent[0];
                let var = self.name(&ty);
                let body = self.from_proof(&premise);
                return Rc::new(Term::Lambda {
                    var,
                    ty: ty.clone(),
                    body,
                });
            }
            Rule::AndL { ref ty } => {
                let premise = &proof.premises[0];
                let lhs = Rc::new(Term::Tuple {
                    elems: vec![
                        Rc::new(Term::Var {
                            var: self.name(&premise.conclusion.antecedent[0]),
                        }),
                        Rc::new(Term::Var {
                            var: self.name(&premise.conclusion.antecedent[1]),
                        }),
                    ],
                });
                let rhs = Rc::new(Term::Var {
                    var: self.name(&ty),
                });
                return Rc::new(Term::Let {
                    lhs,
                    rhs,
                    body: self.from_proof(premise),
                });
            }
            Rule::AndR => {
                let elems = vec![
                    self.from_proof(&proof.premises[0]),
                    self.from_proof(&proof.premises[1]),
                ];
                return Rc::new(Term::Tuple { elems });
            }
            Rule::OrInjL => {
                let premise = &proof.premises[0];
                let arg = self.from_proof(&premise);
                let fun = Rc::new(Term::Var {
                    var: String::from("Left"),
                });
                return Rc::new(Term::App { fun, arg });
            }
            Rule::OrInjR => {
                let premise = &proof.premises[0];
                let arg = self.from_proof(&premise);
                let fun = Rc::new(Term::Var {
                    var: String::from("Right"),
                });
                return Rc::new(Term::App { fun, arg });
            }
            Rule::OrL => {}
            Rule::ImpVarL { ref fun, ref arg } => {
                return Rc::new(Term::App {
                    fun: Rc::new(Term::Var {
                        var: self.name(fun),
                    }),
                    arg: Rc::new(Term::Var {
                        var: self.name(arg),
                    }),
                });
            }
            Rule::ImpAndL => {}
            Rule::ImpOrL => {}
            Rule::ImpImpL => {}
        }

        panic!("whoops")
    }
}
