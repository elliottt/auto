use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty};
use crate::prove::{Proof, Rule};
use crate::types::Type;

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
}

impl Pretty for Term {
    fn pp(&self, f: &mut fmt::Formatter<'_>, prec: usize) -> fmt::Result {
        match self {
            Term::Lambda { var, ty, body } => {
                pretty::parens(f, prec >= 5, |f| {
                    write!(f, "\\ {}: ", var)?;
                    ty.pp(f, 0)?;
                    write!(f, ". ")?;
                    body.pp(f, 0)
                })?;
            }

            Term::App { fun, arg } => {
                pretty::parens(f, prec >= 5, |f| {
                    fun.pp(f, 0)?;
                    write!(f, " ")?;
                    arg.pp(f, 5)
                })?;
            }

            Term::Var { var } => {
                write!(f, "{}", var)?;
            }
        }
        Ok(())
    }
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(f, 0)
    }
}

impl Term {
    pub fn from_proof(proof: &Proof) -> Rc<Term> {
        let mut env = Env::new();
        env.from_proof(proof)
    }
}

struct Env<'a> {
    vars: HashMap<&'a Type, String>,
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

impl<'a> Env<'a> {
    fn new() -> Self {
        Env {
            vars: HashMap::new(),
            next: 0,
        }
    }

    fn name(&mut self, ty: &'a Type) -> String {
        if let Some(name) = self.vars.get(ty) {
            name.clone()
        } else {
            let name = next_name(self.next);
            self.next += 1;
            self.vars.insert(ty, name.clone());
            name
        }
    }

    fn from_proof(&mut self, proof: &'a Proof) -> Rc<Term> {
        match proof.rule {
            Rule::Axiom => {
                let var = self.name(&proof.conclusion.consequent);
                return Rc::new(Term::Var { var });
            }
            Rule::ExFalso => {}
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
            Rule::AndL => {}
            Rule::AndR => {}
            Rule::OrInjL => {}
            Rule::OrInjR => {
                let premise = &proof.premises[0];
                let arg = self.from_proof(&premise);
                let fun = Rc::new(Term::Var {
                    var: String::from("Right"),
                });
                return Rc::new(Term::App { fun, arg });
            }
            Rule::OrL => {}
            Rule::ImpVarL => {}
            Rule::ImpAndL => {}
            Rule::ImpOrL => {}
            Rule::ImpImpL => {}
        }

        panic!("whoops")
    }
}
