use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty, RcDoc};
use crate::prove::{Proof, Rule};
use crate::types::Type;

#[derive(Debug)]
pub struct Binding {
    pub lhs: Box<Term>,
    pub rhs: Box<Term>,
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

#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pat: Box<Term>,
    pub body: Box<Term>,
}

impl Pretty for MatchArm {
    fn pp(&self, _prec: usize) -> RcDoc {
        self.pat
            .pp(0)
            .append(RcDoc::text(" -> "))
            .append(self.body.pp(0).nest(2))
    }
}

#[derive(Debug, Clone)]
pub enum Term {
    Lambda {
        var: String,
        ty: Rc<Type>,
        body: Box<Term>,
    },
    App {
        fun: Box<Term>,
        arg: Box<Term>,
    },
    Var {
        var: String,
    },
    Tuple {
        elems: Vec<Box<Term>>,
    },
    Let {
        lhs: Box<Term>,
        rhs: Box<Term>,
        body: Box<Term>,
    },
    Match {
        arg: Box<Term>,
        arms: Vec<MatchArm>,
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
                    .append(body.pp(0).nest(if body.is_lambda() { 0 } else { 2 })),
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
                RcDoc::line()
                    .append(RcDoc::text("let "))
                    .append(lhs.pp(10))
                    .append(RcDoc::text(" = "))
                    .append(rhs.pp(0))
                    .append(RcDoc::text(" in "))
                    .append(body.pp(0)),
            ),

            Term::Match { arg, arms } => pretty::parens(
                prec >= 5,
                RcDoc::line()
                    .append(RcDoc::text("match "))
                    .append(arg.pp(0))
                    .append(RcDoc::space())
                    .append(RcDoc::text("{"))
                    .append(
                        RcDoc::concat(arms.iter().map(|arm| RcDoc::line().append(arm.pp(0))))
                            .nest(2),
                    )
                    .append(RcDoc::line())
                    .append(RcDoc::text("}")),
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
    pub fn is_lambda(&self) -> bool {
        if let Term::Lambda { .. } = self {
            true
        } else {
            false
        }
    }

    pub fn from_proof(proof: &Proof) -> Box<Term> {
        let mut env = Env::new();
        Term::simplify(env.from_proof(proof))
    }

    pub fn constr(name: &str, arg: Box<Term>) -> Box<Term> {
        Box::new(Term::App {
            fun: Box::new(Term::Var {
                var: String::from(name),
            }),
            arg,
        })
    }

    pub fn app(fun: Box<Term>, arg: Box<Term>) -> Box<Term> {
        Box::new(Term::App { fun, arg })
    }

    pub fn var(var: String) -> Box<Term> {
        Box::new(Term::Var { var })
    }

    pub fn simplify(tm: Box<Term>) -> Box<Term> {
        match *tm {
            Term::Lambda { var, ty, body } => {
                let body = Term::simplify(body);

                if let Term::App {
                    fun: body_fun,
                    arg: body_arg,
                } = body.as_ref()
                {
                    if let Term::Var { var: arg_var } = body_arg.as_ref() {
                        if var.eq(arg_var) {
                            // \x. f x ==> f
                            return body_fun.clone();
                        }
                    }
                }

                Box::new(Term::Lambda { var, ty, body })
            }
            Term::App { fun, arg } => {
                let fun = Term::simplify(fun);
                let arg = Term::simplify(arg);

                Box::new(Term::App { fun, arg })
            }
            Term::Var { .. } => tm,
            Term::Tuple { elems } => Box::new(Term::Tuple {
                elems: elems.into_iter().map(Term::simplify).collect(),
            }),
            Term::Let { lhs, rhs, body } => {
                let lhs = Term::simplify(lhs);
                let rhs = Term::simplify(rhs);
                let body = Term::simplify(body);

                if let Term::Var { var: lhs_name } = lhs.as_ref() {
                    if let Term::Var { var: body_name } = body.as_ref() {
                        if body_name.eq(lhs_name) {
                            // let x = y in x ==> y
                            return rhs;
                        }
                    }
                }

                Box::new(Term::Let { lhs, rhs, body })
            }
            Term::Match { arg, arms } => Box::new(Term::Match {
                arg: Term::simplify(arg),
                arms: arms
                    .into_iter()
                    .map(|arm| MatchArm {
                        pat: Term::simplify(arm.pat),
                        body: Term::simplify(arm.body),
                    })
                    .collect(),
            }),
        }
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

    fn var(&mut self, ty: &Rc<Type>) -> Box<Term> {
        Box::new(Term::Var { var: self.name(ty) })
    }

    fn make_match_arm(&mut self, con: &str, proof: &Proof) -> MatchArm {
        let arg = self.name(&proof.conclusion.antecedent[0]);
        let pat = Term::constr(con, Box::new(Term::Var { var: arg }));
        let body = self.from_proof(proof);
        MatchArm { pat, body }
    }

    fn from_proof(&mut self, proof: &Proof) -> Box<Term> {
        match proof.rule {
            Rule::Axiom { ref ty } => {
                let var = self.name(ty);
                return Box::new(Term::Var { var });
            }
            Rule::ExFalso => {
                return Box::new(Term::Var {
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
                return Box::new(Term::Lambda {
                    var,
                    ty: ty.clone(),
                    body,
                });
            }
            Rule::AndL { ref ty } => {
                let premise = &proof.premises[0];
                let lhs = Box::new(Term::Tuple {
                    elems: vec![
                        Box::new(Term::Var {
                            var: self.name(&premise.conclusion.antecedent[0]),
                        }),
                        Box::new(Term::Var {
                            var: self.name(&premise.conclusion.antecedent[1]),
                        }),
                    ],
                });
                let rhs = Box::new(Term::Var {
                    var: self.name(&ty),
                });
                return Box::new(Term::Let {
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
                return Box::new(Term::Tuple { elems });
            }
            Rule::OrInjL => {
                let premise = &proof.premises[0];
                let arg = self.from_proof(&premise);
                let fun = Box::new(Term::Var {
                    var: String::from("Left"),
                });
                return Box::new(Term::App { fun, arg });
            }
            Rule::OrInjR => {
                let premise = &proof.premises[0];
                let arg = self.from_proof(&premise);
                let fun = Box::new(Term::Var {
                    var: String::from("Right"),
                });
                return Box::new(Term::App { fun, arg });
            }
            Rule::OrL { ref arg } => {
                let var = Box::new(Term::Var {
                    var: self.name(arg),
                });
                let lproof = &proof.premises[0];
                let rproof = &proof.premises[1];
                return Box::new(Term::Match {
                    arg: var,
                    arms: vec![
                        self.make_match_arm("Left", lproof),
                        self.make_match_arm("Right", rproof),
                    ],
                });
            }
            Rule::ImpVarL { ref fun, ref arg } => {
                let cont = &proof.premises[0];
                let lhs = self.var(&cont.conclusion.antecedent[0]);
                let fun = self.var(fun);
                let arg = self.var(arg);

                return Box::new(Term::Let {
                    lhs,
                    rhs: Box::new(Term::App { fun, arg }),
                    body: self.from_proof(cont),
                });
            }
            Rule::ImpAndL { ref fun } => {
                let cont = &proof.premises[0];
                let curried = &cont.conclusion.antecedent[0];

                let lhs = self.var(curried);
                let rhs = self.lambda(curried, |env1, a, k| {
                    env1.lambda(k, |env2, b, _| {
                        Term::app(
                            env2.var(fun),
                            Box::new(Term::Tuple {
                                elems: vec![Term::var(a.clone()), Term::var(b)],
                            }),
                        )
                    })
                });

                return Box::new(Term::Let {
                    lhs,
                    rhs,
                    body: self.from_proof(cont),
                });
            }
            Rule::ImpOrL { ref fun } => {
                let cont = &proof.premises[0];
                let lty = &cont.conclusion.antecedent[0];
                let lfun = self.lambda(lty, |env, var, _| {
                    Term::app(env.var(fun), Term::constr("Left", Term::var(var)))
                });
                let rty = &cont.conclusion.antecedent[1];
                let rfun = self.lambda(rty, |env, var, _| {
                    Term::app(env.var(fun), Term::constr("Right", Term::var(var)))
                });

                return Box::new(Term::Let {
                    lhs: self.var(lty),
                    rhs: lfun,
                    body: Box::new(Term::Let {
                        lhs: self.var(rty),
                        rhs: rfun,
                        body: self.from_proof(cont),
                    }),
                });
            }
            Rule::ImpImpL { ref fun } => {
                let cont = &proof.premises[1];
                let lhs = self.var(&cont.conclusion.antecedent[0]);
                let fun = self.var(fun);
                let arg = self.from_proof(&proof.premises[0]);

                return Box::new(Term::Let {
                    lhs,
                    rhs: Box::new(Term::App { fun, arg }),
                    body: self.from_proof(cont),
                });
            }
        }
    }

    pub fn lambda<MkBody>(&mut self, ty: &Type, mk_body: MkBody) -> Box<Term>
    where
        MkBody: FnOnce(&mut Self, String, &Type) -> Box<Term>,
    {
        if let Type::Imp { left, right } = ty {
            let var = self.name(left);
            let body = mk_body(self, var.clone(), right);
            Box::new(Term::Lambda {
                var,
                ty: left.clone(),
                body,
            })
        } else {
            panic!("lambda: type given was not an Imp");
        }
    }
}
