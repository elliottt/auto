use std::fmt;
use std::rc::Rc;

use crate::pretty::{Pretty, RcDoc};
use crate::types::Type;

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Sequent {
    pub antecedent: Vec<Rc<Type>>,
    pub consequent: Rc<Type>,
}

impl Pretty for Sequent {
    fn pp(&self, _prec: usize) -> RcDoc {
        let res = if !self.antecedent.is_empty() {
            RcDoc::intersperse(self.antecedent.iter().map(|ty| ty.pp(0)), RcDoc::text(", "))
                .append(RcDoc::text(" ⇒ "))
        } else {
            RcDoc::nil()
        };
        res.append(self.consequent.pp(0))
    }
}

impl fmt::Display for Sequent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

impl Sequent {
    pub fn from_type(consequent: Rc<Type>) -> Self {
        Sequent {
            antecedent: Vec::new(),
            consequent,
        }
    }

    pub fn find_assumption(&self, ty: &Type) -> Option<&Rc<Type>> {
        self.antecedent.iter().find(|other| other.as_ref() == ty)
    }
}

#[derive(Debug)]
pub enum Rule {
    Axiom { ty: Rc<Type> },
    ExFalso,
    ImpR,
    AndL { size: usize, ty: Rc<Type> },
    AndR,
    OrInj { ix: usize },
    OrL { arg: Rc<Type> },
    ImpVarL { fun: Rc<Type>, arg: Rc<Type> },
    ImpAndL { fun: Rc<Type> },
    ImpOrL { fun: Rc<Type> },
    ImpImpL { fun: Rc<Type> },
}

impl Pretty for Rule {
    fn pp(&self, _prec: usize) -> RcDoc {
        match self {
            Rule::Axiom { .. } => RcDoc::text("Axiom"),
            Rule::ExFalso => RcDoc::text("Ex-Falso"),
            Rule::ImpR => RcDoc::text("IMP-R"),
            Rule::AndL { .. } => RcDoc::text("AND-L"),
            Rule::AndR => RcDoc::text("AND-R"),
            Rule::OrInj { ix } => RcDoc::text(format!("OR-INJ-{}", ix)),
            Rule::OrL { .. } => RcDoc::text("OR-L"),
            Rule::ImpVarL { .. } => RcDoc::text("IMP-VAR-L"),
            Rule::ImpAndL { .. } => RcDoc::text("IMP-AND-L"),
            Rule::ImpOrL { .. } => RcDoc::text("IMP-OR-L"),
            Rule::ImpImpL { .. } => RcDoc::text("IMP-IMP-L"),
        }
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

#[derive(Debug)]
struct Subgoal {
    rule: Rule,
    goals: Vec<Rc<Sequent>>,
}

// These are steps that will never fail, and are always worth taking to simplify the proof state
fn try_simple(goal: Rc<Sequent>) -> Option<Subgoal> {
    match goal.consequent.as_ref() {
        // ```
        // ----------
        // A , Г => A
        // ```
        Type::Atom { .. } => {
            if let Some(assump) = goal.find_assumption(goal.consequent.as_ref()) {
                return Some(Subgoal {
                    rule: Rule::Axiom { ty: assump.clone() },
                    goals: Vec::new(),
                });
            }
        }

        // ```
        // A, Г => B
        // -----------
        // Г => A -> B
        // ```
        Type::Imp { left, right } => {
            let mut antecedent = Vec::with_capacity(goal.antecedent.len() + 1);
            antecedent.push(left.clone());
            antecedent.extend_from_slice(&goal.antecedent);
            return Some(Subgoal {
                rule: Rule::ImpR,
                goals: vec![Rc::new(Sequent {
                    antecedent,
                    consequent: right.clone(),
                })],
            });
        }

        // ```
        // Г => A  Г => B
        // --------------
        //   Г => A ∧ B
        // ```
        Type::And { args } => {
            return Some(Subgoal {
                rule: Rule::AndR,
                goals: args
                    .iter()
                    .map(|arg| {
                        Rc::new(Sequent {
                            antecedent: goal.antecedent.clone(),
                            consequent: arg.clone(),
                        })
                    })
                    .collect(),
            })
        }

        _ => (),
    }

    for (ix, assump) in goal.antecedent.iter().enumerate() {
        match assump.as_ref() {
            // ```
            // ----------
            // ⊥ , Г => G
            // ```
            Type::Bottom => {
                return Some(Subgoal {
                    rule: Rule::ExFalso,
                    goals: Vec::new(),
                })
            }

            // ```
            // A, B, Г => G
            // -------------
            // A ∧ B, Г => G
            // ```
            //
            // TODO: is it necessary to consume the `A ∧ B` fact?
            Type::And { args } => {
                let mut antecedent = Vec::with_capacity(goal.antecedent.len() + args.len() - 1);

                for arg in args.iter() {
                    antecedent.push(arg.clone())
                }

                antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);

                return Some(Subgoal {
                    rule: Rule::AndL { size: args.len(), ty: assump.clone() },
                    goals: vec![Rc::new(Sequent {
                        antecedent,
                        consequent: goal.consequent.clone(),
                    })],
                });
            }

            Type::Imp { left, right } => {
                // ```
                // B, A, Г => G
                // ----------------
                // A → B, A, Г => G
                // ```
                // NOTE: this doesn't need to consume the argument as removing the implication from
                // the antecedent is enough to ensure that a loop doesn't occur.
                if left.is_atomic() {
                    if let Some(arg) = goal.find_assumption(&left) {
                        let mut antecedent = Vec::with_capacity(goal.antecedent.len());
                        antecedent.push(right.clone());
                        antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                        antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                        return Some(Subgoal {
                            rule: Rule::ImpVarL {
                                fun: assump.clone(),
                                arg: arg.clone(),
                            },
                            goals: vec![Rc::new(Sequent {
                                antecedent,
                                consequent: goal.consequent.clone(),
                            })],
                        });
                    }
                }

                // ```
                // A → (B → C) Г => G
                // -------------------
                // (A ∧ B) → B, Г => G
                // ```
                if let Type::And { args } = left.as_ref() {
                    let mut ty = right.clone();

                    for arg in args.iter().rev() {
                        ty = Type::imp(arg.clone(), ty);
                    }

                    let mut antecedent = Vec::with_capacity(goal.antecedent.len());
                    antecedent.push(ty);
                    antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                    antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                    return Some(Subgoal {
                        rule: Rule::ImpAndL {
                            fun: assump.clone(),
                        },
                        goals: vec![Rc::new(Sequent {
                            antecedent,
                            consequent: goal.consequent.clone(),
                        })],
                    });
                }

                // ```
                // A → C, B → C, Г => G
                // --------------------
                // (A ∨ B) → C, Г => G
                // ```
                if let Type::Or { cases } = left.as_ref() {
                    let mut antecedent =
                        Vec::with_capacity(goal.antecedent.len() + cases.len() - 1);
                    for case in cases.iter() {
                        antecedent.push(Type::imp(Rc::clone(case), Rc::clone(right)));
                    }
                    antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                    antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                    return Some(Subgoal {
                        rule: Rule::ImpOrL {
                            fun: assump.clone(),
                        },
                        goals: vec![Rc::new(Sequent {
                            antecedent,
                            consequent: goal.consequent.clone(),
                        })],
                    });
                }

                // ```
                // B → C, Г => A → B  C, Г => G
                // ----------------------------
                //      (A → B) → C, Г => G
                // ```
                if let Type::Imp { right: ir, .. } = left.as_ref() {
                    let mut lassumps = Vec::with_capacity(goal.antecedent.len());
                    lassumps.push(Type::imp(ir.clone(), right.clone()));
                    lassumps.extend_from_slice(&goal.antecedent[0..ix]);
                    lassumps.extend_from_slice(&goal.antecedent[ix + 1..]);

                    let mut rassumps = Vec::with_capacity(goal.antecedent.len());
                    rassumps.push(right.clone());
                    rassumps.extend_from_slice(&goal.antecedent[0..ix]);
                    rassumps.extend_from_slice(&goal.antecedent[ix + 1..]);

                    return Some(Subgoal {
                        rule: Rule::ImpImpL {
                            fun: assump.clone(),
                        },
                        goals: vec![
                            Rc::new(Sequent {
                                antecedent: lassumps,
                                consequent: left.clone(),
                            }),
                            Rc::new(Sequent {
                                antecedent: rassumps,
                                consequent: goal.consequent.clone(),
                            }),
                        ],
                    });
                }
            }

            _ => (),
        }
    }

    None
}

#[derive(Debug)]
pub struct Proof {
    pub rule: Rule,
    pub premises: Vec<Proof>,
    pub conclusion: Rc<Sequent>,
}

impl Pretty for Proof {
    fn pp(&self, _prec: usize) -> RcDoc {
        RcDoc::concat(self.premises.iter().map(|proof| proof.pp(0).nest(2)))
            .append(RcDoc::line())
            .append(RcDoc::text("------------- ").append(self.rule.pp(0)))
            .append(RcDoc::line())
            .append(self.conclusion.pp(0))
    }
}

impl fmt::Display for Proof {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(0).render_fmt(80, f)
    }
}

pub fn prove(goal: Rc<Sequent>) -> Option<Proof> {
    if let Some(subgoal) = try_simple(goal.clone()) {
        let mut premises = Vec::with_capacity(subgoal.goals.len());
        for goal in subgoal.goals {
            if let Some(proof) = prove(goal) {
                premises.push(proof);
            } else {
                return None;
            }
        }
        return Some(Proof {
            rule: subgoal.rule,
            premises,
            conclusion: goal,
        });
    }

    // try proving the branches of an or
    if let Type::Or { cases } = goal.consequent.as_ref() {
        for (ix, case) in cases.iter().enumerate() {
            if let Some(lproof) = prove(Rc::new(Sequent {
                antecedent: goal.antecedent.clone(),
                consequent: Rc::clone(case),
            })) {
                return Some(Proof {
                    rule: Rule::OrInj { ix },
                    premises: vec![lproof],
                    conclusion: goal,
                });
            }
        }
    }

    for (ix, assump) in goal.antecedent.iter().enumerate() {
        match assump.as_ref() {
            // try proving the goal in terms of an or in the environment
            Type::Or { cases } => {
                let lassumps = &goal.antecedent[0..ix];
                let rassumps = &goal.antecedent[ix + 1..];

                let mut premises = Vec::with_capacity(cases.len());
                for case in cases.iter() {
                    let mut antecedent = Vec::with_capacity(goal.antecedent.len());
                    antecedent.push(Rc::clone(case));
                    antecedent.extend_from_slice(lassumps);
                    antecedent.extend_from_slice(rassumps);

                    if let Some(proof) = prove(Rc::new(Sequent {
                        antecedent,
                        consequent: Rc::clone(&goal.consequent),
                    })) {
                        premises.push(proof);
                    } else {
                        break;
                    }
                }

                if premises.len() != cases.len() {
                    continue;
                }

                return Some(Proof {
                    rule: Rule::OrL {
                        arg: assump.clone(),
                    },
                    premises,
                    conclusion: goal,
                });
            }

            _ => (),
        }
    }

    None
}
