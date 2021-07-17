use std::fmt;
use std::rc::Rc;

use crate::pretty::{self, Pretty};
use crate::types::Type;

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct Sequent {
    pub antecedent: Vec<Rc<Type>>,
    pub consequent: Rc<Type>,
}

impl Pretty for Sequent {
    fn pp(&self, f: &mut fmt::Formatter<'_>, _prec: usize) -> fmt::Result {
        if !self.antecedent.is_empty() {
            pretty::commas(f, &self.antecedent)?;
            write!(f, " ⇒ ")?;
        }
        self.consequent.pp(f, 0)
    }
}

impl fmt::Display for Sequent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(f, 0)
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

    pub fn has_assumption(&self, ty: &Type) -> bool {
        self.find_assumption(ty).is_some()
    }
}

#[derive(Debug)]
pub enum Rule {
    Axiom,
    ExFalso,
    ImpR,
    AndL { ty: Rc<Type> },
    AndR,
    OrInjL,
    OrInjR,
    OrL,
    ImpVarL { fun: Rc<Type>, arg: Rc<Type> },
    ImpAndL,
    ImpOrL,
    ImpImpL,
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rule::Axiom => write!(f, "Axiom"),
            Rule::ExFalso => write!(f, "Ex-Falso"),
            Rule::ImpR => write!(f, "IMP-R"),
            Rule::AndL { .. } => write!(f, "AND-L"),
            Rule::AndR => write!(f, "AND-R"),
            Rule::OrInjL => write!(f, "OR-INJ-L"),
            Rule::OrInjR => write!(f, "OR-INJ-R"),
            Rule::OrL => write!(f, "OR-L"),
            Rule::ImpVarL { .. } => write!(f, "IMP-VAR-L"),
            Rule::ImpAndL => write!(f, "IMP-AND-L"),
            Rule::ImpOrL => write!(f, "IMP-OR-L"),
            Rule::ImpImpL => write!(f, "IMP-IMP-L"),
        }
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
        _ if goal.has_assumption(goal.consequent.as_ref()) => {
            return Some(Subgoal {
                rule: Rule::Axiom,
                goals: Vec::new(),
            })
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
        Type::And { left, right } => {
            return Some(Subgoal {
                rule: Rule::AndR,
                goals: vec![
                    Rc::new(Sequent {
                        antecedent: goal.antecedent.clone(),
                        consequent: left.clone(),
                    }),
                    Rc::new(Sequent {
                        antecedent: goal.antecedent.clone(),
                        consequent: right.clone(),
                    }),
                ],
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
            Type::And { left, right } => {
                let mut antecedent = Vec::with_capacity(goal.antecedent.len() + 1);
                antecedent.push(left.clone());
                antecedent.push(right.clone());
                antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                return Some(Subgoal {
                    rule: Rule::AndL { ty: assump.clone() },
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
                // TODO: should this consume the argument as well?
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
                if let Type::And {
                    left: al,
                    right: ar,
                } = left.as_ref()
                {
                    let mut antecedent = Vec::with_capacity(goal.antecedent.len());
                    antecedent.push(Rc::new(Type::imp(
                        al.clone(),
                        Rc::new(Type::imp(ar.clone(), right.clone())),
                    )));
                    antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                    antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                    return Some(Subgoal {
                        rule: Rule::ImpAndL,
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
                if let Type::Or {
                    left: ol,
                    right: or,
                } = left.as_ref()
                {
                    let mut antecedent = Vec::with_capacity(goal.antecedent.len() + 1);
                    antecedent.push(Rc::new(Type::imp(ol.clone(), right.clone())));
                    antecedent.push(Rc::new(Type::imp(or.clone(), right.clone())));
                    antecedent.extend_from_slice(&goal.antecedent[0..ix]);
                    antecedent.extend_from_slice(&goal.antecedent[ix + 1..]);
                    return Some(Subgoal {
                        rule: Rule::ImpOrL,
                        goals: vec![Rc::new(Sequent {
                            antecedent,
                            consequent: goal.consequent.clone(),
                        })],
                    });
                }

                // ```
                // B → C, Г => A → B  B, Г => G
                // ----------------------------
                //      (A → B) → C, Г => G
                // ```
                if let Type::Imp { right: ir, .. } = left.as_ref() {
                    let mut lassumps = Vec::with_capacity(goal.antecedent.len());
                    lassumps.push(Rc::new(Type::imp(ir.clone(), right.clone())));
                    lassumps.extend_from_slice(&goal.antecedent[0..ix]);
                    lassumps.extend_from_slice(&goal.antecedent[ix + 1..]);

                    let mut rassumps = Vec::with_capacity(goal.antecedent.len());
                    rassumps.push(right.clone());
                    rassumps.extend_from_slice(&goal.antecedent[0..ix]);
                    rassumps.extend_from_slice(&goal.antecedent[ix + 1..]);

                    return Some(Subgoal {
                        rule: Rule::ImpImpL,
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

                return None;
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
    fn pp(&self, f: &mut fmt::Formatter<'_>, _prec: usize) -> fmt::Result {
        for premise in &self.premises {
            premise.pp(f, 0)?;
        }
        write!(f, "------------- {}\n", self.rule)?;
        self.conclusion.pp(f, 0)?;
        writeln!(f, "")
    }
}

impl fmt::Display for Proof {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.pp(f, 0)
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
    if let Type::Or { left, right } = goal.consequent.as_ref() {
        let lgoal = Rc::new(Sequent {
            antecedent: goal.antecedent.clone(),
            consequent: left.clone(),
        });
        if let Some(lproof) = prove(lgoal) {
            return Some(Proof {
                rule: Rule::OrInjL,
                premises: vec![lproof],
                conclusion: goal,
            });
        }

        let rgoal = Rc::new(Sequent {
            antecedent: goal.antecedent.clone(),
            consequent: right.clone(),
        });
        if let Some(rproof) = prove(rgoal) {
            return Some(Proof {
                rule: Rule::OrInjR,
                premises: vec![rproof],
                conclusion: goal,
            });
        }
    }

    for (ix, assump) in goal.antecedent.iter().enumerate() {
        match assump.as_ref() {
            // try proving the goal in terms of an or in the environment
            Type::Or { left, right } => {
                let mut lassumps = Vec::with_capacity(goal.antecedent.len());
                lassumps.push(left.clone());
                lassumps.extend_from_slice(&goal.antecedent[0..ix]);
                lassumps.extend_from_slice(&goal.antecedent[ix..]);
                let lproof = prove(Rc::new(Sequent {
                    antecedent: lassumps,
                    consequent: goal.consequent.clone(),
                }));
                if lproof.is_none() {
                    continue;
                }

                let mut rassumps = Vec::with_capacity(goal.antecedent.len());
                rassumps.push(right.clone());
                rassumps.extend_from_slice(&goal.antecedent[0..ix]);
                rassumps.extend_from_slice(&goal.antecedent[ix..]);
                let rproof = prove(Rc::new(Sequent {
                    antecedent: rassumps,
                    consequent: goal.consequent.clone(),
                }));
                if rproof.is_none() {
                    continue;
                }

                return Some(Proof {
                    rule: Rule::OrL,
                    premises: vec![lproof.unwrap(), rproof.unwrap()],
                    conclusion: goal,
                });
            }

            _ => (),
        }
    }

    None
}