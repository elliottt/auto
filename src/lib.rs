#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub struct Expr(u32);

#[derive(Clone, Debug)]
pub struct Env {
    exprs: Vec<ExprData>,
}

impl std::ops::Index<Expr> for Env {
    type Output = ExprData;

    fn index(&self, index: Expr) -> &Self::Output {
        &self.exprs[index.0 as usize]
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            exprs: vec![ExprData::Bottom],
        }
    }

    fn push(&mut self, e: ExprData) -> Expr {
        let ix = self.exprs.len() as u32;
        self.exprs.push(e);
        Expr(ix)
    }

    pub fn bottom(&self) -> Expr {
        Expr(0)
    }

    pub fn atom(&mut self, name: impl ToString) -> Expr {
        self.push(ExprData::Atom {
            name: name.to_string(),
        })
    }

    pub fn imp(&mut self, left: Expr, right: Expr) -> Expr {
        self.push(ExprData::Imp { left, right })
    }

    pub fn app(&mut self, fun: Expr, args: impl IntoIterator<Item = Expr>) -> Expr {
        let args = Vec::from_iter(args);
        if args.is_empty() {
            fun
        } else {
            self.push(ExprData::App { fun, args })
        }
    }

    pub fn and(&mut self, args: impl IntoIterator<Item = Expr>) -> Expr {
        let args = Vec::from_iter(args);
        match args.len() {
            0 => panic!("and requires at least one argument"),
            1 => args[0],
            _ => self.push(ExprData::And { args }),
        }
    }

    pub fn or(&mut self, args: impl IntoIterator<Item = Expr>) -> Expr {
        let cases = Vec::from_iter(args);
        match cases.len() {
            0 => self.bottom(),
            1 => cases[0],
            _ => self.push(ExprData::Or { cases }),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprData {
    Atom { name: String },

    Imp { left: Expr, right: Expr },

    App { fun: Expr, args: Vec<Expr> },

    And { args: Vec<Expr> },

    Or { cases: Vec<Expr> },

    Bottom,
}

impl ExprData {
    pub fn is_atom(&self) -> bool {
        matches!(self, ExprData::Atom { .. })
    }
}

#[derive(Clone, Debug)]
pub struct Sequent {
    pub antecedent: Vec<Expr>,
    pub consequent: Expr,
}

impl Sequent {
    pub fn new(antecedent: impl IntoIterator<Item = Expr>, consequent: Expr) -> Self {
        Self {
            antecedent: Vec::from_iter(antecedent),
            consequent,
        }
    }

    pub fn assumes(&self, expr: Expr) -> bool {
        self.antecedent.iter().find(|&&e| e == expr).is_some()
    }
}

#[derive(Clone, Debug)]
pub enum Rule {
    Axiom { expr: Expr },
    ExFalso,
    ImpR,
    AndL,
    AndR,
    OrInj { case: usize },
    OrL,
    ImpVarL,
    ImpAndL,
    ImpOrL,
    ImpImpL,
}

#[derive(Clone, Debug)]
pub struct Subgoal {
    rule: Rule,
    goals: Vec<Sequent>,
}

fn without_assump(antecedent: &[Expr], ix: usize) -> Vec<Expr> {
    let mut assumps = Vec::new();
    assumps.extend(&antecedent[0..ix]);
    assumps.extend(&antecedent[ix + 1..]);
    assumps
}

/// Attempt to apply rules that will never fail to reduce this goal.
fn try_simple(env: &mut Env, goal: &Sequent) -> Option<Subgoal> {
    match &env[goal.consequent] {
        // ---------- AXIOM
        // A , Г => A
        ExprData::Atom { .. } if goal.assumes(goal.consequent) => {
            return Some(Subgoal {
                rule: Rule::Axiom {
                    expr: goal.consequent,
                },
                goals: Vec::new(),
            });
        }

        // A, Г => B
        // ----------- IMP_R
        // Г => A -> B
        ExprData::Imp { left, right } => {
            return Some(Subgoal {
                rule: Rule::ImpR,
                goals: vec![Sequent::new(
                    goal.antecedent
                        .iter()
                        .copied()
                        .chain(std::iter::once(*left)),
                    *right,
                )],
            });
        }

        // Г => A  Г => B
        // -------------- AND_R
        //   Г => A ∧ B
        ExprData::And { args } => {
            return Some(Subgoal {
                rule: Rule::AndR,
                goals: args
                    .iter()
                    .map(|&arg| Sequent::new(goal.antecedent.clone(), arg))
                    .collect(),
            });
        }

        _ => {}
    }

    for (ix, &assump) in goal.antecedent.iter().enumerate() {
        match &env[assump] {
            // ---------- EX_FALSO
            // ⊥ , Г => G
            ExprData::Bottom => {
                return Some(Subgoal {
                    rule: Rule::ExFalso,
                    goals: vec![],
                });
            }

            // A, B, Г => G
            // ------------- AND_L
            // A ∧ B, Г => G
            ExprData::And { args } => {
                let mut antecedent = without_assump(&goal.antecedent, ix);
                antecedent.extend(args);

                return Some(Subgoal {
                    rule: Rule::AndL,
                    goals: vec![Sequent::new(antecedent, goal.consequent)],
                });
            }

            &ExprData::Imp { left, right } => match &env[left] {
                // B, A, Г => G
                // ----------------
                // A → B, A, Г => G
                //
                // NOTE: this doesn't need to consume the argument as removing the implication from
                // the antecedent is enough to ensure that a loop doesn't occur.
                ExprData::Atom { .. } => {
                    if goal.assumes(left) {
                        let mut antecedent = without_assump(&goal.antecedent, ix);
                        antecedent.push(right);
                        return Some(Subgoal {
                            rule: Rule::ImpVarL,
                            goals: vec![Sequent::new(antecedent, goal.consequent)],
                        });
                    }
                }

                // A → (B → C) Г => G
                // ------------------- IMP_AND_L
                // (A ∧ B) → B, Г => G
                ExprData::And { args } => {
                    let mut imp = right;

                    for arg in args.clone().into_iter().rev() {
                        imp = env.imp(arg, imp);
                    }

                    let mut antecedent = without_assump(&goal.antecedent, ix);
                    antecedent.push(imp);

                    return Some(Subgoal {
                        rule: Rule::ImpAndL,
                        goals: vec![Sequent::new(antecedent, goal.consequent)],
                    });
                }

                // A → C, B → C, Г => G
                // -------------------- IMP_OR_L
                // (A ∨ B) → C, Г => G
                ExprData::Or { cases } => {
                    let mut antecedent = without_assump(&goal.antecedent, ix);
                    for case in cases.clone() {
                        antecedent.push(env.imp(case, right));
                    }

                    return Some(Subgoal {
                        rule: Rule::ImpOrL,
                        goals: vec![Sequent::new(antecedent, goal.consequent)],
                    });
                }

                // B → C, Г => A → B  C, Г => G
                // ---------------------------- IMP_IMP_L
                //      (A → B) → C, Г => G
                &ExprData::Imp { right: b, .. } => {
                    let mut lantecedent = without_assump(&goal.antecedent, ix);
                    let mut rantecedent = lantecedent.clone();

                    lantecedent.push(env.imp(b, right));
                    rantecedent.push(right);

                    return Some(Subgoal {
                        rule: Rule::ImpImpL,

                        goals: vec![
                            Sequent::new(lantecedent, left),
                            Sequent::new(rantecedent, goal.consequent),
                        ],
                    });
                }

                _ => {}
            },

            _ => {}
        }
    }

    None
}

#[derive(Clone, Debug)]
pub struct Proof {
    pub rule: Rule,
    pub assumptions: Vec<Proof>,
    pub conclusion: Sequent,
}

pub fn prove(env: &mut Env, goal: Sequent) -> Option<Proof> {
    // Try to discharge the goal with rules that don't require backtracking.
    if let Some(subgoal) = try_simple(env, &goal) {
        let mut assumptions = Vec::with_capacity(subgoal.goals.len());

        for goal in subgoal.goals {
            if let Some(proof) = prove(env, goal) {
                assumptions.push(proof);
            } else {
                return None;
            }
        }

        return Some(Proof {
            rule: subgoal.rule,
            assumptions,
            conclusion: goal,
        });
    }

    // Attempt to discharge a goal introduced by disjunction.
    if let ExprData::Or { cases } = &env[goal.consequent] {
        for (ix, case) in cases.clone().into_iter().enumerate() {
            let case = Sequent::new(goal.antecedent.clone(), case);
            if let Some(proof) = prove(env, case) {
                return Some(Proof {
                    rule: Rule::OrInj { case: ix },
                    assumptions: vec![proof],
                    conclusion: goal,
                });
            }
        }
    }

    // Attempt to discharge the goal by using a disjunction in the environment.
    for (ix, assump) in goal.antecedent.iter().enumerate() {
        if let ExprData::Or { cases } = &env[*assump] {
            let antecedent = without_assump(&goal.antecedent, ix);

            let mut assumptions = Vec::with_capacity(cases.len());

            for case in cases.clone() {
                let goal = Sequent::new(
                    antecedent.iter().copied().chain(std::iter::once(case)),
                    goal.consequent,
                );

                if let Some(proof) = prove(env, goal) {
                    assumptions.push(proof);
                } else {
                    // If we can't prove all cases of the disjunction, we can't use it to prove the
                    // goal.
                    continue;
                }
            }

            return Some(Proof {
                rule: Rule::OrL,
                assumptions,
                conclusion: goal,
            });
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use super::{prove, Env, Proof, Rule, Sequent};

    #[test]
    fn test_ex_falso() {
        let mut env = Env::new();
        let goal = Sequent::new([env.bottom()], env.atom("A"));
        assert!(matches!(
            prove(&mut env, goal),
            Some(Proof {
                rule: Rule::ExFalso,
                assumptions,
                ..
            }) if assumptions.is_empty()
        ));
    }

    #[test]
    fn test_axiom() {
        let mut env = Env::new();
        let a = env.atom("A");
        let goal = Sequent::new([a], a);
        assert!(matches!(
            prove(&mut env, goal),
            Some(Proof {
                rule: Rule::Axiom {..},
                assumptions,
                ..
            }) if assumptions.is_empty()
        ));
    }

    #[test]
    fn test_disjunction_goal() {
        let mut env = Env::new();
        let a = env.atom("A");
        let b = env.atom("B");
        let goal = Sequent::new([a], env.or([a, b]));

        let assumptions = match prove(&mut env, goal) {
            Some(Proof {
                rule: Rule::OrInj { case: 0 },
                assumptions,
                ..
            }) => assumptions,

            _ => panic!("failed to match disjunction"),
        };

        assert_eq!(assumptions.len(), 1);

        assert!(matches!(
            &assumptions[0],
            Proof {
                rule: Rule::Axiom { .. },
                ..
            }
        ));
    }

    #[test]
    fn test_disjunction_argument() {
        let mut env = Env::new();
        let a = env.atom("A");
        let goal = Sequent::new([env.or([a, a])], a);

        let assumptions = match prove(&mut env, goal) {
            Some(Proof {
                rule: Rule::OrL,
                assumptions,
                ..
            }) => assumptions,

            _ => panic!("failed to match disjunction"),
        };

        assert_eq!(assumptions.len(), 2);

        assert!(matches!(
            &assumptions[0],
            Proof {
                rule: Rule::Axiom { expr },
                assumptions,
                ..
            } if assumptions.is_empty() && *expr == a
        ));
        assert!(matches!(
            &assumptions[1],
            Proof {
                rule: Rule::Axiom { expr },
                assumptions,
                ..
            } if assumptions.is_empty() && *expr == a
        ));
    }
}
