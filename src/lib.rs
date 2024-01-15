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

    pub fn atom(&mut self, name: String) -> Expr {
        self.push(ExprData::Atom { name })
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
    OrInj,
    OrL,
    ImpVarL,
    ImpAndL,
    ImpOrL,
    ImpImpL,
}

#[derive(Clone, Debug)]
pub struct Proof {
    pub rule: Rule,
    pub assumptions: Vec<Proof>,
    pub conclusion: Sequent,
}

#[derive(Clone, Debug)]
pub struct Subgoal {
    rule: Rule,
    goals: Vec<Sequent>,
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

    fn without_assump(antecedent: &[Expr], ix: usize) -> Vec<Expr> {
        let mut assumps = Vec::new();
        assumps.extend(&antecedent[0..ix]);
        assumps.extend(&antecedent[ix + 1..]);
        assumps
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
