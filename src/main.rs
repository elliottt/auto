use std::rc::Rc;

#[derive(Debug, Hash, PartialEq, Eq)]
enum Type {
    Var { name: String },

    Imp { left: Rc<Type>, right: Rc<Type> },

    And { left: Rc<Type>, right: Rc<Type> },

    Or { left: Rc<Type>, right: Rc<Type> },

    Bottom,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Var { .. } => 1,
            Type::Imp { left, right } => left.size() + right.size() + 1,
            Type::And { left, right } => left.size() + right.size() + 2,
            Type::Or { left, right } => left.size() + right.size() + 1,
            Type::Bottom => 1,
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
}

#[derive(Debug, Clone, Hash, PartialEq)]
struct Sequent {
    antecedent: Vec<Rc<Type>>,
    consequent: Rc<Type>,
}

impl Sequent {
    pub fn from_type(consequent: Rc<Type>) -> Self {
        Sequent {
            antecedent: Vec::new(),
            consequent,
        }
    }

    pub fn has_assumption(&self, ty: &Type) -> bool {
        self.antecedent
            .iter()
            .find(|other| other.as_ref() == ty)
            .is_some()
    }

    pub fn assume(mut self, ty: Rc<Type>) -> Self {
        self.antecedent.push(ty);
        self
    }

    /// true when Type::Bottom shows in the antecedent of the sequent.
    pub fn false_assumption(&self) -> bool {
        self.antecedent
            .iter()
            .find(|ty| match ty.as_ref() {
                Type::Bottom => true,
                _ => false,
            })
            .is_some()
    }
}

/*
#[derive(Debug, Hash)]
struct Inference {
    premises: Vec<Rc<Sequent>>,
    conclusion: Rc<Sequent>,
}

impl Inference {

    /// ```
    /// Г => A
    /// -----------
    /// Г => A ∨ B
    /// ```
    pub fn or_l(s: Rc<Sequent>) -> Option<Self> {
        if let Some((left, right)) = s.antecedent.iter().find_map(|ty| match ty.as_ref() {
            Type::Or { left, right } => Some((left, right)),
            _ => None,
        }) {
            let mut l = s.as_ref().clone();
            l.antecedent.push(left.clone());
            let mut r = s.as_ref().clone();
            r.antecedent.push(right.clone());
            Some(Inference {
                premises: vec![Rc::new(l), Rc::new(r)],
                conclusion: s,
            })
        } else {
            None
        }
    }

    /// ```
    /// Г => A
    /// -----------
    /// Г => A ∨ B
    /// ```
    pub fn or_r_l(s: Rc<Sequent>) -> Option<Self> {
        match s.consequent.as_ref() {
            Type::Or { left, .. } => Some(Inference {
                premises: vec![Rc::new(Sequent {
                    antecedent: s.antecedent.clone(),
                    consequent: left.clone(),
                })],
                conclusion: s,
            }),

            _ => None,
        }
    }

    /// ```
    /// Г => B
    /// -----------
    /// Г => A ∨ B
    /// ```
    pub fn or_r_r(s: Rc<Sequent>) -> Option<Self> {
        match s.consequent.as_ref() {
            Type::Or { right, .. } => Some(Inference {
                premises: vec![Rc::new(Sequent {
                    antecedent: s.antecedent.clone(),
                    consequent: right.clone(),
                })],
                conclusion: s,
            }),

            _ => None,
        }
    }
}

*/

#[derive(Debug)]
enum Rule {
    Axiom,
    ExFalso,
    ImpR,
    AndL,
    AndR,
}

#[derive(Debug)]
struct Subgoal {
    rule: Rule,
    goals: Vec<Rc<Sequent>>,
}

// These are steps that are always worth taking, and don't require back-tracking.
fn try_simple(goal: Rc<Sequent>) -> Option<Subgoal> {

    match goal.consequent.as_ref() {
        // ```
        // ----------
        // A , Г => A
        // ```
        _ if goal.has_assumption(goal.consequent.as_ref()) =>
            return Some(Subgoal {
            rule: Rule::Axiom,
            goals: Vec::new(),
        }),

        // ```
        // A, Г => B
        // -----------
        // Г => A -> B
        // ```
        Type::Imp { left, right } => {
            let mut antecedent = goal.antecedent.clone();
            antecedent.push(left.clone());
            return Some(Subgoal {
                rule: Rule::ImpR,
                goals: vec![Rc::new(Sequent {
                    antecedent,
                    consequent: right.clone(),
                })],
            })
        }

        // ```
        // Г => A  Г => B
        // --------------
        //   Г => A ∧ B
        // ```
        Type::And { left, right } =>
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
            }),

        _ => (),
    }

    for assump in goal.antecedent.iter() {
        match assump.as_ref() {
            // ```
            // ----------
            // ⊥ , Г => G
            // ```
            Type::Bottom =>
            return Some(Subgoal {
                rule: Rule::ExFalso,
                goals: Vec::new(),
            }),

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
                antecedent.extend_from_slice(&goal.antecedent[1..]);
                return Some(Subgoal {
                    rule: Rule::AndL,
                    goals: vec![Rc::new(Sequent{ antecedent, consequent: goal.consequent.clone() })],
                })
            }

            _ => (),
        }
    }

    None
}

#[derive(Debug)]
struct Proof {
    rule: Rule,
    premises: Vec<Proof>,
    conclusion: Rc<Sequent>,
}

fn prove(goal: Rc<Sequent>) -> Option<Proof> {
    if let Some(subgoal) = try_simple(goal.clone()) {
        let mut premises = Vec::with_capacity(subgoal.goals.len());
        for goal in subgoal.goals {
            if let Some(proof) = prove(goal) {
                premises.push(proof);
            } else {
                return None;
            }
        }
        return Some(Proof{
            rule: subgoal.rule,
            premises,
            conclusion: goal,
        })
    }

    // try remaining strategies in sequence

    None
}

fn main() {
    let var_a = Rc::new(Type::var("a"));
    let var_b = Rc::new(Type::var("b"));
    let and = Rc::new(Type::and(var_a.clone(), var_b));
    let goal = Rc::new(Sequent::from_type(Rc::new(Type::imp(and, var_a))));


    if let Some(proof) = prove(goal) {
        println!("{:?}", proof);
    }
}
