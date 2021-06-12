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

#[derive(Debug, Clone, Hash)]
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

#[derive(Debug, Hash)]
struct Inference {
    premises: Vec<Rc<Sequent>>,
    conclusion: Rc<Sequent>,
}

impl Inference {
    /// ```
    /// ----------
    /// A , Г => A
    /// ```
    pub fn axiom(s: Rc<Sequent>) -> Option<Self> {
        match s.consequent.as_ref() {
            Type::Var { .. } if s.has_assumption(s.consequent.as_ref()) => Some(Inference {
                premises: Vec::new(),
                conclusion: s,
            }),
            _ => None,
        }
    }

    /// ```
    /// ----------
    /// ⊥ , Г => G
    /// ```
    pub fn ex_falso(s: Rc<Sequent>) -> Option<Self> {
        if s.false_assumption() {
            Some(Inference {
                premises: Vec::new(),
                conclusion: s,
            })
        } else {
            None
        }
    }

    /// ```
    /// A, B, Г => G
    /// -------------
    /// A ∧ B, Г => G
    /// ```
    pub fn and_l(s: Rc<Sequent>) -> Option<Self> {
        if let Some((left, right)) = s.antecedent.iter().find_map(|ty| match ty.as_ref() {
            Type::And { left, right } => Some((left, right)),
            _ => None,
        }) {
            let mut antecedent = Vec::with_capacity(2);
            antecedent.push(left.clone());
            antecedent.push(right.clone());
            let consequent = s.consequent.clone();
            Some(Inference {
                premises: vec![Rc::new(Sequent {
                    antecedent,
                    consequent,
                })],
                conclusion: s,
            })
        } else {
            None
        }
    }

    /// ```
    /// Г => A  Г => B
    /// --------------
    ///   Г => A ∧ B
    /// ```
    pub fn and_r(s: Rc<Sequent>) -> Option<Self> {
        match s.consequent.as_ref() {
            Type::And { left, right } => Some(Inference {
                premises: vec![
                    Rc::new(Sequent {
                        antecedent: s.antecedent.clone(),
                        consequent: left.clone(),
                    }),
                    Rc::new(Sequent {
                        antecedent: s.antecedent.clone(),
                        consequent: right.clone(),
                    }),
                ],
                conclusion: s,
            }),

            _ => None,
        }
    }

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

    /// ```
    /// A, Г => B
    /// -----------
    /// Г => A -> B
    /// ```
    pub fn imp_r(s: Rc<Sequent>) -> Option<Self> {
        match s.consequent.as_ref() {
            Type::Imp { left, right } => {
                let mut antecedent = s.antecedent.clone();
                antecedent.push(left.clone());
                Some(Inference {
                    premises: vec![Rc::new(Sequent {
                        antecedent,
                        consequent: right.clone(),
                    })],
                    conclusion: s,
                })
            }

            _ => None,
        }
    }
}

fn main() {
    let var = Rc::new(Type::var("a"));
    let ty = Rc::new(Type::imp(var.clone(), var));
    let seq = Rc::new(Sequent::from_type(ty));
    let inf = Inference::imp_r(seq);
    println!("{:?}", inf);
}
