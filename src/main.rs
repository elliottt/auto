extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::rc::Rc;

mod parser;
mod pretty;
mod prove;
mod terms;
mod types;

use prove::{prove, Sequent};
use terms::Term;
use types::Type;

fn main() {
    let var_a = Rc::new(Type::var("a"));
    let var_b = Rc::new(Type::var("b"));
    let or = Rc::new(Type::or(var_a.clone(), var_b.clone()));
    let goal = Rc::new(Sequent::from_type(Rc::new(Type::imp(var_b, or))));

    println!("goal:  {}", goal);
    if let Some(proof) = prove(goal) {
        println!("proof: {}", Term::from_proof(&proof));
    }
}
