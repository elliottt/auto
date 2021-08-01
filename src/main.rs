extern crate pest;
extern crate repl_framework;
#[macro_use]
extern crate pest_derive;

use repl_framework::Repl;
use std::rc::Rc;

mod parser;
mod pretty;
mod prove;
mod terms;
mod types;

use prove::{prove, Sequent};
use terms::Term;

#[derive(Default)]
struct Env {}

fn derive_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        let term = Term::from_proof(&proof);
        println!("{}", term);
    } else {
        println!("Unable to prove: {}", ty);
    }
}

fn derive_simplify_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        let term = Term::from_proof(&proof);
        println!("{}", Term::simplify(term));
    } else {
        println!("Unable to prove: {}", ty);
    }
}

fn prove_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        println!("{}", proof);
    } else {
        println!("Unable to prove: {}", ty);
    }
}

fn data_cmd(_env: &mut Env, args: Vec<String>) {
    let arg_str: String = args.join(" ");
    println!("parsing: {:?}", arg_str);

    let data = parser::parse_data(&arg_str);
    println!("{:?}", data);
}

fn main() -> std::io::Result<()> {
    Repl::new(Env::default())
        .with_prompt("auto> ")
        .with_function("?", derive_cmd)
        .with_function("?!", derive_simplify_cmd)
        .with_function("prove", prove_cmd)
        .with_function("data", data_cmd)
        .run()
}
