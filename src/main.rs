extern crate pest;
extern crate repl_rs;
#[macro_use]
extern crate pest_derive;

use repl_rs::{Command, Convert, Parameter, Repl, Result, Value};
use std::collections::HashMap;
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

fn derive_cmd(args: HashMap<String, Value>, _env: &mut Env) -> Result<Option<String>> {
    let ty_str: String = args["type"].convert()?;
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        let term = Term::from_proof(&proof);
        println!("{}", term);
    } else {
        println!("Unable to prove: {}", ty);
    }

    Ok(None)
}

fn derive_simplify_cmd(args: HashMap<String, Value>, _env: &mut Env) -> Result<Option<String>> {
    let ty_str: String = args["type"].convert()?;
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        let term = Term::from_proof(&proof);
        println!("{}", Term::simplify(term));
    } else {
        println!("Unable to prove: {}", ty);
    }

    Ok(None)
}

fn prove_cmd(args: HashMap<String, Value>, _env: &mut Env) -> Result<Option<String>> {
    let ty_str: String = args["type"].convert()?;
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        println!("{}", proof);
    } else {
        println!("Unable to prove: {}", ty);
    }

    Ok(None)
}

fn main() -> Result<()> {
    let mut repl = Repl::new(Env::default())
        .with_name("auto")
        .add_command(
            Command::new("?", derive_cmd)
                .with_parameter(Parameter::new("type").set_required(true)?)?
                .with_help("Derive a term from a type"),
        )
        .add_command(
            Command::new("?!", derive_simplify_cmd)
                .with_parameter(Parameter::new("type").set_required(true)?)?
                .with_help("Derive a term from a type, and simplify it"),
        )
        .add_command(
            Command::new("prove", prove_cmd)
                .with_parameter(Parameter::new("type").set_required(true)?)?
                .with_help("Try to derive a proof for the given term"),
        );
    repl.run()
}
