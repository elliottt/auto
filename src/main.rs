extern crate pest;
extern crate repl_framework;
#[macro_use]
extern crate pest_derive;

use repl_framework::Repl;
use std::collections::HashMap;
use std::rc::Rc;

mod parser;
mod pretty;
mod prove;
mod terms;
mod types;

use prove::{prove, Sequent};
use terms::Term;
use types::Data;

#[derive(Default)]
struct Env {
    datas: HashMap<String, Data>,
}

fn derive_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");
    match parser::parse_type(&ty_str) {
        Ok(ty) => {
            if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
                let term = Term::from_proof(&proof);
                println!("{}", term);
            } else {
                println!("Unable to prove: {}", ty);
            }
        }

        Err(err) => {
            println!("{}", err);
        }
    }
}

fn derive_simplify_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");

    match parser::parse_type(&ty_str) {
        Ok(ty) => {
            if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
                let term = Term::from_proof(&proof);
                println!("{}", Term::simplify(term));
            } else {
                println!("Unable to prove: {}", ty);
            }
        }

        Err(err) => {
            println!("{}", err);
        }
    }
}

fn prove_cmd(_env: &mut Env, args: Vec<String>) {
    let ty_str: String = args.join(" ");
    match parser::parse_type(&ty_str) {
        Ok(ty) => {
            if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
                println!("{}", proof);
            } else {
                println!("Unable to prove: {}", ty);
            }
        }

        Err(err) => {
            println!("{}", err);
        }
    }
}

fn data_cmd(env: &mut Env, args: Vec<String>) {
    let arg_str: String = args.join(" ");
    match parser::parse_data(&arg_str) {
        Ok(data) => {
            env.datas.insert(data.name.clone(), data);
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}

fn env_cmd(env: &mut Env, _args: Vec<String>) {
    for data in env.datas.values() {
        println!("{}", data);
    }
}

fn help_cmd(_env: &mut Env, _args: Vec<String>) {
    println!("help           Print this message");
    println!("? formula      Construct a term for this formula, if possible");
    println!("?! formula     Same as `?`, but simplify the result as well");
    println!("prove formula  Display the proof tree for the formula");
}

fn main() -> std::io::Result<()> {
    Repl::new(Env::default())
        .with_prompt("auto> ")
        .with_function("help", help_cmd)
        .with_function("?", derive_cmd)
        .with_function("?!", derive_simplify_cmd)
        .with_function("prove", prove_cmd)
        .with_function("data", data_cmd)
        .with_function("env", env_cmd)
        .run()
}
