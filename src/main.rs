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

fn derive<T>(args: HashMap<String, Value>, _: &mut T) -> Result<Option<String>> {
    let ty_str: String = args["type"].convert()?;
    let ty = parser::parse_type(&ty_str);

    if let Some(proof) = prove(Rc::new(Sequent::from_type(ty.clone()))) {
        println!("{}", Term::from_proof(&proof));
    } else {
        println!("Unable to prove: {}", ty);
    }

    Ok(None)
}

fn main() -> Result<()> {
    let mut repl = Repl::new(()).with_name("auto").add_command(
        Command::new("?", derive)
            .with_parameter(Parameter::new("type").set_required(true)?)?
            .with_help("Derive a term from a type"),
    );
    repl.run()
}
