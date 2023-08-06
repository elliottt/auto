
use lalrpop_util::lalrpop_mod;

mod ast;
mod strings;

lalrpop_mod!(pub grammar);

fn main() {
    let decl = grammar::DeclParser::new()
        .parse("data foo")
        .unwrap();

    println!("decl: {:?}", decl);
}
