
use std::rc::Rc;
use pest::{ Parser, iterators::Pair };

use crate::types::Type;

#[derive(Parser)]
#[grammar = "type.pest"]
pub struct TypeParser;

pub fn parse_type(input: &str) -> Rc<Type> {
    let res = TypeParser::parse(Rule::type_, input)
        .expect("Failed to parse type")
        .next().unwrap();

    parse_value(res)
}

fn parse_value(pair: Pair<Rule>) -> Rc<Type> {
    match pair.as_rule() {
        Rule::type_ => {
            return parse_value(pair.into_inner().nth(0).unwrap())
        }

        Rule::iexp => {
            let mut pairs = pair.into_inner();
            let lhs = parse_value(pairs.next().unwrap());
            if let Some(rhs) = pairs.next() {
                return Rc::new(Type::imp(lhs, parse_value(rhs)))
            } else {
                return lhs
            }
        }

        Rule::aexp => {
            let mut pairs = pair.into_inner();
            let lhs = parse_value(pairs.next().unwrap());
            if let Some(rhs) = pairs.next() {
                return Rc::new(Type::and(lhs, parse_value(rhs)))
            } else {
                return lhs
            }
        }

        Rule::oexp => {
            let mut pairs = pair.into_inner();
            let lhs = parse_value(pairs.next().unwrap());
            if let Some(rhs) = pairs.next() {
                return Rc::new(Type::or(lhs, parse_value(rhs)))
            } else {
                return lhs
            }
        }

        Rule::atomic => {
            let mut pairs = pair.into_inner();
            return parse_value(pairs.next().unwrap())
        }

        Rule::atom => {
            return Rc::new(Type::var(pair.as_str()))
        }

        Rule::bottom => {
            return Rc::new(Type::Bottom)
        }

        _ => {
            println!("UNHANDLED: {:?}", pair);
            return Rc::new(Type::Bottom)
        }
    }
}
