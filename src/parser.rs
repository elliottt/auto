use pest::{iterators::Pair, Parser};
use std::rc::Rc;

use crate::types::{Data, Type};

#[derive(Parser)]
#[grammar = "type.pest"]
pub struct TypeParser;

pub fn parse_type(input: &str) -> Rc<Type> {
    let res = TypeParser::parse(Rule::type_, input)
        .expect("Failed to parse type")
        .next()
        .unwrap();

    fn parse_value(pair: Pair<Rule>) -> Rc<Type> {
        match pair.as_rule() {
            Rule::type_ => return parse_value(pair.into_inner().nth(0).unwrap()),

            Rule::iexp => {
                let mut pairs = pair.into_inner();
                let lhs = parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Type::imp(lhs, parse_value(rhs));
                } else {
                    return lhs;
                }
            }

            Rule::aexp => {
                let mut pairs = pair.into_inner();
                let lhs = parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Type::and([lhs, parse_value(rhs)]);
                } else {
                    return lhs;
                }
            }

            Rule::oexp => {
                let mut pairs = pair.into_inner();
                let lhs = parse_value(pairs.next().unwrap());
                if let Some(rhs) = pairs.next() {
                    return Type::or([lhs, parse_value(rhs)]);
                } else {
                    return lhs;
                }
            }

            Rule::app => {
                let mut iter = pair.into_inner();
                let fun = parse_value(iter.next().unwrap());
                return Type::app(fun, iter.map(parse_value));
            }

            Rule::atomic => {
                let mut pairs = pair.into_inner();
                return parse_value(pairs.next().unwrap());
            }

            Rule::atom => return Type::atom(pair.as_str()),

            // TODO: bad?
            Rule::var => return Type::atom(pair.as_str()),

            Rule::bottom => return Type::bottom(),

            _ => {
                println!("UNHANDLED: {:?}", pair);
                return Type::bottom();
            }
        }
    }

    parse_value(res)
}

pub fn parse_data(input: &str) -> Data {
    let res = TypeParser::parse(Rule::data, input)
        .expect("Failed to parse data declaration")
        .next()
        .unwrap();

    let mut data = Data::default();

    for node in res.into_inner() {
        match node.as_rule() {
            Rule::atom => data.name.push_str(node.as_str()),

            Rule::vars => {
                for var in node.into_inner() {
                    data.vars.push(String::from(var.as_str()))
                }
            }

            Rule::constrs => {
                let res_ty = data.as_type();

                for constr in node.into_inner() {
                    let mut iter = constr.into_inner();

                    let name = String::from(iter.next().unwrap().as_str());
                    let mut res = Rc::clone(&res_ty);

                    if let Some(constrs) = iter.next() {
                        for ty in constrs.into_inner().rev() {
                            res = Type::imp(parse_type(ty.as_str()), res);
                        }
                    }

                    data.constrs.push((name, res));
                }
            }

            _ => break,
        }
    }

    data
}
