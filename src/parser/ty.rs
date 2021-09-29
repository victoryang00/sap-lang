use std::collections::BTreeMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::tuple,
    IResult,
};

use crate::{
    parser::expr::ident,
    utils::{list0, ws},
};

#[derive(Debug, Clone)]
pub enum Type {
    Any,
    // AnyArgsList,
    Number,
    String,
    Bool,
    Function(Vec<Type>, Box<Type>),
    Array,
    Object(BTreeMap<String, Type>),
    Enum(Vec<Type>),
    Alias(String),
}

pub fn parse_type(s: &str) -> IResult<&str, Type> {
    alt((
        map(ident, |i| Type::Alias(i)),
        map(tag("any"), |_| Type::Any),
        map(tag("number"), |_| Type::Number),
        map(tag("string"), |_| Type::String),
        map(tag("bool"), |_| Type::Bool),
        map(tag("array"), |_| Type::Array),
        map(tag("object"), |_| Type::Object(BTreeMap::new())),
        map(
            tuple((
                list0("(", ")", ",", parse_type),
                opt(ws),
                tag("->"),
                opt(ws),
                parse_type,
            )),
            |(a, _, _, _, t)| Type::Function(a, Box::new(t)),
        ),
    ))(s)
}
