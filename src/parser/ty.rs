use std::collections::BTreeMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::separated_list1,
    sequence::tuple,
    IResult,
};
use nom_locate::LocatedSpan;
use std::{boxed::Box, string::String, vec::Vec};

use crate::{
    parser::expr::ident,
    utils::{list0, ws},
};

#[derive(Debug, Clone)]
pub enum Type {
    Pair(String, Box<Type>),
    Any,
    // AnyArgsList,
    Number,
    String,
    Bool,
    Function(Vec<Type>, Box<Type>),
    Array,
    Object(BTreeMap<String, Type>),
    Enum(Vec<Type>),
    Alias(Vec<String>),
}
impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Pair(s, t) => write!(
                f,
                "\x1b[1;35m(\x1b[0m\x1b[1;32m\"{}\"\x1b[0m: {}\x1b[1;35m)\x1b[0m",
                s, t
            ),
            Type::Any => write!(f, "\x1b[1;32many\x1b[0m"),
            Type::Number => write!(f, "\x1b[1;32mnumber\x1b[0m"),
            Type::String => write!(f, "\x1b[1;32mstring\x1b[0m"),
            Type::Bool => write!(f, "\x1b[1;32mbool\x1b[0m"),
            Type::Function(ps, r) => {
                write!(f, "\x1b[1;35m(\x1b[0m")?;
                for (n, s) in ps.iter().enumerate() {
                    if n > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", s)?;
                }
                write!(f, "\x1b[1;35m)\x1b[0m")?;
                write!(f, " \x1b[1;35m->\x1b[0m {}", r)
            }
            Type::Array => write!(f, "\x1b[1;32marray\x1b[0m"),
            Type::Object(o) => {
                write!(f, "\x1b[1;35m{{\x1b[0m")?;
                for (n, s) in o.iter().enumerate() {
                    if n > 0 {
                        write!(f, ", ")?;
                    }
                    let (sss, ssss) = s;
                    write!(f, "\x1b[1;32m{}\x1b[0m: {}", sss, ssss)?;
                }
                write!(f, "\x1b[1;35m}}\x1b[0m")
            }
            Type::Enum(_) => todo!(),
            Type::Alias(a) => {
                for (n, s) in a.iter().enumerate() {
                    if n > 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "\x1b[0;36m{}\x1b[0m", s)?;
                }
                Ok(())
            }
        }
    }
}

pub fn parse_function_type(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Type> {
    let (l, (v, _, _, _, t)) = tuple((
        list0("(", ")", ",", parse_type),
        opt(ws),
        tag("->"),
        opt(ws),
        parse_type,
    ))(s)?;
    Ok((l, Type::Function(v, Box::new(t))))
}

pub fn parse_type(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Type> {
    alt((
        map(separated_list1(tag("::"), ident), |i| Type::Alias(i)),
        map(tag("any"), |_| Type::Any),
        map(tag("number"), |_| Type::Number),
        map(tag("string"), |_| Type::String),
        map(tag("bool"), |_| Type::Bool),
        map(tag("array"), |_| Type::Array),
        map(tag("object"), |_| Type::Object(BTreeMap::new())),
        parse_function_type,
    ))(s)
}
