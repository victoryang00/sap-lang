use nom::{
    branch::alt, bytes::complete::tag, character::complete::one_of, combinator::map, IResult,
};
use nom_locate::LocatedSpan;
use std::string::String;

use crate::parser::Parser;

// pub mod array;
pub mod char;
pub mod number;
pub mod string;
// pub mod struct_;

pub fn escape_code(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, char> {
    one_of(r#"\"'nabfnrtvzx01234567{}"#)(s)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Integer(i128),
    Floating(f64),
}
impl std::fmt::Display for Number {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Number::Integer(i) => write!(f, "{}", i),
            Number::Floating(i) => write!(f, "{}", i),
        }
    }
}

impl Number {
    pub fn int(self) -> i128 {
        match self {
            Number::Integer(r) => r,
            Number::Floating(_) => panic!("Unknown Error"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Literal {
    Null,
    Bool(bool),
    // Struct(Struct),
    // Array(Vec<Literal>),
    String(String),
    Number(Number),
    // Default,
}
impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Null => write!(f, "\x1b[1;33mnull\x1b[0m"),
            Literal::Bool(b) => write!(f, "\x1b[1;33m{}\x1b[0m", b),
            Literal::String(s) => write!(f, "\x1b[1;34m{:?}\x1b[0m", s),
            Literal::Number(n) => write!(f, "\x1b[1;33m{}\x1b[0m", n),
        }
    }
}

impl Parser for Literal {
    fn parse(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Self> {
        alt((
            // map(struct_::struct_, |s| Literal::Struct(s)),
            // map(array::array, |s| Literal::Array(s)),
            map(number::number, |s| Literal::Number(s)),
            map(string::string, |s| Literal::String(s)),
            //char
            //byte
            map(alt((tag("True"), tag("False"))), |s: LocatedSpan<&str>| {
                Literal::Bool(s.fragment() == &"True")
            }), // map(tag("Default"), |_| Literal::Default),
            map(tag("null"), |_| Literal::Null),
        ))(s)
    }
}
