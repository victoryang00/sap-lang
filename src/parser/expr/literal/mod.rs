use alloc::string::String;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::one_of,
    combinator::map,
    IResult,
};
use nom_locate::LocatedSpan;

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

pub fn literal(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Literal> {
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
