use crate::parser::expr::literal::string::string;
use crate::parser::expr::parse_closure;
use crate::parser::expr::parse_expr;
use crate::utils::list1;
use nom::branch::alt;
use nom::bytes::complete::take_until;
use nom::bytes::complete::take_while;
use nom::multi::many1;
use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    multi::separated_list1,
    sequence::tuple,
    IResult,
};
use nom_locate::LocatedSpan;
use std::collections::BTreeMap;
use std::str::FromStr;

use crate::{
    parser::expr::ident,
    utils::{list0, ws},
};

use self::expr::literal::string::raw_string;
use self::expr::CommentedExpr;
use self::{expr::Expr, ty::Type};

pub mod expr;

pub mod ty;
struct TypedExpr {
    expr: Expr,
    ty: Type,
}

#[derive(Debug)]
pub enum TopLevel {
    Comment(String),
    TypeDef(String, CommentedExpr),
    EnumDef(String, Vec<(String, CommentedExpr)>),
    Import(Vec<String>, String),
    Expr(CommentedExpr),
}

fn parse_type_def(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, TopLevel> {
    map(
        tuple((
            tag("type"),
            ws,
            ident,
            opt(ws),
            tag("="),
            opt(ws),
            parse_expr,
        )),
        |(_, _, i, _, _, _, c)| TopLevel::TypeDef(i, c),
    )(s)
}
fn parse_enum_body(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, Vec<(String, CommentedExpr)>> {
    map(
        tuple((
            tag("|"),
            opt(ws),
            separated_list1(
                tuple((opt(ws), tag("|"), ws)),
                map(
                    tuple((ident, opt(ws), tag(":"), opt(ws), parse_expr)),
                    |(i, _, _, _, c)| (i, c),
                ),
            ),
        )),
        |(_, _, s)| s,
    )(s)
}

fn parse_enum_def(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, TopLevel> {
    map(
        tuple((tag("type"), ws, ident, opt(ws), parse_enum_body)),
        |(_, _, i, _, b)| TopLevel::EnumDef(i, b),
    )(s)
}

fn parse_import(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, TopLevel> {
    map(
        tuple((
            tag("import"),
            ws,
            list1("(", ")", ",", ident),
            ws,
            tag("from"),
            ws,
            string,
        )),
        |(_, _, v, _, _, _, s)| TopLevel::Import(v, s),
    )(s)
}
pub fn parse_top_level(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, TopLevel> {
    alt((
        map(parse_comments, |c| TopLevel::Comment(c)),
        parse_type_def,
        parse_enum_def,
        parse_import,
        map(parse_expr, |e| TopLevel::Expr(e)),
    ))(s)
}

pub fn doc(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, LocatedSpan<&str>> {
    map(tuple((tag("##DOC"), take_while(|s| s != '\n'))), |(_, s)| s)(s)
}

pub fn parse_comment(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (s, _) = opt(ws)(s)?;
    let _ = tag("#")(s)?;
    if let Ok((_, _)) = doc(s) {
        Err(nom::Err::Error(nom::error::Error::new(
            s,
            nom::error::ErrorKind::IsNot,
        )))
    } else {
        map(
            tuple((tag("#"), take_while(|s| s != '\n'), opt(ws))),
            |(_, c, _)| c.fragment().to_string(),
        )(s)
    }
}

pub fn parse_comments(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    map(many1(parse_comment), |v| {
        let mut s = String::new();
        for i in v {
            s += &i;
        }
        s
    })(s)
}

#[test]
fn test_parse_comment() {
    let commnets = [
        "    # abc\"@aaaa\t\n\t# bcd\n#efg",
        "#abc\nabc",
        "##DOC abcabcabc",
    ];
    for c in commnets {
        println!("{:?}", parse_comments(LocatedSpan::new(c)));
    }
}
