use crate::parser::expr::literal::string::string;
use crate::parser::expr::parse_closure;
use crate::parser::expr::parse_expr;
use crate::utils::list1;
use nom::branch::alt;
use nom::{
    bytes::complete::tag,
    combinator::{map, opt},
    multi::separated_list1,
    sequence::tuple,
    IResult,
};
use nom_locate::LocatedSpan;
use std::collections::BTreeMap;

use crate::{
    parser::expr::ident,
    utils::{list0, ws},
};

use self::{expr::Expr, ty::Type};

pub mod expr;

pub mod ty;
struct TypedExpr {
    expr: Expr,
    ty: Type,
}

#[derive(Debug)]
pub enum TopLevel {
    TypeDef(String, Expr),
    EnumDef(String, Vec<(String, Expr)>),
    Import(Vec<String>, String),
    Expr(Expr),
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
fn parse_enum_body(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<(String, Expr)>> {
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
        parse_type_def,
        parse_enum_def,
        parse_import,
        map(parse_expr, |e| TopLevel::Expr(e)),
    ))(s)
}
