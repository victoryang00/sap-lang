use std::collections::BTreeMap;

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    multi::separated_list1,
    sequence::{delimited, pair, tuple},
    IResult,
};
use nom_locate::LocatedSpan;

use crate::utils::{list0, list1, valid_name, ws, ws_single_line};

use self::literal::{literal, string::string, Literal};

use super::ty::{parse_type, Type};

pub(crate) mod literal;

#[derive(Debug, Clone)]
pub enum Expr {
    Quoted(Box<Expr>),
    Block(Vec<Expr>),
    Literal(Literal),
    Ident(Vec<String>),
    Array(Vec<Expr>),
    Object(Vec<(String, Expr)>),
    Closure(Vec<(String, Option<Type>)>, Option<Type>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    MultiIf(Vec<(Expr, Expr)>, Box<Expr>),
    For(String, Box<Expr>, Box<Expr>),
    // ll
    Call(Box<Expr>, Vec<Expr>),
    ErrorHandle(Box<Expr>),
    Bind(Box<Expr>, Box<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Assign(Box<Expr>, Box<Expr>),
    SpecifyTyped(Box<Expr>, Type),
}

pub fn key_word(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, LocatedSpan<&str>> {
    alt((
        //
        tag("string"),
        tag("number"),
        tag("bool"),
        tag("array"),
        tag("object"),
        tag("function"),
        tag("any"),
        tag("type"),
        //
        tag("null"),
        //
        tag("if"),
        tag("else"),
        tag("for"),
        tag("in"),
        tag("while"),
        tag("ARGS"),
        tag("ENV"),
    ))(s)
}

pub fn ident(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (o, name) = valid_name(s)?;
    let k = key_word(name);
    match k {
        Ok((a, _)) => {
            if a.fragment() == &"" {
                Err(nom::Err::Error(nom::error::Error::new(
                    name,
                    nom::error::ErrorKind::Not,
                )))
            } else {
                Ok((o, name.fragment().to_string()))
            }
        }
        Err(_) => Ok((o, name.fragment().to_string())),
    }
}

fn parse_block(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<Expr>> {
    // list 1 expand
    list1("{", "}", ";", parse_expr)(s)
}

pub(crate) fn parse_closure(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Expr> {
    map(
        tuple((
            list0("(", ")", ",", |s| {
                map(tuple((ident, opt(ws), opt(type_specifier))), |(i, _, t)| {
                    (i, t)
                })(s)
            }),
            opt(map(
                tuple((opt(ws), tag("->"), opt(ws), parse_type)),
                |(_, _, _, t)| t,
            )),
            map(tuple((opt(ws), parse_block)), |(_, e)| Expr::Block(e)),
        )),
        |(a, t, e)| Expr::Closure(a, t, Box::new(e)),
    )(s)
}

fn parse_array(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<Expr>> {
    list0("[", "]", ",", parse_expr)(s)
}
fn parse_object(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<(String, Expr)>> {
    list0("{", "}", ",", |s| {
        alt((
            map(
                tuple((ident, opt(ws), tag(":"), opt(ws), parse_expr)),
                |(s, _, _, _, e)| (s.to_owned(), e),
            ),
            map(
                tuple((string, opt(ws), tag(":"), opt(ws), parse_expr)),
                |(s, _, _, _, e)| (s, e),
            ),
            map(separated_list1(tag("::"), ident), |s| {
                (s.last().unwrap().clone(), Expr::Ident(s))
            }),
        ))(s)
    })(s)
}
fn parse_if(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, (Expr, Expr, Expr)> {
    map(
        tuple((
            tag("if"),
            ws,
            parse_expr,
            opt(ws),
            parse_block,
            opt(ws),
            tuple((tag("else"), ws, parse_block)),
        )),
        |(_, _, e, _, b, _, (_, _, o))| (e, Expr::Block(b), Expr::Block(o)),
    )(s)
}

fn parse_multi_if_body(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<(Expr, Expr)>> {
    map(
        tuple((
            tag("|"),
            ws,
            separated_list1(
                tuple((opt(ws), tag("|"), ws)),
                map(tuple((parse_expr, opt(ws), parse_block)), |(a, _, b)| {
                    (a, Expr::Block(b))
                }),
            ),
        )),
        |(_, _, s)| s,
    )(s)
}

fn parse_for(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, (String, Box<Expr>, Box<Expr>)> {
    map(
        tuple((
            tag("for"),
            ws,
            ident,
            ws,
            tag("in"),
            ws,
            parse_expr,
            opt(ws),
            parse_block,
        )),
        |(_, _, i, _, _, _, e, _, b)| (i.to_owned(), Box::new(e), Box::new(Expr::Block(b))),
    )(s)
}

fn parse_multi_if(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, (Vec<(Expr, Expr)>, Expr)> {
    map(
        tuple((
            tag("if"),
            ws,
            parse_multi_if_body,
            opt(ws),
            tuple((tag("else"), ws, parse_block)),
        )),
        |(_, _, l, _, (_, _, e))| (l, Expr::Block(e)),
    )(s)
}

fn q(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, LocatedSpan<&str>> {
    tag("?")(s)
}
fn type_specifier(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Type> {
    map(tuple((tag(":"), opt(ws), parse_type)), |(_, _, ty)| ty)(s)
}
fn expr_ll(l: Expr, r: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Expr> {
    let (r, _) = opt(ws)(r)?;
    if let Ok((r, _)) = q(r) {
        expr_ll(Expr::ErrorHandle(Box::new(l)), r)
    } else if let Ok((r, c)) = list0("(", ")", ",", parse_expr)(r) {
        expr_ll(Expr::Call(Box::new(l), c), r)
    } else if let Ok((r, (_, _, e, _, _))) =
        tuple((tag("["), opt(ws), parse_expr, opt(ws), tag("]")))(r)
    {
        expr_ll(Expr::Index(Box::new(l), Box::new(e)), r)
    } else if let Ok((r, (_, _, e))) = tuple((tag("."), opt(ws), parse_expr))(r) {
        expr_ll(Expr::Bind(Box::new(l), Box::new(e)), r)
    } else if let Ok((r, (_, _, e))) = tuple((tag("="), opt(ws), parse_expr))(r) {
        expr_ll(Expr::Assign(Box::new(l), Box::new(e)), r)
    } else if let Ok((r, ty)) = type_specifier(r) {
        expr_ll(Expr::SpecifyTyped(Box::new(l), ty), r)
    } else {
        Ok((r, l))
    }
}
pub(crate) fn parse_expr(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Expr> {
    let (r, l) = alt((
        parse_closure,
        map(
            tuple((tag("("), opt(ws), parse_expr, opt(ws), tag(")"))),
            |(_, _, e, _, _)| e,
        ),
        map(literal, |l| Expr::Literal(l)),
        map(parse_object, |b| Expr::Object(b)),
        map(parse_block, |b| Expr::Block(b)),
        map(parse_if, |(a, b, c)| {
            Expr::If(Box::new(a), Box::new(b), Box::new(c))
        }),
        map(parse_for, |(a, b, c)| Expr::For(a, b, c)),
        map(separated_list1(tag("::"), ident), |s| Expr::Ident(s)),
        map(parse_multi_if, |(a, b)| Expr::MultiIf(a, Box::new(b))),
        map(parse_array, |a| Expr::Array(a)),
        // map(parse_),
    ))(s)?;
    expr_ll(l, r).map(|(s, e)| (s, reverse_bind(e)))
}

pub fn reverse_bind(e: Expr) -> Expr {
    match e {
        Expr::Quoted(e) => Expr::Quoted(Box::new(reverse_bind(*e))),
        Expr::Block(es) => Expr::Block(es.iter().map(|e| reverse_bind(e.clone())).collect()),
        Expr::Array(es) => Expr::Array(es.iter().map(|e| reverse_bind(e.clone())).collect()),
        Expr::Object(o) => Expr::Object(
            o.iter()
                .map(|(s, e)| (s.clone(), reverse_bind(e.clone())))
                .collect(),
        ),
        Expr::Closure(a, b, e) => Expr::Closure(a, b, Box::new(reverse_bind(*e))),
        Expr::If(a, b, c) => Expr::If(a, Box::new(reverse_bind(*b)), Box::new(reverse_bind(*c))),
        Expr::MultiIf(ifs, e) => Expr::MultiIf(
            ifs.iter()
                .map(|(e1, e2)| (reverse_bind(e1.clone()), reverse_bind(e2.clone())))
                .collect(),
            Box::new(reverse_bind(*e)),
        ),
        Expr::For(a, b, c) => Expr::For(a, Box::new(reverse_bind(*b)), Box::new(reverse_bind(*c))),
        Expr::Call(a, b) => Expr::Call(
            Box::new(reverse_bind(*a)),
            b.iter().map(|e| reverse_bind(e.clone())).collect(),
        ),
        Expr::ErrorHandle(e) => Expr::ErrorHandle(Box::new(reverse_bind(*e))),
        Expr::Bind(a, box Expr::Bind(b, c)) => {
            reverse_bind(Expr::Bind(Box::new(Expr::Bind(a, b)), c))
        }
        Expr::Index(a, b) => Expr::Index(Box::new(reverse_bind(*a)), Box::new(reverse_bind(*b))),
        Expr::Assign(a, b) => Expr::Assign(Box::new(reverse_bind(*a)), Box::new(reverse_bind(*b))),
        Expr::SpecifyTyped(a, t) => Expr::SpecifyTyped(Box::new(reverse_bind(*a)), t),
        e => e,
    }
}
#[test]
fn test_expr() {
    let exprs = [
        "{
        1?;
        2;
        3;
        }",
        "1",
        "a",
        "\"shit\"",
        "[ 1,\n 2,\t a,b][ 0]",
        "1(2,3)",
        "if 1 {\n\n\n\n2\n\n\n\n}\n\t\r else \n\n\n\n{\n\n\n\n3\n\n\n\n\n\n}",
        r"if
            | 2 { 3 }
                    |
                    4 {
                        5;
                    }
            else        { 6 }",
        "null",
        "err(\"an error\")",
        "a(1,2)(3,4).b(1).c(2).d()",
        "a[1][2]",
        "a = b = c[10] = d[\"t\"] = 1",
        "{a,\"b\":c, \nd\t:\r 3,  }",
        "for i in j {i}",
        "a::b::c()?.a::d",
        "a : (number,number) -> number = 1",
        "(a:number)->number {1; a[1]; if 1 {2} else {3}}",
    ];
    for e in exprs {
        let r = parse_expr(LocatedSpan::from(e));
        // assert!(r.is_ok());
        // let (r0, r1) = r.unwrap();
        // assert_eq!(r0, "");
        println!("{:?}\n", r);
    }
}
