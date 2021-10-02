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

use super::{
    parse_comment, parse_comments,
    ty::{parse_type, Type},
};

pub(crate) mod literal;

#[derive(Debug, Clone)]
pub enum Expr {
    Quoted(Box<CommentedExpr>),
    Block(Vec<CommentedExpr>),
    Literal(Literal),
    Ident(Vec<String>),
    Array(Vec<CommentedExpr>),
    Object(Vec<((Option<String>, String), CommentedExpr)>),
    Closure(
        Vec<(String, Option<Type>)>,
        Option<Type>,
        Box<CommentedExpr>,
    ),
    If(Box<CommentedExpr>, Box<CommentedExpr>, Box<CommentedExpr>),
    MultiIf(Vec<(CommentedExpr, CommentedExpr)>, Box<CommentedExpr>),
    For(String, Box<CommentedExpr>, Box<CommentedExpr>),
    // ll
    Call(Box<CommentedExpr>, Vec<CommentedExpr>),
    ErrorHandle(Box<CommentedExpr>),
    Bind(Box<CommentedExpr>, Box<CommentedExpr>),
    Index(Box<CommentedExpr>, Box<CommentedExpr>),
    Assign(Box<CommentedExpr>, Box<CommentedExpr>),
    SpecifyTyped(Box<CommentedExpr>, Type),
}

#[derive(Debug, Clone)]
pub struct CommentedExpr {
    pub comment: Option<String>,
    pub expr: Expr,
}
impl CommentedExpr {
    pub fn from_expr(expr: Expr) -> Self {
        Self {
            comment: None,
            expr,
        }
    }
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

fn parse_block(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<CommentedExpr>> {
    // list 1 expand
    list1("{", "}", ";", parse_expr)(s)
}

pub(crate) fn parse_closure(
    s: LocatedSpan<&str>,
) -> IResult<
    LocatedSpan<&str>,
    (
        Vec<(String, Option<Type>)>,
        Option<Type>,
        Box<CommentedExpr>,
    ),
> {
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
        map(tuple((opt(ws), parse_expr)), |(_, e)| Box::new(e)),
    ))(s)
}

fn parse_array(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Vec<CommentedExpr>> {
    list0("[", "]", ",", parse_expr)(s)
}
// TODO: after comment
fn parse_object(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, Vec<((Option<String>, String), CommentedExpr)>> {
    list0("{", "}", ",", |s| {
        alt((
            map(
                tuple((
                    opt(parse_comments),
                    ident,
                    opt(ws),
                    tag(":"),
                    opt(ws),
                    parse_expr,
                )),
                |(c, s, _, _, _, e)| ((c, s.to_owned()), e),
            ),
            map(
                tuple((
                    opt(parse_comments),
                    string,
                    opt(ws),
                    tag(":"),
                    opt(ws),
                    parse_expr,
                )),
                |(c, s, _, _, _, e)| ((c, s), e),
            ),
            map(
                tuple((opt(parse_comments), separated_list1(tag("::"), ident))),
                |(c, s)| {
                    (
                        (c, s.last().unwrap().clone()),
                        CommentedExpr {
                            comment: None,
                            expr: Expr::Ident(s),
                        },
                    )
                },
            ),
        ))(s)
    })(s)
}
fn parse_if(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, (CommentedExpr, CommentedExpr, CommentedExpr)> {
    map(
        tuple((
            tag("if"),
            ws,
            parse_expr,
            opt(ws),
            parse_expr,
            opt(ws),
            tuple((tag("else"), ws, parse_expr)),
        )),
        |(_, _, e, _, b, _, (_, _, o))| (e, b, o),
    )(s)
}

fn parse_multi_if_body(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, Vec<(CommentedExpr, CommentedExpr)>> {
    map(
        tuple((
            tag("|"),
            ws,
            separated_list1(
                tuple((opt(ws), tag("|"), ws)),
                map(tuple((parse_expr, opt(ws), parse_expr)), |(a, _, b)| (a, b)),
            ),
        )),
        |(_, _, s)| s,
    )(s)
}

fn parse_for(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, (String, Box<CommentedExpr>, Box<CommentedExpr>)> {
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
            parse_expr,
        )),
        |(_, _, i, _, _, _, e, _, b)| (i.to_owned(), Box::new(e), Box::new(b)),
    )(s)
}

fn parse_multi_if(
    s: LocatedSpan<&str>,
) -> IResult<LocatedSpan<&str>, (Vec<(CommentedExpr, CommentedExpr)>, CommentedExpr)> {
    map(
        tuple((
            tag("if"),
            ws,
            parse_multi_if_body,
            opt(ws),
            tuple((tag("else"), ws, parse_expr)),
        )),
        |(_, _, l, _, (_, _, e))| (l, e),
    )(s)
}

fn q(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, LocatedSpan<&str>> {
    tag("?")(s)
}
fn type_specifier(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Type> {
    map(tuple((tag(":"), opt(ws), parse_type)), |(_, _, ty)| ty)(s)
}
fn expr_ll(l: CommentedExpr, r: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, CommentedExpr> {
    let (r, (comment, _)) = tuple((opt(parse_comments), opt(ws)))(r)?;
    if let Ok((r, _)) = q(r) {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::ErrorHandle(Box::new(l)),
            },
            r,
        )
    } else if let Ok((r, c)) = list0("(", ")", ",", parse_expr)(r) {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::Call(Box::new(l), c),
            },
            r,
        )
    } else if let Ok((r, (_, _, e, _, _))) =
        tuple((tag("["), opt(ws), parse_expr, opt(ws), tag("]")))(r)
    {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::Index(Box::new(l), Box::new(e)),
            },
            r,
        )
    } else if let Ok((r, (_, _, e))) = tuple((tag("."), opt(ws), parse_expr))(r) {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::Bind(Box::new(l), Box::new(e)),
            },
            r,
        )
    } else if let Ok((r, (_, _, e))) = tuple((tag("="), opt(ws), parse_expr))(r) {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::Assign(Box::new(l), Box::new(e)),
            },
            r,
        )
    } else if let Ok((r, ty)) = type_specifier(r) {
        expr_ll(
            CommentedExpr {
                comment,
                expr: Expr::SpecifyTyped(Box::new(l), ty),
            },
            r,
        )
    } else {
        Ok((r, l))
    }
}
pub(crate) fn parse_expr(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, CommentedExpr> {
    let (l, r) = map(
        tuple((
            opt(parse_comments),
            alt((
                map(parse_closure, |(a, b, c)| Expr::Closure(a, b, c)),
                map(
                    tuple((tag("("), opt(ws), parse_expr, opt(ws), tag(")"))),
                    |(_, _, e, _, _)| Expr::Quoted(Box::new(e)),
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
            )),
            opt(parse_comments),
        )),
        |(c1, e, c2)| {
            let comment = match c1 {
                Some(c1) => match c2 {
                    Some(c2) => Some(c1 + &c2),
                    None => Some(c1),
                },
                None => c2,
            };
            CommentedExpr { comment, expr: e }
        },
    )(s)?;
    expr_ll(r, l).map(|(s, e)| (s, reverse_bind(e)))
}

pub fn reverse_bind(e: CommentedExpr) -> CommentedExpr {
    let CommentedExpr { comment, expr } = e;
    fn reverse_bind_expr(expr: Expr) -> Expr {
        match expr {
            Expr::Quoted(e) => Expr::Quoted(Box::new(reverse_bind(*e))),
            Expr::Block(es) => Expr::Block(es.iter().map(|e| reverse_bind(e.clone())).collect()),
            Expr::Array(es) => Expr::Array(es.iter().map(|e| reverse_bind(e.clone())).collect()),
            Expr::Object(o) => Expr::Object(
                o.iter()
                    .map(|(s, e)| (s.clone(), reverse_bind(e.clone())))
                    .collect(),
            ),
            Expr::Closure(a, b, e) => Expr::Closure(a, b, Box::new(reverse_bind(*e))),
            Expr::If(a, b, c) => {
                Expr::If(a, Box::new(reverse_bind(*b)), Box::new(reverse_bind(*c)))
            }
            Expr::MultiIf(ifs, e) => Expr::MultiIf(
                ifs.iter()
                    .map(|(e1, e2)| (reverse_bind(e1.clone()), reverse_bind(e2.clone())))
                    .collect(),
                Box::new(reverse_bind(*e)),
            ),
            Expr::For(a, b, c) => {
                Expr::For(a, Box::new(reverse_bind(*b)), Box::new(reverse_bind(*c)))
            }
            Expr::Call(a, b) => Expr::Call(
                Box::new(reverse_bind(*a)),
                b.iter().map(|e| reverse_bind(e.clone())).collect(),
            ),
            Expr::ErrorHandle(e) => Expr::ErrorHandle(Box::new(reverse_bind(*e))),
            Expr::Bind(
                box CommentedExpr {
                    comment: c1,
                    expr: a,
                },
                box CommentedExpr {
                    comment: c2,
                    expr: Expr::Bind(b, c),
                },
            ) => reverse_bind_expr(Expr::Bind(
                Box::new(CommentedExpr {
                    comment: c2,
                    expr: Expr::Bind(
                        Box::new(CommentedExpr {
                            comment: c1,
                            expr: a,
                        }),
                        b,
                    ),
                }),
                c,
            )),
            Expr::Index(a, b) => {
                Expr::Index(Box::new(reverse_bind(*a)), Box::new(reverse_bind(*b)))
            }
            Expr::Assign(a, b) => {
                Expr::Assign(Box::new(reverse_bind(*a)), Box::new(reverse_bind(*b)))
            }
            Expr::SpecifyTyped(a, t) => Expr::SpecifyTyped(Box::new(reverse_bind(*a)), t),
            e => e,
        }
    }
    CommentedExpr {
        comment,
        expr: reverse_bind_expr(expr),
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
