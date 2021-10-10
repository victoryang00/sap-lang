use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_till},
    character::complete::{digit1, hex_digit1},
    combinator::{map, opt, recognize},
    sequence::tuple,
    IResult,
};
use nom_locate::LocatedSpan;
use std::string::{String, ToString};

use super::Number;

fn hex(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Number> {
    fn str2i(s: LocatedSpan<&str>) -> i128 {
        i128::from_str_radix(&s[2..], 16).unwrap()
    }
    map(recognize(tuple((tag_no_case("0x"), hex_digit1))), |s| {
        Number::Integer(str2i(s))
    })(s)
}

fn bin(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Number> {
    fn str2i(s: LocatedSpan<&str>) -> i128 {
        i128::from_str_radix(&s[2..], 2).unwrap()
    }
    map(
        recognize(tuple((
            tag_no_case("0b"),
            take_till(|c| c != '0' && c != '1'),
        ))),
        |s| Number::Integer(str2i(s)),
    )(s)
}

fn int(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Number> {
    let (o, (s, v)) = tuple((
        opt(alt((tag("-"), tag("+")))),
        alt((
            hex,
            bin,
            map(digit1, |s: LocatedSpan<&str>| {
                Number::Integer(i128::from_str_radix(s.fragment(), 10).unwrap())
            }),
        )),
    ))(s)?;
    if let Some(c) = s {
        if c.fragment() == &"-" {
            let v = Number::Integer(-v.int());
            Ok((o, v))
        } else {
            Ok((o, v))
        }
    } else {
        Ok((o, v))
    }
}
fn float(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Number> {
    map(
        alt((
            map(
                tuple((
                    map(opt(int), |s| {
                        if let Some(n) = s {
                            n.int().to_string()
                        } else {
                            "0".to_string()
                        }
                    }),
                    map(
                        tuple((
                            tag("."),
                            map(
                                alt((
                                    bin,
                                    map(digit1, |s: LocatedSpan<&str>| {
                                        Number::Integer(
                                            i128::from_str_radix(s.fragment(), 10).unwrap(),
                                        )
                                    }),
                                )),
                                |n| n.int().to_string(),
                            ),
                        )),
                        |(a, b): (LocatedSpan<&str>, String)| a.fragment().to_string() + b.as_str(),
                    ),
                    map(opt(tuple((tag_no_case("e"), int))), |o| {
                        let (e, n) = o?;
                        Some((e, n.int().to_string()))
                    }),
                )),
                |(a, b, c): (String, String, Option<(LocatedSpan<&str>, String)>)| {
                    if let Some((c1, c2)) = c {
                        a + &b + c1.fragment() + &c2
                    } else {
                        a + &b
                    }
                },
            ),
            map(
                tuple((
                    map(int, |s| s.int().to_string()),
                    map(
                        opt(tuple((
                            tag("."),
                            map(
                                alt((
                                    bin,
                                    map(digit1, |s: LocatedSpan<&str>| {
                                        Number::Integer(
                                            i128::from_str_radix(s.fragment(), 10).unwrap(),
                                        )
                                    }),
                                )),
                                |n| n.int().to_string(),
                            ),
                        ))),
                        |o: Option<(LocatedSpan<&str>, String)>| {
                            if let Some((a, b)) = o {
                                a.fragment().to_string() + b.as_str()
                            } else {
                                "".to_string()
                            }
                        },
                    ),
                    map(tuple((tag_no_case("e"), int)), |o| {
                        let (e, n) = o;
                        (e, n.int().to_string())
                    }),
                )),
                |(a, b, (c1, c2)): (String, String, (LocatedSpan<&str>, String))| {
                    a + &b + c1.fragment() + &c2
                },
            ),
        )),
        |s| {
            // println!("s is: {}", s);
            Number::Floating(
                s.parse::<f64>()
                    .expect("ERROR could not covert value to valid float"),
            )
        },
    )(s)
}

pub fn number(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, Number> {
    alt((float, int))(s)
}

#[test]
fn test_parse_number() {
    let numbers = [
        "123",
        "+123",
        "-123",
        "0b110110",
        "+0b110110",
        "-0B110110",
        "0xFFFF",
        "+0xFFFF",
        "-0xFFFF",
        "3.14",
        "+3.14",
        "-3.14",
        "+0b110.114514",
        "+0xFFFF.0b110E-0xFFFF",
        ".10",
        "-1E10",
        ".0b110110E0xFF",
    ];
    for res in numbers
        .iter()
        .map(|s| number(LocatedSpan::from(*s)).unwrap())
    {
        // println!("{:?}", res);
        assert_eq!(res.0.fragment(), &"");
    }
}
