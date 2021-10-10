use nom::branch::alt;
use nom::bytes::complete::{escaped, is_not, tag, take_until};
use nom::character::complete::{char};

use nom::combinator::map;

use nom::sequence::{delimited};
use nom::{IResult};
use nom_locate::LocatedSpan;
use std::borrow::ToOwned;
use std::string::{String, ToString};
use std::vec::Vec;

use super::escape_code;

enum ControlChar {
    Byte(u8),
    IgnoreWhitespace,
    HexLit,
    OctLit,
    Codepoint,
}

pub fn rewrite<'a>(
    o: LocatedSpan<&'a str>,
    st: LocatedSpan<&'a str>,
) -> IResult<LocatedSpan<&'a str>, String> {
    let mut res = String::new();
    let chars: Vec<char> = st.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '\\' {
            if i + 1 != chars.len() {
                i += 1;
                let c = chars[i];
                match c.to_ascii_lowercase() {
                    '{' => res.push(c),
                    '}' => res.push(c),
                    '\\' => res.push(c),
                    '\'' => res.push(c),
                    '"' => res.push(c),
                    'n' => res.push('\n'),
                    'a' => res.push('\u{07}'), // bell
                    'b' => res.push('\u{08}'), // backspace
                    'f' => res.push('\u{0c}'), // form feed
                    'r' => res.push('\u{0d}'), // carriage return
                    't' => res.push('\u{09}'), // horizontal tab
                    'v' => res.push('\u{0b}'), // vertical tab
                    'x' => {
                        i += 1;
                        let mut string = String::new();
                        while i != chars.len()
                            && "0123456789abcdef"
                                .chars()
                                .collect::<Vec<char>>()
                                .contains(&chars[i].to_ascii_lowercase())
                        {
                            string.push(chars[i]);
                            i += 1;
                        }
                        i -= 1;
                        let c = u32::from_str_radix(&string, 16).unwrap();
                        // println!("C is {}", c);
                        let c = char::from_u32(c).unwrap();
                        res.push(c);
                    }
                    c @ '0'..'9' => {
                        i += 1;
                        let mut string = String::new();
                        string.push(c);
                        while i != chars.len() && ('0'..'9').contains(&chars[i]) {
                            string.push(chars[i]);
                            i += 1;
                        }
                        let c = u32::from_str_radix(&string, 8).unwrap();
                        let c = char::from_u32(c).unwrap();
                        res.push(c);
                        i -= 1;
                    }
                    _ => {
                        return Err(nom::Err::Error(nom::error::Error::new(
                            o,
                            nom::error::ErrorKind::Escaped,
                        )));
                    }
                };
                i += 1;
            } else {
                return Err(nom::Err::Error(nom::error::Error::new(
                    o,
                    nom::error::ErrorKind::Escaped,
                )));
            }
        } else {
            res.push(chars[i]);
            i += 1;
        }
    }
    Ok((o, res))
}

pub fn quoted_string(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (o, i) = delimited(
        char('"'),
        escaped(is_not("\"\\"), '\\', escape_code),
        char('"'),
    )(s)?;
    rewrite(o, i)
}

pub fn raw_string(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (l, _) = tag("##RAW")(s)?;
    let (l, s) = map(take_until("##"), |s: LocatedSpan<&str>| {
        s.fragment().to_string()
    })(l)?;
    let (l, _) = tag("##")(l)?;
    Ok((l, s))
}

pub fn string(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, String> {
    let (o, s) = alt((quoted_string, raw_string))(s)?;
    Ok((o, s.to_owned()))
}
