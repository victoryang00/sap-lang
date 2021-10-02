use super::escape_code;
use nom::{
    bytes::complete::{escaped, is_not, tag},
    combinator::map,
    sequence::delimited,
    IResult,
};
use nom_locate::LocatedSpan;
// TODO: limit one char
fn char(s: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, char> {
    map(
        delimited(
            tag("\'"),
            escaped(is_not("\"\\"), '\\', escape_code),
            tag("\'"),
        ),
        |s: LocatedSpan<&str>| s.chars().into_iter().next().unwrap(),
    )(s)
}
