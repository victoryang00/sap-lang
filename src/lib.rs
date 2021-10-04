#![feature(exclusive_range_pattern)]
#![feature(box_patterns)]
#![feature(c_variadic)]
#![feature(vec_into_raw_parts)]
#![no_std]

use alloc::{format, string::String, vec::Vec};
use nom::{
    combinator::opt,
    multi::{many0, many1},
    sequence::tuple,
};
use nom_locate::LocatedSpan;
use parser::{parse_top_level, TopLevel};

use crate::utils::ws;

extern crate alloc;
pub mod interpreter;
pub mod parser;
pub mod utils;

pub fn parse_single_line(s: &str) -> Result<TopLevel, String> {
    parse_top_level(LocatedSpan::new(s))
        .ok()
        .map(|(l, r)| {
            if l.fragment() != &"" {
                Err(format!("could not parse substring at position: {:?}", l))
            } else {
                Ok(r)
            }
        })
        .unwrap_or(Err(format!("failed to parse {:?}", s)))
}

pub fn parse_multi_line(s: &str) -> Result<Vec<TopLevel>, String> {
    let s = LocatedSpan::new(s);
    let t = match many0(tuple((opt(ws), parse_top_level, opt(ws))))(s) {
        Ok((l, r)) => {
            if l.fragment() != &"" {
                Err(format!("could not parse substring at position: {:?}", l))
            } else {
                Ok(r)
            }
        }
        Err(e) => Err(format!("{:?}", e)),
    }?;
    Ok(t.iter().map(|(_, s, _)| s.clone()).collect())
}
