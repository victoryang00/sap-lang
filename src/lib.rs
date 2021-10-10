#![feature(exclusive_range_pattern)]
#![feature(box_patterns)]
#![feature(c_variadic)]
#![feature(vec_into_raw_parts)]

use alloc::{format, string::String, vec::Vec};
use nom::{combinator::opt, multi::many0, sequence::tuple};
use nom_locate::LocatedSpan;
use parser::top_level::TopLevel;

use std::{format, string::String, vec::Vec};

use crate::{parser::Parser, utils::ws};

extern crate alloc;
pub mod parser;
pub mod state;
pub mod utils;

pub fn parse_single_line(s: &str) -> Result<TopLevel, String> {
    TopLevel::parse(LocatedSpan::new(s))
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
<<<<<<< HEAD
pub fn parse_all_line(s: &str) -> Result<Vec<TopLevel>, String> {
    let s_vec: Vec<&str> = s.trim().split("\n").collect();
    let mut r_vec: Vec<TopLevel> = alloc::vec![];
    for s in s_vec {
        if s != "" {
            match parse_top_level(LocatedSpan::new(s))
                .ok()
                .map(|(l, r)| {
                    if l.fragment() != &"" {
                        Err(format!("could not parse substring at position: {:?}", l))
                    } else {
                        Ok(r)
                    }
                })
                .unwrap()
            {
                Ok(tmp) => r_vec.push(tmp),
                Err(s) => return Err(format!("failed to parse {:?}", s)),
            }
        }
    }
    Ok(r_vec)
=======

pub fn parse_multi_line(s: &str) -> Result<Vec<TopLevel>, String> {
    let s = LocatedSpan::new(s);
    let t = match many0(tuple((opt(ws), TopLevel::parse, opt(ws))))(s) {
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
>>>>>>> upstream/master
}
