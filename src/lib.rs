#![feature(exclusive_range_pattern)]
#![feature(box_patterns)]
#![feature(c_variadic)]
#![feature(vec_into_raw_parts)]
#![no_std]

use alloc::{format, string::String, vec::Vec};
use nom_locate::LocatedSpan;
use parser::{parse_top_level, TopLevel};

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
}
