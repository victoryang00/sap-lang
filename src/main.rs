#![feature(exclusive_range_pattern)]
#![feature(box_patterns)]
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;

use parser::expr::Expr;
mod interpreter;
mod parser;
mod utils;
use rustyline::error::ReadlineError;
use rustyline::Editor;

use crate::interpreter::{type_check_expr, TypeCheckContext};
use crate::parser::parse_top_level;

fn main() {
    println!("   ____\x1b[1;34m____ \x1b[0m    ___  __                  \x1b[1;32m| Next-GEN Confguration Template Generation Language\x1b[0m");
    println!("  / __\x1b[1;34m/ /\\ \\ *\x1b[0m / _ \\/ /  ___ ____  ___ _ \x1b[1;32m| \x1b[0m");
    println!(" _\\ \\\x1b[1;34m|-|  |-|\x1b[0m / ___/ /__/ _ `/ _ \\/ _ `/ \x1b[1;32m| Version: \x1b[0;34m0.1.0 alpha\x1b[0m");
    println!("/___/ \x1b[1;34m\\_\\/_/\x1b[0m_\\_/  /____/\\_,_/_//_/\\_, /  \x1b[1;32m| Press \x1b[0;34m'?'\x1b[1;32m or type in \x1b[0;34m'help'\x1b[1;32m for help\x1b[0m");
    println!(
        "                                 /___/   \x1b[1;32m| Author's Blog: \x1b[0;34m'https://www.lemonhx.moe/'\x1b[32m\x1b[0m"
    );
    let mut rl = Editor::<()>::new();
    if rl.load_history("history.txt").is_err() {
        println!("No previous history.");
    }
    let mut context = Rc::new(RefCell::new(TypeCheckContext {
        parent: None,
        free_var: HashMap::new(),
        alias: HashMap::new(),
    }));
    loop {
        let readline = rl.readline("\x1b[1;34msap-lang>>\x1b[0m ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.as_str() == "?" || line.as_str() == "help" {
                    println!(r"press Control-D for exit")
                } else if line.as_str() == "debug!" {
                    println!("{:?}", context);
                } else {
                    match parse_top_level(line.as_str()) {
                        Ok((_, r)) => {
                            println!("{:?}", r);
                            match r {
                                parser::TopLevel::Expr(mut e) => {
                                    let ty = type_check_expr(&mut e, context.clone());
                                    println!("{:?}", ty)
                                }
                                _ => {}
                            }
                        }
                        Err(e) => println!("ERROR, failed to parse {:?}", e),
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Interruped");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("EOF detected Exit");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
