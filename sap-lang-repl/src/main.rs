extern crate alloc;
use alloc::collections::BTreeMap;
use alloc::rc::Rc;
use core::cell::{RefCell, UnsafeCell};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use sap_lang::parse_single_line;
use sap_lang::parser::ty::Type;

use sap_lang::interpreter::interpreter::{eval_expr, EvalContext};
use sap_lang::interpreter::type_checker::{type_check_expr, TypeCheckContext};
use sap_lang::interpreter::Runner;

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
    let mut runner = Runner::new_with_std();

    'r: loop {
        let mut prompt = "\x1b[1;34msap-lang>>\x1b[0m ";
        let mut line = String::new();
        loop {
            let readline = rl.readline(prompt);
            match readline {
                Ok(l) => {
                    line.push_str(&l);
                }
                Err(ReadlineError::Interrupted) => {
                    println!("Exit");
                    break 'r;
                }
                Err(ReadlineError::Eof) => {
                    println!("EOF detected drop current line");
                    break;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    break 'r;
                }
            }
            if line.as_str() == "?" || line.as_str() == "help" {
                rl.add_history_entry(line.as_str());
                line.clear();
                println!(r"press Control-C for exit");
                println!(r"press Control-D to abandon current line")
            } else if line.as_str() == "debug!" {
                rl.add_history_entry(line.as_str());
                line.clear();
                println!("{:?}", runner)
            } else {
                match parse_single_line(line.as_str()) {
                    Ok(r) => {
                        rl.add_history_entry(line.as_str());
                        line.clear();
                        prompt = "\x1b[1;34msap-lang>>\x1b[0m ";
                        println!("parsed: {}", r);
                        match &r {
                            sap_lang::parser::TopLevel::Comment(c) => {
                                println!("comment: {:?}", c)
                            }
                            sap_lang::parser::TopLevel::Expr(e) => {
                                let (t, v) = runner.type_check_and_run(r);
                                match v {
                                    Ok(t) => {
                                        print!("eval result: {:?}", unsafe { &*t.as_ref().get() })
                                    }
                                    Err(e) => {
                                        print!("\x1b[1;31meval error: {:?}\x1b[0m", e)
                                    }
                                }
                                match t {
                                    Ok(t) => {
                                        println!(
                                            "\x1b[1;32m\ttype: {}\x1b[0m",
                                            t.unwrap_or(Type::Any)
                                        )
                                    }
                                    Err(e) => {
                                        println!("\x1b[1;31m\ttype check error: {:?}\x1b[0m", e)
                                    }
                                }
                            }
                            &sap_lang::parser::TopLevel::TypeDef(_, _) => {
                                let (t, _) = runner.type_check_and_run(r);
                                match t {
                                    Ok(t) => {
                                        println!("\x1b[1;32m\ttype: {}\x1b[0m", t.unwrap())
                                    }
                                    Err(e) => {
                                        println!("\x1b[1;31m\ttype check error: {:?}\x1b[0m", e)
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    Err(e) => {
                        line.push_str("\n"); // separate input lines
                        prompt = "\x1b[1;34m*>>\x1b[0m ";
                    }
                }
            }
        }
    }
    rl.save_history("history.txt").unwrap();
}
