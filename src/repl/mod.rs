mod highlighter;

use std::cell::{RefCell, UnsafeCell};
use std::collections::{BTreeMap, HashMap};
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::interpreter::interpreter::{eval_expr, EvalContext};
use crate::interpreter::typechecker::{type_check_expr, TypeCheckContext};
use crate::interpreter::Runner;
use crate::parser::expr::Expr;
use crate::parser::parse_top_level;
use rustyline::error::ReadlineError;

pub unsafe extern "C" fn add(n: usize, mut args: ...) -> usize {
    let mut sum = 0;
    for _ in 0..n {
        sum += args.arg::<usize>();
    }
    sum
}

use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::config::OutputStreamType;
use rustyline::highlight::{Highlighter, MatchingBracketHighlighter};
use rustyline::hint::{Hinter, HistoryHinter};
use rustyline::validate::{self, MatchingBracketValidator, Validator};
use rustyline::{Cmd, CompletionType, Config, Context, EditMode, Editor, KeyEvent};
use rustyline_derive::Helper;
use std::borrow::Cow::{self, Borrowed, Owned};

#[derive(Helper)]
struct SapRepl {
    completer: FilenameCompleter,
    highlighter: MatchingBracketHighlighter,
    validator: MatchingBracketValidator,
    hinter: HistoryHinter,
    colored_prompt: String,
}

impl Completer for SapRepl {
    type Candidate = Pair;

    fn complete(
        &self,
        line: LocatedSpan<&str>,
        pos: usize,
        ctx: &Context<'_>,
    ) -> Result<(usize, Vec<Pair>), ReadlineError> {
        self.completer.complete(line, pos, ctx)
    }
}

impl Hinter for SapRepl {
    type Hint = String;

    fn hint(&self, line: LocatedSpan<&str>, pos: usize, ctx: &Context<'_>) -> Option<String> {
        self.hinter.hint(line, pos, ctx)
    }
}

impl Highlighter for SapRepl {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        default: bool,
    ) -> Cow<'b, str> {
        if default {
            Borrowed(&self.colored_prompt)
        } else {
            Borrowed(prompt)
        }
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: LocatedSpan<&str>, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Validator for SapRepl {
    fn validate(
        &self,
        ctx: &mut validate::ValidationContext,
    ) -> rustyline::Result<validate::ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}

pub fn repl() {
    println!("   ____\x1b[1;34m____ \x1b[0m    ___  __                  \x1b[1;32m| Next-GEN Confguration Template Generation Language\x1b[0m");
    println!("  / __\x1b[1;34m/ /\\ \\ *\x1b[0m / _ \\/ /  ___ ____  ___ _ \x1b[1;32m| \x1b[0m");
    println!(" _\\ \\\x1b[1;34m|-|  |-|\x1b[0m / ___/ /__/ _ `/ _ \\/ _ `/ \x1b[1;32m| Version: \x1b[0;34m0.1.0 alpha\x1b[0m");
    println!("/___/ \x1b[1;34m\\_\\/_/\x1b[0m_\\_/  /____/\\_,_/_//_/\\_, /  \x1b[1;32m| Press \x1b[0;34m'?'\x1b[1;32m or type in \x1b[0;34m'help'\x1b[1;32m for help\x1b[0m");
    println!(
        "                                 /___/   \x1b[1;32m| Author's Blog: \x1b[0;34m'https://www.lemonhx.moe/'\x1b[32m\x1b[0m"
    );
    let config = Config::builder()
        .color_mode(rustyline::ColorMode::Forced)
        .auto_add_history(false)
        .build();
    let mut rl = Editor::<SapRepl>::with_config(config);
    rl.set_helper(Some(SapRepl {
        completer: FilenameCompleter::new(),
        highlighter: MatchingBracketHighlighter::new(),
        hinter: HistoryHinter {},
        colored_prompt: "\x1b[1;34msap-lang>>\x1b[0m ".to_owned(),
        validator: MatchingBracketValidator::new(),
    }));
    let mut runner = Runner::new_with_std();
    loop {
        let readline = rl.readline("\x1b[1;34msap-lang>>\x1b[0m ");

        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str());
                if line.as_str() == "?" || line.as_str() == "help" {
                    println!(r"press Control-D for exit")
                } else if line.as_str() == "debug!" {
                    println!("{:?}", runner)
                } else {
                    match parse_top_level(line.as_str()) {
                        Ok((l, r)) => {
                            if l != "" {
                                println!("\x1b[1;31munable to parse substring:\x1b[0;0m {:?}", l);
                            }
                            println!("parsed expr: {:?}", r);
                            match r {
                                crate::parser::TopLevel::Expr(mut e) => {
                                    let (t, v) = runner.run_expr(e);
                                    match v {
                                        Ok(t) => {
                                            rl.add_history_entry(line);
                                            print!("{:?}", unsafe { &*t.as_ref().get() })
                                        }
                                        Err(e) => {
                                            print!("\x1b[1;31meval error: {:?}\x1b[0m", e)
                                        }
                                    }
                                    match t {
                                        Ok(t) => {
                                            println!("\x1b[1;32m\ttype: {:?}\x1b[0m", t)
                                        }
                                        Err(e) => {
                                            println!("\x1b[1;31m\ttype check error: {:?}\x1b[0m", e)
                                        }
                                    }
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
    rl.save_history(
        format!(
            "repl_{:?}.sap",
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("Time went backwards")
                .as_millis()
        )
        .as_str(),
    )
    .unwrap();
}
