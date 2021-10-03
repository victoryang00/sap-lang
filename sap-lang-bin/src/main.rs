extern crate alloc;
use clap::{App, Arg};
use sap_lang::{parse_all_line};

use sap_lang::interpreter::Runner;

fn main() {
    let matches = App::new("sap-lang-bin")
        .version("0.1.0")
        .arg(
            Arg::with_name("file")
                .takes_value(true)
                .help("Sap Lang file to input"),
        )
        .arg(
            Arg::with_name("output")
                .short("o")
                .long("output")
                .takes_value(true)
                .help("The output path you specify"),
        )
        .arg(
            Arg::with_name("serializer")
                .short("serializer")
                .takes_value(true)
                .help("The output format you specify"),
        )
        .arg(
            Arg::with_name("url")
                .short("url")
                .takes_value(true)
                .help("The website you want to get into"),
        )
        .get_matches();
    let serializer = matches.value_of("serializer").unwrap_or("json");
    let postfix = format!("output.{}", serializer);
    let infile = matches.value_of("file").unwrap_or("input.sap");
    let outfile = matches.value_of("output").unwrap_or(postfix.as_str());
    let contents = std::fs::read_to_string(infile).expect("Something went wrong reading the file");
    let mut runner = Runner::new_with_std();
    match parse_all_line(contents.as_str()) {
        Ok(r) => {
            println!("parsed: {:?}", r);
            for r in r {
                match r {
                    sap_lang::parser::TopLevel::Comment(c) => {
                        println!("comment: {:?}", c)
                    }
                    sap_lang::parser::TopLevel::Expr(e) => {
                        let (t, v) = runner.run(*e);
                        match v {
                            Ok(t) => unsafe {
                                std::fs::write(outfile, format!("{:?}", &*t.as_ref().get()))
                                    .expect("Something went wrong writing the file");
                            },
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
        }
        Err(e) => println!("\x1b[1;31mError: \x1b[0;0m {}", e),
    }
}
