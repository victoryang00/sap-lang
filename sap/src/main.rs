use std::collections::BTreeMap;

use clap::{clap_app, App, Arg};

fn main() {
    //     let matches = clap_app!( sap =>
    //         (name: "Sap-lang CLI")
    //         (version: "0.1.0 alpha")
    //         (about: "Next-GEN Confguration Template Generation Language")

    //         (@arg validate: -v --validate "Validate that file with type checker only")
    //         (@arg disable_type_checker: -dt --disable_type_checker "To disable type checker")
    //         (@arg INPUT:  "File input normally is a file end with .sap")
    //         // (@arg verbose: -v --verbose "Sets the level of verbosity")
    //         (@subcommand gen =>
    //             (@arg INPUT:  "Files need to convert into .sap")
    //             (@arg file_format: +required +takes_value -f --format [file_format] "File format for convention")
    //             (@arg output: +takes_value -o --output [output] "path of output")
    //         )
    //     )
    //     .get_matches();
    //     let sub = matches.subcommand_matches("gen");
    //     if sub.is_some() {
    //         println!("{:?}", sub.unwrap().values_of("file_format"))
    //     }
    let xml: BTreeMap<String, Vec<String>> =
        serde_xml_rs::from_str("<h1><title>gay</title><title>fuck</title></h1>").unwrap();
    let xml = serde_xml_rs::to_string(&xml).unwrap();
    println!("{:?}", xml)
}
