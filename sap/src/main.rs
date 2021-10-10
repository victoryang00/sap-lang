use std::{cell::UnsafeCell, collections::BTreeMap, rc::Rc};

use clap::{clap_app, App, Arg};
use quick_xml::Reader;
use sap_lang::{parser::expr::literal::Number, state::evaluator::value::Value};

fn main() {
    let v: serde_json::Value = quick_xml::de::from_str(
        "<root>
            <h1 shit=\"shit\">
                <text> gay </text>
                <h2> 
                    <text> gay1 </text> 
                    <h3> fuck2 </h3>
                    <text> gay3 </text>
                </h2> 
                <h3 />
            </h1>
        </root>",
    )
    .unwrap();
    println!("{}", v);
}
