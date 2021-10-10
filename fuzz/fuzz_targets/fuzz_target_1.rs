#![no_main]
use libfuzzer_sys::fuzz_target;
extern crate sap_lang;
use sap_lang::parse_multi_line;
use std::str;
use sap_lang::state::SapState;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
    let mut runner = SapState::new_with_std();

    if let Ok(utf8_str) = str::from_utf8(data) {
        let _ = parse_multi_line(utf8_str);
    }
});
