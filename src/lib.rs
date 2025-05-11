#![warn(missing_docs)]
//! The automata language implementation library

/// Module for dfa construction
pub mod dfa;
/// Module for generating dfa
pub mod enumerate;
/// Module for evaluating automata programs
pub mod eval;
/// module for parsing automata programs
pub mod parse;
/// Module for program syntax
pub mod syntax;
/// Module for typechecking automata programs
pub mod typecheck;

use crate::dfa::*;
use std::panic;
use syntax::Symbol;
use wasm_bindgen::prelude::*;

/// Initialize panic hook only once
#[wasm_bindgen(start)]
pub fn start() {
    // Set panic hook to get better error messages in JavaScript
    panic::set_hook(Box::new(console_error_panic_hook::hook));
}

/// Process an automata program and return a Mermaid diagram
#[wasm_bindgen]
pub fn render_mermaid(code: &str) -> Result<String, JsValue> {
    // parse -> typecheck -> enumerate -> generate mermaid
    let result = process_program(code);

    match result {
        Ok(diagram) => Ok(format!("{}", diagram)),
        Err(err) => Err(JsValue::from_str(&format!("Error: {}", err))),
    }
}

/// Private function to process the automata program to a DFA
fn process_program(code: &str) -> Result<Dfa<Symbol>, String> {
    let program = match parse::parse(code) {
        Ok(prog) => prog,
        Err(err) => return Err(format!("Parse error: {}", err)),
    };

    if let Err(err) = typecheck::typecheck_program(&program) {
        return Err(format!("Type error: {}", err));
    }

    let dfa = match enumerate::enumerate(&program) {
        Ok(dfa) => dfa,
        Err(err) => return Err(format!("RuntimeError: {}", err)),
    };

    Ok(dfa)
}
