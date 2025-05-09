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

use crate::dfa::Dfa;
use std::panic;
use wasm_bindgen::prelude::*;

/// Initialize panic hook only once
#[wasm_bindgen(start)]
pub fn start() {
    // Set panic hook to get better error messages in JavaScript
    panic::set_hook(Box::new(console_error_panic_hook::hook));
}

/// Process an automata program and return a Mermaid diagram
#[wasm_bindgen]
pub fn generate_mermaid_diagram(code: &str) -> Result<String, JsValue> {
    // Process: parse -> typecheck -> enumerate -> generate mermaid
    let result = process_automata_program(code);

    match result {
        Ok(diagram) => Ok(diagram),
        Err(err) => Err(JsValue::from_str(&format!("Error: {}", err))),
    }
}

/// Parse an automata program and return its typed representation
#[wasm_bindgen]
pub fn parse_automata_program(code: &str) -> Result<JsValue, JsValue> {
    // Parse the program
    let program = match parse::parse(code) {
        Ok(prog) => prog,
        Err(err) => return Err(JsValue::from_str(&format!("Parse error: {}", err))),
    };

    // Convert to JSON representation
    let result = JsValue::from_str(&format!(
        "Successfully parsed program with {} statements",
        program.start.len()
    ));

    Ok(result)
}

/// Typecheck an automata program and return a success status
#[wasm_bindgen]
pub fn typecheck_automata_program(code: &str) -> Result<bool, JsValue> {
    // Parse the program
    let program = match parse::parse(code) {
        Ok(prog) => prog,
        Err(err) => return Err(JsValue::from_str(&format!("Parse error: {}", err))),
    };

    // Typecheck the program
    match typecheck::typecheck_program(&program) {
        Ok(_) => Ok(true),
        Err(err) => Err(JsValue::from_str(&format!("Type error: {}", err))),
    }
}

/// Generate a DFA from an automata program and return the Mermaid representation
#[wasm_bindgen]
pub fn generate_dfa(code: &str) -> Result<String, JsValue> {
    // Process the program to get the DFA
    let dfa = match process_automata_program_to_dfa(code) {
        Ok(dfa) => dfa,
        Err(err) => return Err(JsValue::from_str(&format!("Error: {}", err))),
    };

    // Convert DFA to string format using the Display implementation
    let dfa_string = format!("{}", dfa);

    Ok(dfa_string)
}

/// Private function to process the automata program fully to Mermaid code
fn process_automata_program(code: &str) -> Result<String, String> {
    // Get the DFA
    let dfa = process_automata_program_to_dfa(code)?;

    // Convert DFA to mermaid format - using the Display implementation
    let mermaid_code = format!("{}", dfa);

    Ok(mermaid_code)
}

/// Private function to process the automata program to a DFA
fn process_automata_program_to_dfa(code: &str) -> Result<Dfa<crate::syntax::Symbol>, String> {
    // Parse the program
    let program = match parse::parse(code) {
        Ok(prog) => prog,
        Err(err) => return Err(format!("Parse error: {}", err)),
    };

    // Typecheck the program
    if let Err(err) = typecheck::typecheck_program(&program) {
        return Err(format!("Type error: {}", err));
    }

    // Enumerate states to generate the DFA
    let dfa = match enumerate::enumerate(&program) {
        Ok(dfa) => dfa,
        Err(err) => return Err(format!("Runtime error during enumeration: {}", err)),
    };

    Ok(dfa)
}
