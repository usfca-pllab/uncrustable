//! The DFA parser for mentor-like syntax.
//!
//! The syntax for DFA descriptions is:
//!
//! ```
//! alphabet: { <symbol>, ... }
//! start: <state>
//! accepting: { <state>, ... }
//!
//! <state> (<symbol> -> <state>) ...
//! ```
//!
//! The alphabet cannot be empty.
//!
//! State names can contain any alphanumeric character, underscore and apostrophe.
//! State names cannot start with underscore or apostrophe.
//!
//! Symbols are any single character other than `:{}()->_'`.
//!
//! C++-style line comments can be used in DFA files.
#![allow(missing_docs, reason = "pest-generated code does not have docs")]

use super::*;
use pest::{iterators::Pair, Parser};

#[derive(pest_derive::Parser)]
#[grammar_inline = r#"
WHITESPACE = _{ " " | "\t" | NEWLINE }
COMMENT = _{ "//" ~ (!NEWLINE ~ ANY)* ~ &NEWLINE }

dfa = { SOI ~ alphabet_decl ~ start_decl ~ accepting_decl ~ (rule_decl)* ~ EOI }

alphabet_decl = { "alphabet:" ~ "{" ~ symbol ~ ("," ~ symbol)* ~ "}" ~ NEWLINE }
accepting_decl = { "accepting:" ~ "{" ~ (state ~ ("," ~ state)*)? ~ "}" ~ NEWLINE }
start_decl = { "start:" ~ state ~ NEWLINE }
rule_decl = { state ~ transition* }
transition = { "(" ~ symbol ~ "->" ~ state ~ ")" }

state = @{ ((("@" | "_")+ ~ ASCII_ALPHANUMERIC) | ASCII_ALPHA) ~ ("_" | "." | ASCII_ALPHANUMERIC)* }
symbol = @{ ! (":" | "{" | "}" | "(" | ")" | "-" | ">" | "_" | "'") ~ ANY }

"#]
struct DfaParser;

#[derive(Debug)]
pub enum ParseError {
    SyntaxError(String),
    ValidationError(Vec<DfaValidationError<char>>),
}

pub fn parse(input: &str) -> Result<Dfa<char>, ParseError> {
    let ptree = DfaParser::parse(Rule::dfa, input)
        .map_err(|e| ParseError::SyntaxError(format!("{e}")))?;

    let name2state = Map::<&str, State>::new();
    let alphabet: Set<char> = todo!();
    let start = todo!();
    let accepting = todo!();
    let trans = todo!();
    let state_names = name2state.into_iter().map(|(name, state)| (state, name.to_owned())).collect();

    Dfa::try_new(alphabet, trans, start, accepting, state_names).map_err(ParseError::ValidationError)
}
