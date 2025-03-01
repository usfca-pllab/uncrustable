//! The DFA parser for mentor-like syntax.
//!
//! The syntax for DFA descriptions is:
//!
//! ```ignore
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
use pest::Parser;

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

    let mut name2state = Map::<String, State>::new();
    let mut get_state = |name: &str| name2state.entry(name.to_owned()).or_insert_with(State::fresh).clone();
    
    let alphabet: Set<char> =
        ptree.clone().next().unwrap().into_inner().next().unwrap().into_inner().map(|p| {
            let c = p.as_str().chars().next().unwrap();
            if c == '\\' {
                p.as_str().chars().nth(1).unwrap()
            } else {
                c
            }
        }).collect();
    let start = get_state(ptree.clone().next().unwrap().into_inner().next().unwrap().into_inner().next().unwrap().as_str());
        let accepting: Set<State> = ptree.clone().next().unwrap().into_inner().next().unwrap().into_inner().next().unwrap().into_inner().map(|p| {
            get_state(p.as_str())
        }).collect();
    let trans = ptree.clone().next().unwrap().into_inner().skip(3).map(|p| {
        let state = get_state(p.as_str());
        (state, p.into_inner().map(|p| {
            let mut p = p.into_inner();
            let symbol = p.next().unwrap().as_str().chars().next().unwrap();
            let next = get_state(p.next().unwrap().as_str());
            (symbol, next)
        }).collect())
    }).collect();
    let state_names = name2state.into_iter().map(|(name, state)| (state, name)).collect();

    Dfa::try_new(alphabet, trans, start, accepting, state_names).map_err(ParseError::ValidationError)
}

#[cfg(test)]
mod tests {
        use super::*;
        
        #[test]
        fn test_parse() {
                let input = r#"
                alphabet: { a, b }
                start: q0
                accepting: { q1 }
                q0 (a -> q1) (b -> q0)
                q1 (a -> q1) (b -> q0)
                "#;
                let dfa = parse(input).unwrap();
                assert_eq!(dfa.alphabet, Set::from(['a', 'b']));
                assert_eq!(dfa.start, State(0));
                assert_eq!(dfa.accepting, Set::from([State(1)]));
                assert_eq!(dfa.trans, Map::from([
                        (State(0), Map::from([
                                ('a', State(1)),
                                ('b', State(0)),
                        ])),
                        (State(1), Map::from([
                                ('a', State(1)),
                                ('b', State(0)),
                        ])),
                ]));
            assert_eq!(dfa.state_names, Map::from([
                (State(0), "q0".to_owned()),
                (State(1), "q1".to_owned()),
            ]));
        }
}
