//! The DFA parser for mentor-like syntax.
//!
//! The syntax for DFA descriptions is:
//!
//! ```text
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

dfa = { SOI ~ alphabet_decl ~ start_decl ~ accepting_decl ~ (rule_decl)* ~ &EOI }

alphabet_decl = { "alphabet:" ~ "{" ~ symbol ~ ("," ~ symbol)* ~ "}"  }
accepting_decl = { "accepting:" ~ "{" ~ (state ~ ("," ~ state)*)? ~ "}"  }
start_decl = { "start:" ~ state  }
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

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::SyntaxError(s) => write!(f, "Syntax error:\n{}", s),
            ParseError::ValidationError(errors) => {
                write!(f, "Validation error(s):")?;
                for error in errors {
                    write!(f, "\n{:?}", error)?;
                }
                Ok(())
            }
        }
    }
}

pub fn parse(input: &str) -> Result<Dfa<char>, ParseError> {
    let mut dfa_tree = DfaParser::parse(Rule::dfa, input)
        .map_err(|e| ParseError::SyntaxError(format!("{e}")))?
        .next()
        .unwrap()
        .into_inner();

    let mut name2state = Map::<String, State>::new();
    let mut get_state = |name: &str| {
        *name2state
            .entry(name.to_owned())
            .or_insert_with(State::fresh)
    };

    let alphabet: Set<char> = dfa_tree
        .next()
        .unwrap()
        .into_inner()
        .map(|p| {
            let c = p.as_str().chars().next().unwrap();
            if c == '\\' {
                p.as_str().chars().nth(1).unwrap()
            } else {
                c
            }
        })
        .collect();
    let start = get_state(dfa_tree.next().unwrap().into_inner().as_str());
    println!("{:?}", dfa_tree.clone().next().unwrap().into_inner());
    let accepting: Set<State> = dfa_tree
        .next()
        .unwrap()
        .into_inner()
        .map(|p| get_state(p.as_str()))
        .collect();
    println!("{:?}", accepting);
    let trans = dfa_tree
        .map(|p| {
            let mut children = p.into_inner();
            let state = get_state(children.next().unwrap().as_str());
            (
                state,
                children
                    .map(|p| {
                        let mut p = p.into_inner();
                        let symbol = p.next().unwrap().as_str().chars().next().unwrap();
                        let next = get_state(p.next().unwrap().as_str());
                        (symbol, next)
                    })
                    .collect(),
            )
        })
        .collect();
    let state_names = name2state
        .into_iter()
        .map(|(name, state)| (state, name))
        .collect();
    eprintln!("{:?}", state_names);

    Dfa::try_new(alphabet, trans, start, accepting, state_names)
        .map_err(ParseError::ValidationError)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = r#"alphabet: { a, b }
                start: q0
                accepting: { q1 }
                q0 (a -> q1) (b -> q0)
                q1 (a -> q1) (b -> q0)
                "#;
        let dfa = parse(input).unwrap();
        let name2state = dfa
            .state_names
            .iter()
            .map(|(s, n)| (n.clone(), *s))
            .collect::<Map<_, _>>();
        assert_eq!(dfa.alphabet, Set::from(['a', 'b']));
        assert_eq!(dfa.start, name2state["q0"]);
        assert_eq!(dfa.accepting, Set::from([name2state["q1"]]));
        assert_eq!(
            dfa.trans,
            Map::from([
                (
                    name2state["q0"],
                    Map::from([('a', name2state["q1"]), ('b', name2state["q0"]),])
                ),
                (
                    name2state["q1"],
                    Map::from([('a', name2state["q1"]), ('b', name2state["q0"]),])
                ),
            ])
        );
    }

    #[test]
    fn test_parse2() {
        let input = r#"alphabet: { a, b }
                start: q0
                accepting: { q0, q1 }
                q0 (a -> q1) (b -> q0)
                q1 (a -> q1) (b -> q0)
                "#;
        let dfa = parse(input).unwrap();
        let name2state = dfa
            .state_names
            .iter()
            .map(|(s, n)| (n.clone(), *s))
            .collect::<Map<_, _>>();
        assert_eq!(dfa.alphabet, Set::from(['a', 'b']));
        assert_eq!(dfa.start, name2state["q0"]);
        assert_eq!(
            dfa.accepting,
            Set::from([name2state["q0"], name2state["q1"]])
        );
        assert_eq!(
            dfa.trans,
            Map::from([
                (
                    name2state["q0"],
                    Map::from([('a', name2state["q1"]), ('b', name2state["q0"]),])
                ),
                (
                    name2state["q1"],
                    Map::from([('a', name2state["q1"]), ('b', name2state["q0"]),])
                ),
            ])
        );
    }
}
