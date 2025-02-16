//! The abstract syntax for the imperative automaton language

use std::ops::Range;
pub use std::collections::{HashMap as Map, HashSet as Set};

use derive_more::Display;

/// An identifier.
pub type Id = internment::Intern<String>;

/// create an Id from a &str.
pub fn id(name: &str) -> Id {
    Id::new(name.to_string())
}

/// Symbols that the program operates on
#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Display)]
#[display("{}", self.0)]
pub struct Symbol(char);

/// An automaton program.  Everything except the helpers describe the main
/// program.
struct Program {
    /// The set of symbols the input can contain.
    alphabet: Set<Symbol>,
    /// Helper functions.
    helpers: Map<Id, Function>,
    /// Local variables.
    locals: Map<Id, Type>,
    /// Statements that are run in the beginning.
    start: Stmt,
    /// Actions to exectue for every symbol.
    action: Stmt,
    /// Acceptance condition
    accept: Expr,
}

/// Statements that affect the local variables.
enum Stmt {
    Assign(Id, Expr),
    If {
        cond: Expr,
        true_branch: Block,
        false_branch: Block,
    },
    // TODO: rest
}

/// A sequence of statements
struct Block(Vec<Stmt>);

/// Expressions.  Unlike statements, expressions evaluate to a value and they do
/// not change the state of the local variables.
enum Expr {
    /// Function call.
    Call {
        callee: Id,
        args: Vec<Expr>,
    },
    /// A literal numeric value of a given type
    Num(i64, Type),
    /// A Boolean literal
    Bool(bool),
    /// A literal symbol
    Sym(char),
    // TODO: rest
}

/// A pure helper function.
struct Function {
    params: Vec<Id>,
    body: Expr,
}

/// Types.
enum Type {
    /// The boolean type
    BoolT,
    /// A numerical type with given bounds
    NumT(Range<i64>),
}
