//! The abstract syntax for the imperative automaton language

pub use std::collections::{HashMap as Map, HashSet as Set};
use std::ops::Range;

use derive_more::Display;

/// An identifier.
pub type Id = internment::Intern<String>;

/// create an Id from a &str.
pub fn id(name: &str) -> Id {
    Id::new(name.to_string())
}

/// Symbols that the program operates on
#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Display, Clone, Copy)]
#[display("{}", self.0)]
pub struct Symbol(pub char);

/// An automaton program.  Everything except the helpers describe the main
/// program.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Program {
    /// The set of symbols the input can contain.
    pub alphabet: Set<Symbol>,
    /// Helper functions.
    pub helpers: Map<Id, Function>,
    /// Local variables.
    pub locals: Map<Id, Type>,
    /// Statements that are run in the beginning.
    pub start: Block,
    /// Actions to execute for every input symbol.
    ///
    /// The optional `Id` is the name for the current input symbol.
    pub action: (Option<Id>, Block),
    /// Acceptance condition
    pub accept: Expr,
}

/// Statements that affect the local variables.
#[derive(PartialEq, Eq, Debug, Clone)]
#[allow(missing_docs)]
pub enum Stmt {
    Assign(Id, Expr),
    If {
        cond: Expr,
        true_branch: Block,
        false_branch: Block,
    },
}

/// A sequence of statements
pub type Block = Vec<Stmt>;

/// Expressions.  Unlike statements, expressions evaluate to a value and they do
/// not change the state of the local variables.
#[derive(PartialEq, Eq, Debug, Clone)]
#[allow(missing_docs)]
pub enum Expr {
    /// Function call.
    Call { callee: Id, args: Vec<Expr> },
    /// A literal numeric value of a given type
    Num(i64, Type),
    /// A Boolean literal.
    Bool(bool),
    /// A literal symbol.
    Sym(char),
    /// A variable.
    Var(Id),
    /// Binary operation
    BinOp {
        lhs: Box<Expr>,
        op: BOp,
        rhs: Box<Expr>,
    },
    /// Unary operation
    UOp { op: UOp, inner: Box<Expr> },
    /// Casting from one numerical type to another.
    Cast {
        inner: Box<Expr>,
        typ: Type,
        overflow: Overflow,
    },
    /// Pattern matching
    Match {
        scrutinee: Box<Expr>,
        cases: Vec<Case>,
    },
}

/// Pattern match cases.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Case {
    /// The pattern (value or variable) being matched.
    pub pattern: Pattern,
    /// Additional boolean condition on the pattern.
    pub guard: Expr,
    /// Right-hand side of the pattern match case.
    pub result: Expr,
}

/// Patterns
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Pattern {
    /// Match any value and create given variable.
    Var(Id),
    /// Match given numeric constant.
    Num(i64),
    /// Match given boolean constant.
    Bool(bool),
    /// Match given symbol.
    Sym(Symbol),
}

/// Binary operators.
///
/// Some concrete syntax operators are not here because we can represent them
/// using other operators.  For example, `x > y` can be written as `x < y` so we
/// don't need a `>` operator.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum BOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Shl,
    Shr,
    Lt,
    Lte,
    Eq,
    Ne,
    And,
    Or,
}

/// Unary operators.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum UOp {
    Negate,
    Not,
}

/// Overflow handling.
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[allow(missing_docs)]
pub enum Overflow {
    Wraparound,
    Saturate,
    Fail,
}

/// A pure helper function.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Function {
    /// Parameters of the function.
    pub params: Vec<(Id, Type)>,
    /// Return type of the function.
    pub ret_typ: Type,
    /// Return value of the function.
    pub body: Expr,
}

/// Types.
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    /// The boolean type
    BoolT,
    /// A numerical type with given bounds
    NumT(Range<i64>),
    /// The symbol type
    SymT,
}
