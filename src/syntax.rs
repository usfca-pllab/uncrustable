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
pub struct Program {
    /// The set of symbols the input can contain.
    alphabet: Set<Symbol>,
    /// Helper functions.
    helpers: Map<Id, Function>,
    /// Local variables.
    locals: Map<Id, Type>,
    /// Statements that are run in the beginning.
    start: Stmt,
    /// Actions to execute for every input symbol.
    ///
    /// The optional `Id` is the name for the current input symbol.
    action: (Option<Id>, Stmt),
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
    UOp {
	op: BOp,
	inner: Box<Expr>,
    },
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
    }
}

/// Pattern match cases.
struct Case {
    pattern: Pattern,
    guard: Expr,
    result: Expr,
}

/// Patterns
enum Pattern {
    Var(Id),
    Num(i64),
    Bool(bool),
    Sym(Symbol),
}

/// Binary operators.
///
/// Some concrete syntax operators are not here because we can represent them
/// using other operators.  For example, `x > y` can be written as `x < y` so we
/// don't need a `>` operator.
enum BOp {
    Add, Sub, Mul, Div, Rem, Shl, Shr, Lt, Lte, Eq, Ne, And, Or,
}

/// Unary operators.
enum UOp {
    Negate, Not,
}

/// Overflow handling.
enum Overflow {
    Wraparound,
    Saturate,
    Fail,
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
    /// The symbol type
    SymT,
}
