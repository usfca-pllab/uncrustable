use crate::syntax::*;
use std::error::Error;
use std::fmt;

/// Errors that can occur during type checking
#[derive(Debug)]
pub enum TypeError {
    /// Type mismatch between expected and actual types
    TypeMismatch {
        /// The type that was expected
        expected: Type,
        /// The type that was actually found
        actual: Type,
    },
    /// Variable not found in the environment
    UndefinedVariable(Id),
    /// Function not found
    UndefinedFunction(Id),
    /// Symbol not in alphabet
    SymbolNotInAlphabet(char),
}

// Implement Display for TypeError
impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeError::TypeMismatch { expected, actual } => {
                write!(
                    f,
                    "Type mismatch: expected {:?}, found {:?}",
                    expected, actual
                )
            }
            TypeError::UndefinedVariable(id) => {
                write!(f, "Undefined variable: '{}'", id)
            }
            TypeError::UndefinedFunction(id) => {
                write!(f, "Undefined function: '{}'", id)
            }
            TypeError::SymbolNotInAlphabet(c) => {
                write!(f, "Symbol '{}' is not in the alphabet", c)
            }
        }
    }
}

// Implement Error for TypeError
impl Error for TypeError {}

/// Type environment mapping variables to their types
type TypeEnv = Map<Id, Type>;

impl Expr {
    /// Type check an expression in a given environment
    pub fn typeck_expr(&self, env: &TypeEnv) -> Result<Type, TypeError> {
        match self {
            // Variables are always of the type of the variable in the environment
            Expr::Var(id) => {
                // Get the type of the variable from the environment
                env.get(id)
                    .cloned()
                    .ok_or(TypeError::UndefinedVariable(*id))
            }
            // Boolean literals are always of type BoolT
            Expr::Bool(_) => Ok(Type::BoolT),
            // Numeric literals are always of the type they are given
            Expr::Num(_n, t) => Ok(t.clone()),
            // Function calls are always of the type of the function
            Expr::Sym(c) => {
                todo!()
            }
            // Binary operations are always of the type of the left hand side
            Expr::BinOp { lhs, op, rhs } => {
                todo!()
            }
            // Unary operations are always of the type of the inner expression
            Expr::UOp { op, inner } => {
                todo!()
            }
            // Casting is always of the type of the inner expression
            Expr::Cast {
                inner,
                typ,
                overflow,
            } => {
                todo!()
            }
            // Pattern matching is always of the type of the scrutinee
            Expr::Call { callee, args } => {
                todo!()
            }
            // Pattern matching is always of the type of the scrutinee
            Expr::Match { scrutinee, cases } => {
                todo!()
            }
        }
    }
}
