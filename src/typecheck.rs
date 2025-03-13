use crate::syntax::*;
use thiserror::Error;

/// Errors that can occur during type checking
#[derive(Error, Debug)]
pub enum TypeError {
    /// Type mismatch between expected and actual types
    #[error("Type mismatch: expected {expected:?}, found {actual:?}")]
    TypeMismatch {
        /// The type that was expected
        expected: Type,
        /// The type that was actually found
        actual: Type,
    },
    /// Variable not found in the environment
    #[error("Undefined variable: '{0}'")]
    UndefinedVariable(Id),
    /// Function not found
    #[error("Undefined function: '{0}'")]
    UndefinedFunction(Id),
    /// Symbol not in alphabet
    #[error("Symbol '{0}' is not in the alphabet")]
    SymbolNotInAlphabet(char),
}

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
