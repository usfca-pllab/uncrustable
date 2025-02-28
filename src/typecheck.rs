//! The type checker

use crate::syntax::*;

enum TypeError {
    TypeMismatch,
}

// TODO how to sturcture the type checker
// TODO Casting with more concrete examples


fn typeck_expr(expr: &Expr) -> Result<(), TypeError> {
    todo!()
}

fn typeck_stmt(stmt: &Stmt) -> Result<(), TypeError> {
    todo!()
}

fn typeck(program: &Program) -> Result<(), TypeError> {
    todo!()
}

