//! The type checker

use crate::syntax::*;

enum TypeError {
    TypeMismatch,
}

fn typeck(program: &Program) -> Result<(), TypeError> {
    todo!()
}
