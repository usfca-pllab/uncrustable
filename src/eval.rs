//! The interpreter

use crate::syntax::*;
use std::collections::HashMap as Map;

enum RuntimeError {
    InvalidInput,
}

fn eval(program: &Program, input: &str) -> Result<bool, RuntimeError> {
    // create initial state

    // consume each symbol

    // evaluate the final condition
    Ok(false)
}

// define the state type
enum Value {}

type Env = Map<Id, Value>;

// eval. expr.

fn eval_spr(exp: &Expr, env: &Env) -> Result<Value, RuntimeError> {
    Err(RuntimeError::InvalidInput)
}

// eval. stmt.
