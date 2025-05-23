use crate::syntax::*;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::fmt;
use std::ops::Range;
use thiserror::Error;

/// Errors that can occur during type checking
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// Invalid expression that cannot be evaluated
    #[error("Invalid expression")]
    InvalidExpression,
    /// Found a non-bool statement
    #[error("Invalid statement")]
    InvalidStatement,
    /// Found division by zero
    #[error("Division by zero")]
    DivisionbyZero,
    /// Found non-valid type
    #[error("Type error")]
    TypeError,
    /// Found non-valid operand that cannot be evaluated
    #[error("Invalid operand")]
    InvalidOperand,
    /// Found modulus by zero
    #[error("Modulus by zero")]
    ModulusByZero,
    /// Num out of expected range, unable to cast
    #[error("Cast fail: out of expected range")]
    OutOfRange,
}

/// abstraction for values making up expressions
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    /// bool Value
    Bool(bool),
    /// num Value includes i64 and range
    Num(i64, Range<i64>),
    /// sym Value of Symbol type
    Sym(Symbol),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Value::*;
        Some(match (self, other) {
            // Bool comparison: false < true (default Rust behavior)
            (Bool(a), Bool(b)) => a.cmp(b),

            // Bool < Num < Sym
            (Bool(_), Num(_, _)) | (Bool(_), Sym(_)) => Ordering::Less,
            (Num(_, _), Bool(_)) | (Sym(_), Bool(_)) => Ordering::Greater,

            // Num comparison: by value, then range.start, then range.end
            (Num(a_val, a_rng), Num(b_val, b_rng)) => a_val
                .cmp(b_val)
                .then(a_rng.start.cmp(&b_rng.start))
                .then(a_rng.end.cmp(&b_rng.end)),

            // Num < Sym
            (Num(_, _), Sym(_)) => Ordering::Less,
            (Sym(_), Num(_, _)) => Ordering::Greater,

            // Sym comparison
            (Sym(a), Sym(b)) => a.cmp(b),
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Num(n, range) => write!(f, "{} as [{}..{}]", n, range.start, range.end),
            Value::Sym(sym) => write!(f, "{}", sym),
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

/// evaluate one action in a program
pub fn eval_action(program: &Program, env: &mut Env, sym: &Symbol) -> Result<(), RuntimeError> {
    if let Some(id) = &program.action.0 {
        env.insert(id.clone(), Value::Sym(*sym));
    }

    // Evaluate each statement in the action
    for stmt in &program.action.1 {
        eval_stmt(stmt, env, program)?;
    }

    Ok(())
}

// map of variable names to values
type Env = BTreeMap<Id, Value>;

/// set up initial eval env
pub fn init_env(program: &Program) -> Env {
    let mut env = Env::new();
    let alph: BTreeSet<Symbol> = program.alphabet.iter().cloned().collect();
    let first_symbol = Value::Sym(alph.iter().next().unwrap().clone());

    // insert locals into env
    for (id, typ) in &program.locals {
        let value = match typ {
            // defaults for now are zero values
            Type::BoolT => Value::Bool(false),

            //  default to be beginning of range
            Type::NumT(range) => Value::Num(range.start, range.clone()),

            //  default to empty char
            Type::SymT => first_symbol.clone(),
        };

        env.insert(id.clone(), value);
    }

    env
}

fn eval(program: &Program, input: &str) -> Result<(bool, Env), RuntimeError> {
    // create initial state
    let mut env = init_env(program);
    for stmt in &program.start {
        eval_stmt(stmt, &mut env, &program)?;
    }

    for sym in input.chars() {
        eval_action(program, &mut env, &Symbol(sym))?;
    }

    // evaluate accept
    let accept = eval_expr(&program.accept, &env, &program)?;

    match accept {
        Value::Bool(b) => Ok((b, env)),
        _ => Err(RuntimeError::TypeError),
    }
}

fn cast(v: i64, range: Range<i64>, overflow: Overflow) -> Result<Value, RuntimeError> {
    let lower = range.start;
    let upper = range.end;
    let val = match overflow {
        Overflow::Saturate => {
            if v >= upper {
                Ok(Value::Num(upper - 1, range))
            } else if v < lower {
                Ok(Value::Num(lower, range))
            } else {
                Ok(Value::Num(v, range))
            }
        }
        Overflow::Wraparound => Ok(Value::Num(
            (v - lower).abs() % (upper - lower) + lower,
            Range {
                start: lower,
                end: upper,
            },
        )),
        Overflow::Fail => {
            if v >= upper {
                Err(RuntimeError::OutOfRange)
            } else if v < lower {
                Err(RuntimeError::OutOfRange)
            } else {
                Ok(Value::Num(v, range))
            }
        }
    };
    return val;
}

/// evaluate expression for given program, return value
pub fn eval_expr(expr: &Expr, env: &Env, program: &Program) -> Result<Value, RuntimeError> {
    match expr {
        Expr::Num(n, Type::NumT(range)) => cast(*n, range.clone(), Overflow::Wraparound),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Sym(symbol) => Ok(Value::Sym(Symbol(*symbol))),

        Expr::Var(id) => Ok(env.get(id).unwrap().clone()),

        Expr::BinOp { lhs, op, rhs } => {
            let left = eval_expr(lhs, env, &program)?;
            let right = eval_expr(rhs, env, &program)?;

            match (left, right) {
                (Value::Num(l, l_range), Value::Num(r, _)) => match op {
                    // numerical return
                    BOp::Add => cast(l + r, l_range, Overflow::Wraparound),
                    BOp::Sub => cast(l - r, l_range, Overflow::Wraparound),
                    BOp::Mul => cast(l * r, l_range, Overflow::Wraparound),
                    BOp::Div => {
                        if r == 0 {
                            return Err(RuntimeError::DivisionbyZero);
                        } else {
                            cast(l / r, l_range, Overflow::Wraparound)
                        }
                    }
                    BOp::Rem => {
                        if r == 0 {
                            Err(RuntimeError::ModulusByZero)
                        } else {
                            cast(l % r, l_range, Overflow::Wraparound)
                        }
                    }
                    BOp::Shl => cast(l << r, l_range, Overflow::Wraparound),
                    BOp::Shr => cast(l >> r, l_range, Overflow::Wraparound),

                    // boolean return
                    BOp::Lt => Ok(Value::Bool(l < r)),
                    BOp::Lte => Ok(Value::Bool(l <= r)),
                    BOp::Eq => Ok(Value::Bool(l == r)),
                    BOp::Ne => Ok(Value::Bool(l != r)),

                    _ => Err(RuntimeError::InvalidOperand),
                },

                (Value::Bool(l), Value::Bool(r)) => match op {
                    BOp::And => Ok(Value::Bool(l && r)),
                    BOp::Or => Ok(Value::Bool(l || r)),
                    BOp::Eq => Ok(Value::Bool(l == r)),
                    BOp::Ne => Ok(Value::Bool(l != r)),

                    _ => Err(RuntimeError::InvalidOperand),
                },
                (Value::Sym(l), Value::Sym(r)) => match op {
                    BOp::Eq => Ok(Value::Bool(l == r)),
                    BOp::Ne => Ok(Value::Bool(l != r)),

                    _ => Err(RuntimeError::InvalidOperand),
                },
                _ => Err(RuntimeError::TypeError),
            }
        }

        Expr::UOp { op, inner } => {
            let left = eval_expr(inner, env, &program)?;

            match (left, op) {
                (Value::Num(l, range), UOp::Negate) => cast(-l, range, Overflow::Wraparound),
                (Value::Bool(b), UOp::Not) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::TypeError),
            }
        }

        Expr::Cast {
            inner,
            typ,
            overflow,
        } => {
            let Type::NumT(cast_t) = typ else {
                return Err(RuntimeError::TypeError);
            };

            let val = eval_expr(inner, env, &program)?;

            match val {
                Value::Num(num, _) => cast(num, cast_t.clone(), overflow.clone()),
                // can only cast ints
                _ => Err(RuntimeError::TypeError),
            }
        }

        Expr::Match { scrutinee, cases } => {
            let raw_val = eval_expr(&scrutinee, env, &program)?;
            let mut env_scope = Env::new();
            for case in cases {
                let case_env = match (case.pattern, raw_val.clone()) {
                    (Pattern::Var(id), _) => {
                        env_scope.insert(id, raw_val.clone());
                        env_scope.clone()
                    }
                    (Pattern::Bool(bp), Value::Bool(bv)) if bp == bv => env.clone(),
                    (Pattern::Num(np), Value::Num(nv, _)) if np == nv => env.clone(),
                    (Pattern::Sym(sp), Value::Sym(sv)) if sp == sv => env.clone(),
                    _ => continue,
                };
                let g = eval_expr(&case.guard, &case_env, &program)?;
                if let Value::Bool(b) = g {
                    if b {
                        return eval_expr(&case.result, &case_env, &program);
                    }
                } else {
                    return Err(RuntimeError::TypeError);
                }
            }
            return Err(RuntimeError::TypeError);
        }

        Expr::Call { callee, args } => {
            let mut env_args = Env::new();
            let function = program.helpers.get(callee).unwrap().clone();

            for (arg, param) in args.iter().zip(function.params.iter()) {
                let arg_val = eval_expr(arg, env, program)?;
                env_args.insert(param.0, arg_val);
            }
            return eval_expr(&function.body, &env_args, program);
        }

        _ => Err(RuntimeError::InvalidExpression),
    }
}

/// evaluate statement for a given program
pub fn eval_stmt(stmt: &Stmt, env: &mut Env, program: &Program) -> Result<Value, RuntimeError> {
    match stmt {
        Stmt::Assign(id, expr) => {
            let value = eval_expr(expr, env, &program)?;
            env.insert(id.clone(), value.clone());
            Ok(value)
        }

        Stmt::If {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond_val = eval_expr(cond, env, &program)?;
            match cond_val {
                Value::Bool(true) => {
                    for stmt in true_branch {
                        eval_stmt(stmt, env, program)?;
                    }
                    Ok(cond_val)
                }

                Value::Bool(false) => {
                    for stmt in false_branch {
                        eval_stmt(stmt, env, program)?;
                    }
                    Ok(cond_val)
                }

                _ => Err(RuntimeError::InvalidStatement),
            }
        }
    }
}

/// evaluate a program, return result of evaluation
pub fn evaluate(program: &Program, input: &str) -> Result<bool, RuntimeError> {
    match eval(program, input) {
        Ok((result, _)) => Ok(result),
        Err(e) => Err(e),
    }
}

// TEST -----------------------------------------------------------------------------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    #[test]
    // Simple Local Assignment
    fn test_assign() {
        let input = r#"
                alphabet: {'a'}
                let x: int[4];
                on input y {
                    x = 3;   
                }
                accept if x == 3
            "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("x")), Some(&Value::Num(3, 3..4)));
    }

    #[test]
    fn test_binop_1() {
        let input = r#"
            alphabet: {'a'}
            on input y {
                x = 2 as int[4] wraparound + 1 as int[4] wraparound;
                z = 2 as int[4] wraparound - 1 as int[4] wraparound;
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("x")), Some(&Value::Num(3, 0..4)));
        assert_eq!(env.get(&id("z")), Some(&Value::Num(1, 0..4)));
    }

    #[test]
    // Boolean BinOp - Relational Operators
    fn test_binop_2() {
        let input = r#"
            alphabet: {'e'}
            on input y {
                v = 10 < 3;
                x = 3 < 9;
                y = 2 < 2;
                z = 2 <= 2;
                a = 1 <= 2;
                b = 5 <= 2;
                w = 4 == 1;
                c = 4 == 4;
            }
            accept if v == false
        "#;
        let program = parse(input).unwrap();
        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("v")), Some(&Value::Bool(false)));
        assert_eq!(env.get(&id("x")), Some(&Value::Bool(true)));
        assert_eq!(env.get(&id("y")), Some(&Value::Bool(false)));
        assert_eq!(env.get(&id("z")), Some(&Value::Bool(true)));
        assert_eq!(env.get(&id("a")), Some(&Value::Bool(true)));
        assert_eq!(env.get(&id("b")), Some(&Value::Bool(false)));
        assert_eq!(env.get(&id("w")), Some(&Value::Bool(false)));
        assert_eq!(env.get(&id("c")), Some(&Value::Bool(true)));
    }

    #[test]
    // Numerical BinOp - Remainder, Shifts (Left and Right)
    fn test_binop_3() {
        let input = r#"
            alphabet: {'e'}
            let v: int[10];
            let z: int[10];
            let w: int[10];
            on input y {
                v = 9 as int[10] wraparound % 3 as int[10] wraparound;
                z = 2 as int[10] wraparound << 1 as int[10] wraparound;
                w = 4 as int[10] wraparound >> 1 as int[10] wraparound;
            }
            accept if v == 1
        "#;
        let program = parse(input).unwrap();
        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("v")), Some(&Value::Num(0, 0..10)));
        assert_eq!(env.get(&id("z")), Some(&Value::Num(4, 0..10)));
        assert_eq!(env.get(&id("w")), Some(&Value::Num(2, 0..10)));
    }

    #[test]
    // Boolean BinOp - And / Or
    fn test_binop_4() {
        let input = r#"
            alphabet: {'d'}
            let z: bool;
            let w: bool;
            on input y {
                z = true && false;
                w = true || false;
            }
            accept if z == false
        "#;
        let program = parse(input).unwrap();
        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("z")), Some(&Value::Bool(false)));
        assert_eq!(env.get(&id("w")), Some(&Value::Bool(true)));
    }

    #[test]
    // Unary Operations - Negate and Not
    fn test_unop() {
        let input = r#"
            alphabet: {'a'}
            let y: int[-5..5];
            let w: bool;
            y = (2 as int[-5..5] saturate);
            w = !true;
            on input a {
                w = false;
            }
            accept if w == false
        "#;

        let program = parse(input).unwrap();

        let input = "";
        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("y")), Some(&Value::Num(2, -5..5)));
        assert_eq!(env.get(&id("w")), Some(&Value::Bool(false)));
    }

    #[test]
    // Cast, wraparound
    fn test_cast_wraparound() {
        let input = r#"
            alphabet: {'a'}
            let y: int[-5..5];
            let a: int[6..10];
            let w: bool;
            y = (2 as int[3..5] wraparound);
            a = (7 as int[0..1] wraparound);
            on input a {
                w = false;
            }
            accept if w == false
        "#;
        let program = parse(input).unwrap();
        let input = "";

        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("y")), Some(&Value::Num(4, 3..5)));
        assert_eq!(env.get(&id("a")), Some(&Value::Num(0, 0..1)));
    }

    #[test]
    // Cast, saturate
    fn test_cast_saturate() {
        let input = r#"
            alphabet: {'a'}
            let y: int[-5..5];
            let a: int[6..10];
            let w: bool;
            y = (2 as int[3..5] saturate);
            a = (7 as int[0..1] saturate);
            on input a {
                w = false;
            }
            accept if w == false
        "#;
        let program = parse(input).unwrap();
        let input = "";

        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("y")), Some(&Value::Num(3, 3..5)));
        assert_eq!(env.get(&id("a")), Some(&Value::Num(0, 0..1)));
    }

    #[test]
    // Cast, fail
    fn test_cast_fail() {
        let input = r#"
            alphabet: {'a'}
            let y: int[-5..5];
            let a: int[6..10];
            let w: bool;
            y = (2 as int[3..5] fail);
            a = (7 as int[0..1] fail);
            on input a {
                w = false;
            }
            accept if w == false
        "#;
        let program = parse(input).unwrap();
        let input = "";

        let result = eval(&program, input);

        match result {
            Err(RuntimeError::OutOfRange) => (),
            _ => panic!("Expected OutOfRange error but got: {:?}", result),
        }
    }

    #[test]
    // Match
    fn test_match1() {
        let input = r#"
            alphabet: {'a'}
            let x: int[0..4];
            let w: int[0..3];
            let z: bool;
            x = 2 as int[4] wraparound + 1 as int[4] wraparound;
            w = match x {
                3 -> 1
                2 if true -> 2
            };
            on input y {
                z = false;
            }
            accept if z == true
        "#;
        let program = parse(input).unwrap();
        let input = "";

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("x")), Some(&Value::Num(3, 0..4)));
        assert_eq!(env.get(&id("w")), Some(&Value::Num(1, 1..2)));
    }

    #[test]
    // Call
    fn test_call() {
        let input = r#"
            alphabet: {'a'}
            fn add(a: int[0..4], b: int[0..4]) -> int[0..4] = a + b
            let x: int[4];
            on input y {
                    x = add(1 as int[0..4], 2 as int[0..4]);
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("x")), Some(&Value::Num(3, 0..4)));
    }

    #[test]
    // If
    fn test_if() {
        let input = r#"
            alphabet: {'a'}
            fn add(a: int[3], b: int[0..3]) -> int[0..3] = a + b
            let x: int[3];
            on input y {
                x = add(1, 2);
                if x == 4 {
                    y = 1;				
                } else {
                    y = 2;
                }    
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("y")), Some(&Value::Num(2, 2..3)));
    }

    #[test]
    // If
    fn test_if2() {
        let input = r#"
            alphabet: {'a'}
            let x: int[3];
            on input y {
                x = 3;
                if x < 4 {
                    y = 1;				
                } else {
                    y = 2;
                }    
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("y")), Some(&Value::Num(1, 1..2)));
    }

    #[test]
    // If
    fn test_if3() {
        let input = r#"
            alphabet: {'a'}
            let x: int[3];
            on input y {
                x = 6;
                if x < 4 {
                    y = 1;				
                } else {
                    y = 2;
                }    
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("y")), Some(&Value::Num(2, 2..3)));
    }

    #[test]
    // If
    fn test_if4() {
        let input = r#"
            alphabet: {'a'}
            let x: int[3];
            on input y {
                x = 4;
                if x <= 4 {
                    y = 1;				
                } else {
                    y = 2;
                }    
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("y")), Some(&Value::Num(1, 1..2)));
    }

    #[test]
    fn test_match2() {
        let input = r#"
            alphabet: {'a'}
            fn add(a: int[3], b: int[0..3]) -> int[0..3] = a + b
            let x: int[3];
            on input y {
                    x = 3 + - 4 as int[3] wraparound;
                    x = 3 + 4 as int[3] wraparound;
                    if x < 3 {
                        y = 'a';
                    } else {
                        x = match y {
                                'a' -> 1
                                x if true -> 2
                        };
                    }
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let (_result, env) = eval(&program, input).unwrap();
        assert_eq!(env.get(&id("x")), Some(&Value::Num(2, 2..3)));
    }

    #[test]
    // public call 1
    fn test_pub_1() {
        let input = r#"
                alphabet: {'a'}
                let x: int[4];
                on input y {
                    x = 3;   
                }
                accept if x == 3
            "#;
        let program = parse(input).unwrap();

        let result = evaluate(&program, input);
        match result {
            Ok(true) => {}
            Ok(false) => panic!("Expected Ok(true), but got Ok(false)"),
            Err(e) => panic!("Expected Ok(true), but got Err({:?})", e),
        }
    }

    #[test]
    // public call 2
    fn test_pub_2() {
        let input = r#"
            alphabet: {'a'}
            fn add(a: int[0..4], b: int[0..4]) -> int[0..4] = a + b
            let x: int[4];
            on input y {
                    x = add(1 as int[0..4], 2 as int[0..4]);
            }
            accept if x == 3
        "#;
        let program = parse(input).unwrap();

        let result = evaluate(&program, input);
        match result {
            Ok(true) => {}
            Ok(false) => panic!("Expected Ok(true), but got Ok(false)"),
            Err(e) => panic!("Expected Ok(true), but got Err({:?})", e),
        }
    }

    #[test]
    fn test_div_0() {
        let input = r#"
            alphabet: {'a'}
            let x: int[2..4];
            on input y {
                x = 3 / 0; 
            }
            accept if x == 2
        "#;
        let program = parse(input).unwrap();

        let retval = eval(&program, "a");

        if retval.is_ok() {
            panic!("Expected DivisionbyZero error but got: {:?}", retval);
        } else {
            match retval {
                Err(RuntimeError::DivisionbyZero) => (),
                _ => {
                    panic!("Expected DivisionbyZero error but got: {:?}", retval);
                }
            }
        }
    }

    #[test]
    fn test_div3() {
        let input = r#"
            alphabet: { '0', '1' }
            fn char_to_bit(c: sym) -> int[0..3] = match c {
                '0' -> 0
                '1' -> 1
            }
            let rem: int[0..3];
            rem = 1;
            on input bit {
                rem = (2 as int[0..3] * rem as int[0..3]) + (char_to_bit(bit) as int[0..3]);
            }
            accept if rem == 0
        "#;
        let program = parse(input).unwrap();

        let result = evaluate(&program, "1");
        match result {
            Ok(true) => {}
            Ok(false) => panic!("Expected Ok(true), but got Ok(false)"),
            Err(e) => panic!("Expected Ok(true), but got Err({:?})", e),
        }
    }
}
