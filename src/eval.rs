use crate::parse::parse;
use crate::syntax::*;
use internment::Intern;
use std::collections::BTreeSet;
use std::collections::HashMap as Map;
use std::ops::Range;
use std::result;
use thiserror::Error;

/// Errors that can occur during type checking
#[derive(Error, Debug)]

enum RuntimeError {
    #[error("Invalid expression")]
    InvalidExpression,
    #[error("Invalid statement")]
    InvalidStatement,
    #[error("Division by zero")]
    DivisionbyZero,
    #[error("Type error")]
    TypeError,
    #[error("Invalid pperand")]
    InvalidOperand,
    #[error("Modulus by zero")]
    ModulusByZero,
    #[error("Cast fail: out of expected range")]
    OutOfRange,
    #[error("Call fail: incorrect args")]
    IncorrectArgs,
}

// abstraction for values making up expressions
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Num(i64, Range<i64>),
    Sym(Symbol),
}

// map of variable names to values
type Env = Map<Id, Value>;

fn init_env(program: &Program) -> Env {
    let mut env = Env::new();
    let alph: BTreeSet<Symbol> = program.alphabet.iter().cloned().collect();

    // insert locals into env
    for (id, typ) in &program.locals {
        let value = match typ {
            // defaults for now are zero values
            Type::BoolT => Value::Bool(false),

            //  default to be beginning of range
            Type::NumT(range) => Value::Num(range.start, range.clone()),

            //  default to empty char
            Type::SymT => Value::Sym(alph.iter().next().unwrap().clone()),
        };

        env.insert(id.clone(), value);
    }

    env
}

fn eval(program: &Program, input: &str) -> Result<(bool, Env), RuntimeError> {
    // create initial state
    let mut env = init_env(program);

    // evaluate the final condition
    for sym in input.chars() {
        match &program.action.0 {
            Some(id) => env.insert(id.clone(), Value::Sym(Symbol(sym))),
            // Value::Sym(Symbol(sym)),
            _ => continue,
        };
    }
    for stmt in &program.action.1 {
        eval_stmt(stmt, &mut env, &program)?;
    }
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
        Overflow::Fail => {
            if v >= upper {
                Err(RuntimeError::OutOfRange)
            } else if v < lower {
                Err(RuntimeError::OutOfRange)
            } else {
                Ok(Value::Num(v, range))
            }
        }
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
            ((v - lower).abs() % (upper - lower) + lower),
            Range {
                start: lower,
                end: upper,
            },
        )),
    };
    return val;
}

// eval. expr.
fn eval_expr(expr: &Expr, env: &Env, program: &Program) -> Result<Value, RuntimeError> {
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
                            Err(RuntimeError::DivisionbyZero)
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
            for case in cases {
                // match pattern, raw_val {
                //
                // }
                let pattern = match case.pattern {
                    Pattern::Bool(b) => Value::Bool(b),
                    Pattern::Num(n) => Value::Num(
                        n,
                        Range {
                            start: n,
                            end: n + 1,
                        },
                    ),
                    Pattern::Sym(s) => Value::Sym(s),
                    Pattern::Var(id) => env.get(&id).unwrap().clone(),
                };
                if raw_val == pattern {
                    let g = eval_expr(&case.guard, env, &program)?;

                    if let Value::Bool(b) = g {
                        if b {
                            return Ok(eval_expr(&case.result, env, &program)?);
                        }
                    } else {
                        return Err(RuntimeError::TypeError);
                    }
                }
            }
            return Err(RuntimeError::TypeError);
        }

        Expr::Call { callee, args } => {
            let mut env_args = Env::new();
            let function = program.helpers.get(callee).unwrap().clone();

            for i in 0..args.len() {
                let mut arg_val = eval_expr(args.get(i).unwrap(), env, program)?;
                let param_type = function.params.get(i).unwrap().clone().1;

                arg_val = match (arg_val, param_type) {
                    (Value::Num(n, _), Type::NumT(t_range)) => {
                        cast(n, t_range, Overflow::Wraparound)?
                    }
                    _ => continue,
                };
                env_args.insert(function.params.get(i).unwrap().clone().0, arg_val);
            }
            return eval_expr(&function.body, &env_args, program);
        }

        _ => Err(RuntimeError::InvalidExpression),
    }
}

// eval. stmt.
fn eval_stmt(stmt: &Stmt, env: &mut Env, program: &Program) -> Result<Value, RuntimeError> {
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

        _ => Err(RuntimeError::InvalidStatement),
    }
}

pub fn evaluate(program: &Program, input: &str) -> Result<bool, RuntimeError> {
	// do a match then return
    match eval(program, input) {
        Ok((result, _)) => Ok(result),
        Err(e) => Err(e),
    }
}

// TEST -----------------------------------------------------------------------------------------------------------------------------------------------

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
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("x")), Some(&Value::Num(3, 3..4)));
}

#[test]
// Simple Addition and Subtraction
fn test_binop_1() {
    let input = r#"
        alphabet: {'a'}
        let x: int[3];
        let z: int[3];
        on input y {
			x = 2 + 1;
			z = 2 - 1;
        }
        accept if x == 3
    "#;
    let program = parse(input).unwrap();
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("x")), Some(&Value::Num(2, 2..3)));
    assert_eq!(env.get(&id("z")), Some(&Value::Num(1, 1..2)));
}

#[test]
// Subtraction test
fn test_binop_sub() {
    let input = r#"
        alphabet: {'a'}
        let x: int[3];
        on input y {
			x = 2-1;
        }
        accept if x == 3
    "#;
    let program = parse(input).unwrap();
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("x")), Some(&Value::Num(1, 1..2)));
}

#[test]
// Boolean BinOp - Relational Operators
fn test_binop_2() {
    let input = r#"
        alphabet: {'e'}
        let v: bool;
        let x: bool;
        let y: bool;
        let z: bool;
        let a: bool;
        let b: bool;
        let w: bool;
        let c: bool;
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
    let (result, env) = eval(&program, input).unwrap();

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
            v = 10 % 3;
            z = 2 << 1;
            w = 4 >> 1;
        }
        accept if v == 1 && z == 4 && w == 2
    "#;
    let program = parse(input).unwrap();
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("v")), Some(&Value::Num(1, 0..10)));
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
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("z")), Some(&Value::Bool(false)));
    assert_eq!(env.get(&id("w")), Some(&Value::Bool(true)));
}

#[test]
fn test_div_0() {
	let input = r#"
		alphabet: {'a'}
		let x: int[4];
		on input y {
			x = 3 / 0; 
		}
		accept if x == 3
	"#;
	let program = parse(input).unwrap();
	println!("program: {:?}", program);

    let retval = eval(&program, input);

    if retval.is_ok() {
    	panic!("Expected DivisionbyZero error but got: {:?}", retval);
    } else {
    	match retval {
    	   Err(RuntimeError::DivisionbyZero) => (),
    	   _ => { panic!("Expected DivisionbyZero error but got: {:?}", retval);
    	   },
    	}
    }
}

#[test]
// Unary Operations - Negate and Not
fn test_unop() {
    let input = r#"
        alphabet: {'d'}
        let z: int;
        let w: bool;
        on input y {
            z = -4;
            w = !true;
        }
        accept if z == -4 && w == false
    "#;
    let program = parse(input).unwrap();
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("z")), Some(&Value::Num(-4, 0..10)));
    assert_eq!(env.get(&id("w")), Some(&Value::Bool(false)));
}

#[test]
// Match
fn test_match() {
    let input = r#"
        alphabet: {'d'}
        let z: int[5];
        on input y {
            z = match 2 {
                case 'a' => 1;
                case 2 => 2;
            }
        }
        accept if z == 2
    "#;
    
    let program = parse(input).unwrap();
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("z")), Some(&Value::Num(2, 0..5)));
}

#[test]
// Call
fn test_call() {
    let input = r#"
        alphabet: {'a'}
        fn add(a: int[3], b: int[0..3]) -> int[0..3] = a + b
        let x: int[3];
        on input y {
                x = add(1, 2);
        }
        accept if x == 3
    "#;
    let program = parse(input).unwrap();
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("x")), Some(&Value::Num(0, 0..3)));
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
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
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
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
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
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
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
    println!("program: {:?}", program);

    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("y")), Some(&Value::Num(1, 1..2)));
}




/* Test cast?
#[test]
// Cast, wraparound
fn test_cast_wraparound() {
    let input = r#"
        alphabet: {'d'}
        let z: int;
        let a: int;
        on input y {
            z = cast(1, 2..5, wraparound);
            a = cast(9, 2..5, wraparound);
        }
        accept if z == 3 && a == 3
    "#;
    let program = parse(input).unwrap();
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("z")), Some(&Value::Num(3, 2..5)));
    assert_eq!(env.get(&id("a")), Some(&Value::Num(3, 2..5)));
}

#[test]
// Cast, saturate
fn test_cast_saturate() {
    let input = r#"
        alphabet: {'d'}
        let z: int;
        let a: int;
        on input y {
            z = cast(1, 2..5, saturate);
            a = cast(9, 2..5, saturate);
        }
        accept if z == 2 && a == 4
    "#;
    let program = parse(input).unwrap();
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("z")), Some(&Value::Num(2, 2..5)));
    assert_eq!(env.get(&id("a")), Some(&Value::Num(4, 2..5)));
}
*/
