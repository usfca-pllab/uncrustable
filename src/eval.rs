use std::collections::BTreeSet;
use std::collections::HashMap as Map;
use std::ops::Range;
use thiserror::Error;
use crate::syntax::*;

// Errors that can occur during type checking
#[derive(Error, Debug)]
pub enum RuntimeError {
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

    // insert each symbol into the enviornment
    if let Some(id) = &program.action.0 {
        for sym in input.chars() {
            env.insert(id.clone(), Value::Sym(Symbol(sym)));
        }
    }

    // evaluate action
    for stmt in &program.action.1 {
        eval_stmt(stmt, &mut env, &program)?;
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
        _ => Ok(Value::Num(
            (v - lower).abs() % (upper - lower) + lower,
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
                match (case.pattern, raw_val.clone()) {
                    (Pattern::Var(id), _) => {
                        let mut env_scope = Env::new();
                        env_scope.insert(id, raw_val.clone());
                        let g = eval_expr(&case.guard, &env_scope, &program)?;
                        if let Value::Bool(b) = g {
                            if b {
                                return eval_expr(&case.result, env, &program);
                            }
                        } else {
                            return Err(RuntimeError::TypeError);
                        }
                    }
                    (Pattern::Bool(bp), Value::Bool(bv)) if bp == bv => {
                        let g = eval_expr(&case.guard, &env, &program)?;
                        if let Value::Bool(b) = g {
                            if b {
                                return eval_expr(&case.result, env, &program);
                            }
                        } else {
                            return Err(RuntimeError::TypeError);
                        }
                    }
                    (Pattern::Num(np), Value::Num(nv, _)) if np == nv => {
                        let g = eval_expr(&case.guard, &env, &program)?;
                        if let Value::Bool(b) = g {
                            if b {
                                return eval_expr(&case.result, env, &program);
                            }
                        } else {
                            return Err(RuntimeError::TypeError);
                        }
                    }
                    (Pattern::Sym(sp), Value::Sym(sv)) if sp == sv => {
                        let g = eval_expr(&case.guard, &env, &program)?;
                        if let Value::Bool(b) = g {
                            if b {
                                return eval_expr(&case.result, env, &program);
                            }
                        } else {
                            return Err(RuntimeError::TypeError);
                        }
                    }
                    _ => continue,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::syntax::*;

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
        println!("program: {:?}", program);

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
        let program = Program {
            alphabet: Set::from([Symbol('d')]),
            helpers: Map::new(),
            locals: Map::new(),
            start: vec![],
            action: (
                Some(id("y")),
                vec![
                    Stmt::Assign(
                        id("z"),
                        Expr::UOp {
                            op: UOp::Negate,
                            inner: Box::new(Expr::Num(4, Type::NumT(0..10))),
                        },
                    ),
                    Stmt::Assign(
                        id("w"),
                        Expr::UOp {
                            op: UOp::Not,
                            inner: Box::new(Expr::Bool(true)),
                        },
                    ),
                ],
            ),
            accept: Expr::Bool(true),
        };

        let input = "";
        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("z")), Some(&Value::Num(4, 0..10)));
        assert_eq!(env.get(&id("w")), Some(&Value::Bool(false)));
    }

    #[test]
    // Cast, wraparound
    fn test_cast_wraparound() {
        let program = Program {
            alphabet: Set::from([Symbol('d')]),
            helpers: Map::new(),
            locals: Map::new(),
            start: vec![],
            action: (
                Some(id("y")),
                vec![
                    // under bounds
                    Stmt::Assign(
                        id("z"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(1, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Wraparound,
                        },
                    ),
                    // over bounds
                    Stmt::Assign(
                        id("a"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(9, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Wraparound,
                        },
                    ),
                ],
            ),
            accept: Expr::Bool(true),
        };
        let input = "";

        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("z")), Some(&Value::Num(3, 2..5)));
        assert_eq!(env.get(&id("a")), Some(&Value::Num(3, 2..5)));
    }

    #[test]
    // Cast, saturate
    fn test_cast_saturate() {
        let program = Program {
            alphabet: Set::from([Symbol('d')]),
            helpers: Map::new(),
            locals: Map::new(),
            start: vec![],
            action: (
                Some(id("y")),
                vec![
                    // under bounds
                    Stmt::Assign(
                        id("z"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(1, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Saturate,
                        },
                    ),
                    // over bounds
                    Stmt::Assign(
                        id("a"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(9, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Saturate,
                        },
                    ),
                ],
            ),
            accept: Expr::Bool(true),
        };
        let input = "";

        let (_result, env) = eval(&program, input).unwrap();

        assert_eq!(env.get(&id("z")), Some(&Value::Num(2, 2..5)));
        assert_eq!(env.get(&id("a")), Some(&Value::Num(4, 2..5)));
    }

    #[test]
    // Cast, fail
    fn test_cast_fail() {
        let program = Program {
            alphabet: Set::from([Symbol('d')]),
            helpers: Map::new(),
            locals: Map::new(),
            start: vec![],
            action: (
                Some(id("y")),
                vec![
                    // under bounds
                    Stmt::Assign(
                        id("z"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(1, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Fail,
                        },
                    ),
                    // over bounds
                    Stmt::Assign(
                        id("a"),
                        Expr::Cast {
                            inner: Box::new(Expr::Num(9, Type::NumT(0..10))),
                            typ: Type::NumT(2..5),
                            overflow: Overflow::Fail,
                        },
                    ),
                ],
            ),
            accept: Expr::Bool(true),
        };
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
        let input = "";
        let program = Program {
            alphabet: Set::from([Symbol('d')]),
            helpers: Map::new(),
            locals: Map::new(),
            start: vec![],
            action: (
                Some(id("y")),
                vec![
                    // under bounds
                    Stmt::Assign(
                        id("z"),
                        Expr::Match {
                            scrutinee: Box::new(Expr::Num(2, Type::NumT(2..3))),
                            cases: vec![
                                Case {
                                    pattern: Pattern::Sym(Symbol('a')),
                                    guard: Expr::Bool(true),
                                    result: Expr::Num(1, Type::NumT(1..2)),
                                },
                                Case {
                                    pattern: Pattern::Num(2),
                                    guard: Expr::Bool(true),
                                    result: Expr::Num(2, Type::NumT(0..5)),
                                },
                            ],
                        },
                    ),
                ],
            ),
            accept: Expr::Bool(true),
        };
        println!("program: {:?}", program);

        let (_result, env) = eval(&program, input).unwrap();
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

        let (_result, env) = eval(&program, input).unwrap();
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
        println!("program: {:?}", program);

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
        println!("program: {:?}", program);

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
        println!("program: {:?}", program);

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
	                x = 3 + - 4 as int[3];
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
        println!("program: {:?}", program);

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
        println!("program: {:?}", program);

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
	                x = add(1, 2);
	        }
	        accept if x == 3
	    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

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
                _ => {
                    panic!("Expected DivisionbyZero error but got: {:?}", retval);
                }
            }
        }
    }
}
