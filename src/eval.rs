use crate::syntax::*;
use std::collections::HashMap as Map;
use std::ops::Range;
use std::result;
use crate::parse::parse;

#[derive(Debug)]
enum RuntimeError {
    InvalidExpression,
    InvalidStatement,
    DivisionbyZero,
    TypeError,
    InvalidOperand,
    ModulusByZero, 
    OutOfRange,
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

	// insert alphabet into env
    for symbol in &program.alphabet {
        let id = id(&symbol.to_string());
        let value = Value::Sym(*symbol);    
		println!("Inserting alpha. id -> {}, value -> {:?}", id, value);
        env.insert(id, value);
    }

    // insert locals into env
	for (id, typ) in &program.locals {
        let value = match typ {

        	// defaults for now are zero values
        	
            Type::BoolT => Value::Bool(false), 
            
            //  TODO - add Range to the Num enum
            //  default to be beginning of range
            Type::NumT(range) => {
                        Value::Num(range.start, Range { start: range.start, end: range.end })
                    },
                    
            //  default to empty char
            Type::SymT => Value::Sym(Symbol(' ')),

            _ => continue // for now continue, raise error later 
        };

        println!("Inserting local id -> {}, value -> {:?}", id, value);
        env.insert(id.clone(), value);
    }


    env   
}

fn eval(program: &Program, input: &str) ->Result<(bool, Env), RuntimeError> {
    // create initial state
    let mut env = init_env(program); 

	// TODO - figure out what to do with input ("consume each symbol???")
    // consume each symbol

    // evaluate the final condition
    for stmt in &program.action.1 {
		eval_stmt(stmt, &mut env)?;
	}
    eval_expr(&program.accept, &env)?;
    
    Ok((false, env))
}

fn cast(v: i64, range: Range<i64>, overflow: Overflow) -> Result<Value, RuntimeError> {
    let lower = range.start;
    let upper = range.end; 
    let val = match overflow {
        Overflow::Fail => {
            if v > upper {
                Err(RuntimeError::OutOfRange)
            }
            else if v < lower {
                Err(RuntimeError::OutOfRange)
            }
            else {
                Ok(Value::Num(v, range))
            }
        },
        Overflow::Saturate => {
            if v > upper {
                Ok(Value::Num(upper - 1, range))
            }
            else if v < lower {
                Ok(Value::Num(lower, range))
            }
            else {
                Ok(Value::Num(v, range))
            }
        }
        Overflow::Wraparound => Ok(Value::Num(((v - lower).abs() % (upper - lower) + lower), Range { start: lower, end: upper })),
        _ => Err(RuntimeError::InvalidExpression)
    };
    return val;
}

// eval. expr.
fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, RuntimeError> {
    match expr {

		/* TODO
    		Expr::Call
    		Expr::Match
    		How to handle ranges if we have to??
    	*/
    	
        Expr::Num(n, Type::NumT(range)) => cast(*n, range.clone(), Overflow::Wraparound),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Sym(symbol) => Ok(Value::Sym(Symbol(*symbol))),
        Expr::Var(id) => Ok(env.get(id).unwrap().clone()), // will return a Value

        Expr::BinOp { lhs, op, rhs } => {

			let left = eval_expr(lhs, env)?;
			let right = eval_expr(rhs, env)?;

			match (left, right) {
			    (Value::Num(l, l_range), Value::Num(r, range_ignore)) => match op {
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
			        },
			        BOp::Rem => {
                        if r == 0 {
                            Err(RuntimeError::ModulusByZero)
                        } else {
                            cast(l % r, l_range, Overflow::Wraparound)
                        }
                    },
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
			       
			       _ => Err(RuntimeError::InvalidOperand),
			    },
			    _ => Err(RuntimeError::TypeError),
			}

        }

        Expr::UOp { op, inner } => {
            let left = eval_expr(inner, env)?;
            
            match left {
            
                Value::Num(l, range) => match op {
                    UOp::Negate => cast(-l, range, Overflow::Wraparound),
                    _ => Err(RuntimeError::InvalidOperand),
                },
                
                Value::Bool(b) => match op {
                    UOp::Not => Ok(Value::Bool(!b)),
                    _ => Err(RuntimeError::InvalidOperand),
                },
                _ => Err(RuntimeError::TypeError),
            }
        },

        Expr::Cast {inner, typ, overflow } => {
            
            let cast_t = match typ {
                Type::BoolT => Err(RuntimeError::TypeError),
                Type::SymT => Err(RuntimeError::TypeError),
                Type::NumT(n) => Ok(n.clone())
            }?;
            
            let val = eval_expr(inner, env)?;

            match val {
                Value::Num(num, range) => match overflow {
                    
                    Overflow::Fail => cast(num, cast_t, Overflow::Fail),
                    Overflow::Saturate => cast(num, cast_t, Overflow::Saturate),
                    Overflow::Wraparound => cast(num, cast_t, Overflow::Wraparound)
                }
                // can only cast ints
                Value::Bool(b) => Err(RuntimeError::TypeError),
                Value::Sym(s) => Err(RuntimeError::TypeError)
            }
        
        },

        Expr::Match { scrutinee, cases } => {
            let raw_val = eval_expr(&scrutinee, env)?;
            let res;
            for case in cases {
                let pattern = match case.pattern {
                    Pattern::Bool(b) => Value::Bool(b),
                    Pattern::Num(n) => Value::Num(n, Range { start: n, end: n + 1 }),
                    Pattern::Sym(s) => Value::Sym(s),
                    Pattern::Var(id) => continue
                };
                if raw_val == pattern {
                    let g = eval_expr(&case.guard, env).unwrap();
                    match g {
                        Value::Bool(b) if b => Ok(eval_expr(&case.result, env)),
                        Value::Sym(s) => Err(RuntimeError::TypeError),
                        Value::Num(n, range) => Err(RuntimeError::TypeError)
                    }
                }
                else {
                    Err(RuntimeError::TypeError)
                }

            }
            Err(RuntimeError::TypeError)
        },
        /*
         * let val = eval_expr(&scrutinee, env)?;
            let c;
            for case in cases {
                let val_p = match (val, case.pattern){
                    (Value::Bool(vb), Pattern::Bool(pb)) => {
                        if vb == pb {
                            let g = eval_expr(&case.guard, env);
                            match g {
                                Value::Bool(b) => {
                                    if b {
                                        c = case.result;
                                        Ok(Value::Bool(b))
                                    }
                                    else {
                                        Err(RuntimeError::InvalidExpression)
                                    }
                                },
                                Value::Num(n, range),
                                Value::Sym(s)
                            }
                        }
                    }
                    (Value::Num(nn, range), Pattern::Num(pn)) => Ok(Value::Num(nn, range)),
                    (Value::Sym(s), Pattern::Sym(ps)) => Ok(Value::Sym(s)),
                    _ => Err(RuntimeError::TypeError),
                };
            }
            eval_expr(&c, env)
         */

        Expr::Call { callee, args } => {
            let mut env_args = Env::new();
            // let val = eval_expr(env.get(callee).unwrap().clone(), &env_args);
            // env.insert(args);
            // try eval_expr(env.get(callee), args)
            Err(RuntimeError::InvalidExpression)
        }

        _ => Err(RuntimeError::InvalidExpression),
    }
}




// eval. stmt.
fn eval_stmt(stmt: &Stmt, env: &mut Env) -> Result<Value, RuntimeError> {

	// TODO : If 

	
    match stmt {
        Stmt::Assign(id, expr) => {
            let value = eval_expr(expr, env)?;
            println!("Assigned value {:?} to {}", value, id);
            env.insert(id.clone(), value.clone());
            Ok(value)
        }
        _ => Err(RuntimeError::InvalidStatement),
    }
}







// TEST -----------------------------------------------------------------------------------------------------------------------------------------------

#[test]
// Simple Alphabet
fn test_simple_1() {
    let program = Program {
        alphabet: Set::from([Symbol('a')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (None, vec![]),
        accept: Expr::Bool(true),
    };
    
    let input = "";  
    let (result, env) = eval(&program, input).unwrap();
    
    // Check that env contains the alphabet symbol 'a'
    assert!(env.contains_key(&id("a")));
    assert_eq!(env.get(&id("a")), Some(&Value::Sym(Symbol('a'))));
}


#[test]
// Simple Local Assignment
fn test_simple_2() {
    let program = Program {
        alphabet: Set::from([Symbol('b')]),
        helpers: Map::new(),
        locals: Map::from([
            (id("x"), Type::BoolT),
            (id("y"), Type::NumT(2..7)),
        ]),
        start: vec![],
        action: (None, vec![]),
        accept: Expr::Bool(true),
    };
    
    let input = "";  
    let (result, env) = eval(&program, input).unwrap();
    
    assert_eq!(env.get(&id("x")), Some(&Value::Bool(false)));  
    assert_eq!(env.get(&id("y")), Some(&Value::Num(2, 2..7)));
}


#[test]
// Simple Addition and Subtraction
fn test_binop_1() {
    let program = Program {
        alphabet: Set::from([Symbol('c')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("x"),
                Expr::BinOp {
                    op: BOp::Add,
                    lhs: Box::new(Expr::Num(3, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("y"),
                Expr::BinOp {
                    op: BOp::Sub,
                    lhs: Box::new(Expr::Num(8, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("x")), Some(&Value::Num(7, 0..10)));  
    assert_eq!(env.get(&id("y")), Some(&Value::Num(4, 0..10))); 
}


#[test]
// Simple Multipulcation and Division
fn test_binop_2() {
    let program = Program {
        alphabet: Set::from([Symbol('d')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("z"),
                Expr::BinOp {
                    op: BOp::Mul,
                    lhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("w"),
                Expr::BinOp {
                    op: BOp::Div,
                    lhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();
    assert_eq!(env.get(&id("z")), Some(&Value::Num(8, 0..10))); 
    assert_eq!(env.get(&id("w")), Some(&Value::Num(2, 0..10)));
}


#[test]
// Division by Zero
fn test_binop_3() {
    let program = Program {
        alphabet: Set::from([Symbol('e')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("v"),
                Expr::BinOp {
                    op: BOp::Div,
                    lhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(0, Type::NumT(0..10))),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let result = eval(&program, input);

    match result {
        Err(RuntimeError::DivisionbyZero) => (),
        _ => panic!("Expected DivisionbyZero error but got: {:?}", result),
    }
}

#[test]
// Numerical BinOp - Remainder, Shifts (Left and Right)
fn test_binop_4() {
    let program = Program {
        alphabet: Set::from([Symbol('e')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("v"),
                Expr::BinOp {
                    op: BOp::Rem,
                    lhs: Box::new(Expr::Num(10, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(3, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("z"),
                Expr::BinOp {
                    op: BOp::Shl,
                    lhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(1, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("w"),
                Expr::BinOp {
                    op: BOp::Shr,
                    lhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(1, Type::NumT(0..10))),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("v")), Some(&Value::Num(0, 0..10)));
    assert_eq!(env.get(&id("z")), Some(&Value::Num(4, 0..10)));
    assert_eq!(env.get(&id("w")), Some(&Value::Num(2, 0..10))); 
}

#[test]
// Boolean BinOp - Relational OPerators
fn test_binop_5() {
    let program = Program {
        alphabet: Set::from([Symbol('e')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("v"),
                Expr::BinOp {
                    op: BOp::Lt,
                    lhs: Box::new(Expr::Num(10, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(3, Type::NumT(0..10))),
                },
            ),
			Stmt::Assign(
                id("x"),
                Expr::BinOp {
                    op: BOp::Lt,
                    lhs: Box::new(Expr::Num(3, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(9, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("y"),
                Expr::BinOp {
                    op: BOp::Lt,
                    lhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                },
            ),
            Stmt::Assign(
                id("z"),
                Expr::BinOp {
                    op: BOp::Lte,
                    lhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                },
            ),

           Stmt::Assign(
                id("a"),
                Expr::BinOp {
                    op: BOp::Lte,
                    lhs: Box::new(Expr::Num(1, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                },
            ),

           Stmt::Assign(
                id("b"),
                Expr::BinOp {
                    op: BOp::Lte,
                    lhs: Box::new(Expr::Num(5, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(2, Type::NumT(0..10))),
                },
            ),
            
            Stmt::Assign(
                id("w"),
                Expr::BinOp {
                    op: BOp::Eq,
                    lhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(1, Type::NumT(0..10))),
                },
            ),

            Stmt::Assign(
                id("c"),
                Expr::BinOp {
                    op: BOp::Eq,
                    lhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                    rhs: Box::new(Expr::Num(4, Type::NumT(0..10))),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();

	assert_eq!(env.get(&id("v")), Some(&Value::Bool(true)));  
    assert_eq!(env.get(&id("x")), Some(&Value::Bool(true)));  
    assert_eq!(env.get(&id("y")), Some(&Value::Bool(false))); 
    assert_eq!(env.get(&id("z")), Some(&Value::Bool(true)));   
    assert_eq!(env.get(&id("a")), Some(&Value::Bool(true)));   
    assert_eq!(env.get(&id("b")), Some(&Value::Bool(false))); 
    assert_eq!(env.get(&id("w")), Some(&Value::Bool(false))); 
    assert_eq!(env.get(&id("c")), Some(&Value::Bool(true)));   
}

#[test]
// Boolean BinOp - And / Or
fn test_binop_6() {
    let program = Program {
        alphabet: Set::from([Symbol('d')]),
        helpers: Map::new(),
        locals: Map::new(),
        start: vec![],
        action: (Some(id("y")), vec![
            Stmt::Assign(
                id("z"),
                Expr::BinOp {
                    op: BOp::And,
                    lhs: Box::new(Expr::Bool(true)),                     
                    rhs: Box::new(Expr::Bool(false)), 
                },
            ),
            Stmt::Assign(
                id("w"),
                Expr::BinOp {
                    op: BOp::Or,
                    lhs: Box::new(Expr::Bool(true)),  
                    rhs: Box::new(Expr::Bool(false)),
                },
            ),
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();

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
        action: (Some(id("y")), vec![
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
        ]),
        accept: Expr::Bool(true),
    };

    let input = "";
    let (result, env) = eval(&program, input).unwrap();

    assert_eq!(env.get(&id("z")), Some(&Value::Num(-4, 0..10)));
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
        action: (Some(id("y")), vec![
            // under bounds
            Stmt::Assign(
                id("z"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(1, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Wraparound 
                }
            ),
            // over bounds
            Stmt::Assign(
                id("a"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(9, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Wraparound 
                }
            )
        ]),
        accept: Expr::Bool(true),
    };
    let input = "";
    
    let (result, env) = eval(&program, input).unwrap();

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
        action: (Some(id("y")), vec![
            // under bounds
            Stmt::Assign(
                id("z"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(1, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Saturate 
                }
            ),
            // over bounds
            Stmt::Assign(
                id("a"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(9, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Saturate 
                }
            )
        ]),
        accept: Expr::Bool(true),
    };
    let input = "";
    
    let (result, env) = eval(&program, input).unwrap();

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
        action: (Some(id("y")), vec![
            // under bounds
            Stmt::Assign(
                id("z"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(1, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Fail 
                }
            ),
            // over bounds
            Stmt::Assign(
                id("a"), 
                Expr::Cast { 
                    inner: Box::new(Expr::Num(9, Type::NumT(0..10))), 
                    typ: Type::NumT(2..5), 
                    overflow: Overflow::Fail 
                }
            )
        ]),
        accept: Expr::Bool(true),
    };
    let input = "";
    
    let result = eval(&program, input);

    match result {
        Err(RuntimeError::OutOfRange) => (),
        _ => panic!("Expected OutOfRange error but got: {:?}", result),
    }
}

