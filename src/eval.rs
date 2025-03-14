use crate::syntax::*;
use std::collections::HashMap as Map;

#[derive(Debug)]
enum RuntimeError {
    InvalidExpression,
    InvalidStatement,
    DivisionbyZero,
    TypeError,
    InvalidOperand,
}

// abstraction for values making up expressions
#[derive(Debug, Clone, PartialEq)]
enum Value {
    Bool(bool),
    Num(i64),
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

        	// TODO - Do we put default values?? What are the defaults for Nums and Symbols??
        	
            Type::BoolT => Value::Bool(false), 
            
            //  TODO - add int - kinda confused about ranges
            Type::NumT(range) => {
                        Value::Num(range.start)
                    }
                    
            //  TODO - Type::SymT => Value::Sym(??)

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

// eval. expr.
fn eval_expr(expr: &Expr, env: &Env) -> Result<Value, RuntimeError> {
    match expr {

    
		/* TODO
    		Expr::Call
    		Expr::Cast
    		Expr::Match
    		How to handle ranges if we have to??
    	*/
    	
        Expr::Num(n, _) => Ok(Value::Num(*n)),
        Expr::Bool(b) => Ok(Value::Bool(*b)),
        Expr::Sym(symbol) => Ok(Value::Sym(Symbol(*symbol))),

        Expr::BinOp { lhs, op, rhs } => {

			let left = eval_expr(lhs, env)?;
			let right = eval_expr(rhs, env)?;

			match (left, right) {
			    (Value::Num(l), Value::Num(r)) => match op {
			    	// numerical return
			        BOp::Add => Ok(Value::Num(l + r)),
			        BOp::Sub => Ok(Value::Num(l - r)),
			        BOp::Mul => Ok(Value::Num(l * r)),
			        BOp::Div => {
			            if r == 0 {
			                Err(RuntimeError::DivisionbyZero)
			            } else {
			                Ok(Value::Num(l / r))
			            }
			        }
			        BOp::Rem => Ok(Value::Num(l % r)),
			        BOp::Shl => Ok(Value::Num(l << r)),
			        BOp::Shr => Ok(Value::Num(l >> r)),

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
            
                Value::Num(l) => match op {
                    UOp::Negate => Ok(Value::Num(-l)),
                    _ => Err(RuntimeError::InvalidOperand),
                },
                
                Value::Bool(b) => match op {
                    UOp::Not => Ok(Value::Bool(!b)),
                    _ => Err(RuntimeError::InvalidOperand),
                },
                _ => Err(RuntimeError::TypeError),
            }
        },

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
    assert_eq!(env.get(&id("y")), Some(&Value::Num(2)));
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

    assert_eq!(env.get(&id("x")), Some(&Value::Num(7)));  
    assert_eq!(env.get(&id("y")), Some(&Value::Num(4))); 
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
    assert_eq!(env.get(&id("z")), Some(&Value::Num(8))); 
    assert_eq!(env.get(&id("w")), Some(&Value::Num(2)));
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

    assert_eq!(env.get(&id("v")), Some(&Value::Num(1)));  
    assert_eq!(env.get(&id("z")), Some(&Value::Num(4)));  
    assert_eq!(env.get(&id("w")), Some(&Value::Num(2))); 
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
                    rhs: Box::new(Expr::Num(10, Type::NumT(0..10))),
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

    assert_eq!(env.get(&id("z")), Some(&Value::Num(-4)));
    assert_eq!(env.get(&id("w")), Some(&Value::Bool(false)));
}

