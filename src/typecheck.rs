use crate::syntax::*;
use log::{debug, error, warn};
use thiserror::Error;

/// Errors that can occur during type checking
#[derive(Error, Debug)]
pub enum TypeError {
    /// Type mismatch between expected and actual types
    #[error("Type mismatch")]
    TypeMismatch,
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

/// Helper function to create a type mismatch error
fn type_mismatch(actual: &Type, expected: &Type, expr: &Expr) -> TypeError {
    debug!("In {expr:#?}");
    // error!("Type mismatch: expected {expected:?}, found {actual:?}");
    TypeError::TypeMismatch
}

/// Helper function to check if two types are equal
fn expect_equal(actual: &Type, expected: &Type, expr: &Expr) -> Result<(), TypeError> {
    if actual == expected {
        Ok(())
    } else {
        Err(type_mismatch(actual, expected, expr))
    }
}

/// Type context for type checking
pub struct TypeCtx<'prog> {
    /// Type environment
    env: Map<Id, Type>,
    /// Function environment
    funcs: &'prog Map<Id, Function>,
}

/// Type check an expression in a given type context
pub fn typeck_expr(expr: &Expr, ctx: &TypeCtx) -> Result<Type, TypeError> {
    match expr {
        Expr::Var(id) => ctx
            .env
            .get(id)
            .cloned()
            .ok_or(TypeError::UndefinedVariable(*id)),
        Expr::Bool(_) => Ok(Type::BoolT),
        Expr::Num(_n, t) => Ok(t.clone()),
        Expr::Sym(_) => Ok(Type::SymT),
        Expr::BinOp { lhs, op, rhs } => {
            let lhs_type = typeck_expr(lhs, ctx)?;
            let rhs_type = typeck_expr(rhs, ctx)?;
            if !matches!(lhs_type, Type::NumT(_) | Type::SymT | Type::BoolT) {
                return Err(TypeError::TypeMismatch);
            }
            match op {
                BOp::Add | BOp::Sub | BOp::Mul | BOp::Div | BOp::Rem | BOp::Shl | BOp::Shr => {
                    expect_equal(&lhs_type, &rhs_type, &rhs).map(|_| lhs_type.clone())
                }
                BOp::Lt | BOp::Lte | BOp::Eq | BOp::Ne | BOp::And | BOp::Or => {
                    expect_equal(&lhs_type, &rhs_type, &rhs).map(|_| Type::BoolT)
                }
            }
        }
        Expr::UOp { op, inner } => {
            let inner_type = typeck_expr(inner, ctx)?;
            match op {
                UOp::Not => expect_equal(&inner_type, &Type::BoolT, &inner).map(|_| Type::BoolT),
                UOp::Negate => {
                    if matches!(inner_type, Type::NumT(_)) {
                        Ok(inner_type.clone())
                    } else {
                        debug!("In: {expr:?}");
                        Err(TypeError::TypeMismatch)
                    }
                }
            }
        }
        Expr::Cast {
            inner,
            typ,
            overflow: _overflow,
        } => {
            let inner_type = typeck_expr(inner, ctx)?;
            match (&inner_type, typ) {
                (Type::NumT(_), Type::NumT(_)) => Ok(typ.clone()),
                _ => {
                    debug!("In {expr:#?}");
                    Err(TypeError::TypeMismatch)
                }
            }
        }
        Expr::Call { callee, args } => {
            let mut arg_types: Vec<Type> = Vec::new();
            for arg in args {
                arg_types.push(typeck_expr(arg, ctx)?);
            }
            let function = ctx
                .funcs
                .get(callee)
                .ok_or(TypeError::UndefinedFunction(*callee))?;

            // Verify that argument types match parameter types
            let param_types: Vec<Type> = function.params.iter().map(|(_, t)| t.clone()).collect();
            if arg_types != param_types {
                return Err(type_mismatch(&arg_types[0], &param_types[0], expr));
            }
            Ok(function.ret_typ.clone())
        }
        Expr::Match { scrutinee, cases } => {
            let scrutinee_type = typeck_expr(scrutinee, ctx)?;
            // Define a closure to type check a single case and return its result type
            let check_case = |case: &Case| -> Result<Type, TypeError> {
                let mut case_env = ctx.env.clone();
                match &case.pattern {
                    Pattern::Var(id) => {
                        case_env.insert(*id, scrutinee_type.clone());
                    }
                    Pattern::Num(_) if matches!(scrutinee_type, Type::NumT(_)) => {}
                    Pattern::Bool(_) if scrutinee_type == Type::BoolT => {}
                    Pattern::Sym(_) if scrutinee_type == Type::SymT => {}
                    _ => {
                        debug!("In {case:#?}");
                        return Err(TypeError::TypeMismatch);
                    }
                };
                let guard_type = typeck_expr(
                    &case.guard,
                    &TypeCtx {
                        env: case_env.clone(),
                        funcs: ctx.funcs,
                    },
                )?;
                expect_equal(&guard_type, &Type::BoolT, &case.guard)?;
                typeck_expr(
                    &case.result,
                    &TypeCtx {
                        env: case_env,
                        funcs: ctx.funcs,
                    },
                )
            };
            let mut cases_iter = cases.iter();
            let result_type = if let Some(first_case) = cases_iter.next() {
                check_case(first_case)?
            } else {
                warn!("No cases, returning scrutinee type");
                scrutinee_type.clone()
            };
            for case in cases_iter {
                let case_type = check_case(case)?;
                debug!("In {:#?}", case);
                expect_equal(&case_type, &result_type, &case.result)?;
            }
            Ok(result_type)
        }
    }
}

/// Typecheck a statement in a given environment
pub fn typeck_stmt(stmt: &Stmt, ctx: &TypeCtx) -> Result<(), TypeError> {
    match stmt {
        Stmt::Assign(id, expr) => {
            let var_type = ctx
                .env
                .get(id)
                .cloned()
                .ok_or(TypeError::UndefinedVariable(*id))?;

            let expr_type = typeck_expr(expr, ctx)?;

            expect_equal(&expr_type, &var_type, expr)?;
            Ok(())
        }

        Stmt::If {
            cond,
            true_branch,
            false_branch,
        } => {
            let cond_type = typeck_expr(cond, ctx)?;

            // Check that the condition is a boolean
            expect_equal(&cond_type, &Type::BoolT, &cond)?;

            typeck_block(true_branch, ctx)?;
            typeck_block(false_branch, ctx)?;
            Ok(())
        }
    }
}

/// Typecheck a block of statements in the given environment using typeck_stmt
pub fn typeck_block(block: &Block, ctx: &TypeCtx) -> Result<(), TypeError> {
    // type check each stmt in the vector sequence is ok
    for stmt in block {
        typeck_stmt(stmt, ctx)?;
    }
    Ok(())
}

///Typecheck a function using the given environment and function environment
pub fn typeck_function(fun: &Function, ctx: &TypeCtx) -> Result<(), TypeError> {
    let mut fun_env = ctx.env.clone();

    for (param, param_type) in &fun.params {
        fun_env.insert(*param, param_type.clone());
    }

    let fun_ctx = TypeCtx {
        env: fun_env,
        funcs: ctx.funcs,
    };

    let e = typeck_expr(&fun.body, &fun_ctx)?;
    expect_equal(&e, &fun.ret_typ, &fun.body)
}

/// Type check a program
pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    let ctx = TypeCtx {
        env: program.locals.clone(),
        funcs: &program.helpers,
    };
    for (_func_name, function) in &program.helpers {
        typeck_function(function, &ctx)?;
    }
    typeck_block(&program.start, &ctx)?;

    let (input_var, action_block) = &program.action;
    let mut action_env = ctx.env.clone();
    if let Some(var) = input_var {
        // Check if the input variable shadows any existing variable
        if ctx.env.contains_key(var) {
            error!("Input variable '{}' shadows an existing variable", var);
            return Err(TypeError::UndefinedVariable(*var));
        }
        action_env.insert(*var, Type::SymT);
    }
    typeck_block(
        action_block,
        &TypeCtx {
            env: action_env,
            funcs: ctx.funcs,
        },
    )?;
    let final_type = typeck_expr(&program.accept, &ctx)?;
    expect_equal(&final_type, &Type::BoolT, &program.accept)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;

    #[test]
    fn variables() {
        let env = Map::from([
            (id("x"), Type::NumT(0..10)),
            (id("b"), Type::BoolT),
            (id("s"), Type::SymT),
        ]);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };
        let x_expr = Expr::Var(id("x"));
        let b_expr = Expr::Var(id("b"));
        let s_expr = Expr::Var(id("s"));
        assert_eq!(typeck_expr(&x_expr, &ctx).unwrap(), Type::NumT(0..10));
        assert_eq!(typeck_expr(&b_expr, &ctx).unwrap(), Type::BoolT);
        assert_eq!(typeck_expr(&s_expr, &ctx).unwrap(), Type::SymT);
        let unknown_expr = Expr::Var(id("unknown"));
        assert!(typeck_expr(&unknown_expr, &ctx).is_err());
    }

    #[test]
    fn undefined_variables() {
        let env = Map::new();
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };
        let undefined_expr = Expr::Var(id("undefined_flag"));
        assert!(typeck_expr(&undefined_expr, &ctx).is_err());

        let env = Map::from([
            (id("existing_flag"), Type::BoolT),
            (id("count"), Type::NumT(0..100)),
        ]);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };
        let another_undefined_expr = Expr::Var(id("another_flag"));
        assert!(typeck_expr(&another_undefined_expr, &ctx).is_err());
    }

    #[test]
    fn variable_shadowing() {
        // Test that input variables can't shadow existing variables
        let program_with_shadowing = r#"
            alphabet: {'a', 'b'}
            let c: bool;  // Define 'c' as a program variable
            on input c {  // Try to use 'c' as an input variable too
                c = true;
            }
            accept if c
        "#;

        let program: Program = parse(program_with_shadowing).unwrap();
        let result = typecheck_program(&program);

        // Verify that typechecking fails due to variable shadowing
        assert!(result.is_err());
        match result {
            Err(TypeError::UndefinedVariable(var_id)) => {
                assert_eq!(var_id, id("c")); // Verify the specific variable that caused the error
            }
            _ => panic!(
                "Expected UndefinedVariable error for shadowing, got: {:?}",
                result
            ),
        }

        // Test with non-shadowing variables (should pass)
        let program_without_shadowing = r#"
            alphabet: {'a', 'b'}
            let flag: bool;
            on input c {  // Different name than any program variable
                flag = true;
            }
            accept if flag
        "#;

        let valid_program: Program = parse(program_without_shadowing).unwrap();
        assert!(typecheck_program(&valid_program).is_ok());
    }

    #[test]
    fn literals() {
        let ctx = TypeCtx {
            env: Map::new(),
            funcs: &Map::new(),
        };
        assert_eq!(typeck_expr(&Expr::Bool(true), &ctx).unwrap(), Type::BoolT);
        assert_eq!(typeck_expr(&Expr::Bool(false), &ctx).unwrap(), Type::BoolT);

        let ranges = [(0..10), (0..100), (-10..10)];
        let values = [5, 42, -5];
        for (range, value) in ranges.iter().zip(values.iter()) {
            let num = Expr::Num(*value, Type::NumT(range.clone()));
            assert_eq!(typeck_expr(&num, &ctx).unwrap(), Type::NumT(range.clone()));
        }
        assert_eq!(typeck_expr(&Expr::Sym('a'), &ctx).unwrap(), Type::SymT);
    }

    #[test]
    fn binary_operations() {
        let ctx = TypeCtx {
            env: Map::new(),
            funcs: &Map::new(),
        };
        let num_type1 = Type::NumT(0..3);
        let num_type2 = Type::NumT(0..10);

        let num1 = Expr::Num(1, num_type1.clone());
        let num2 = Expr::Num(5, num_type2.clone());
        let bool_true = Expr::Bool(true);
        let bool_false = Expr::Bool(false);

        let arithmetic_ops = [
            BOp::Add,
            BOp::Sub,
            BOp::Mul,
            BOp::Div,
            BOp::Rem,
            BOp::Shl,
            BOp::Shr,
        ];
        for op in &arithmetic_ops {
            let bin_op1 = Expr::BinOp {
                lhs: Box::new(num1.clone()),
                op: op.clone(),
                rhs: Box::new(num1.clone()),
            };
            assert_eq!(typeck_expr(&bin_op1, &ctx).unwrap(), Type::NumT(0..3));
        }

        let comparison_ops = [BOp::Lt, BOp::Lte, BOp::Eq, BOp::Ne];
        for op in &comparison_ops {
            let bin_op = Expr::BinOp {
                lhs: Box::new(num1.clone()),
                op: op.clone(),
                rhs: Box::new(num1.clone()),
            };
            assert_eq!(typeck_expr(&bin_op, &ctx).unwrap(), Type::BoolT);
        }

        let logical_ops = [BOp::And, BOp::Or];
        for op in &logical_ops {
            let bin_op = Expr::BinOp {
                lhs: Box::new(bool_true.clone()),
                op: op.clone(),
                rhs: Box::new(bool_false.clone()),
            };
            assert_eq!(typeck_expr(&bin_op, &ctx).unwrap(), Type::BoolT);
        }

        let mismatch_test_cases = [
            // Arithmetic with mismatched numeric ranges without cast
            Expr::BinOp {
                lhs: Box::new(num1.clone()),
                op: BOp::Add,
                rhs: Box::new(num2.clone()),
            },
            // Arithmetic with boolean
            Expr::BinOp {
                lhs: Box::new(num1.clone()),
                op: BOp::Add,
                rhs: Box::new(bool_true.clone()),
            },
            // Boolean operation with number
            Expr::BinOp {
                lhs: Box::new(bool_true.clone()),
                op: BOp::And,
                rhs: Box::new(num1.clone()),
            },
        ];
        for test_case in &mismatch_test_cases {
            assert!(typeck_expr(test_case, &ctx).is_err());
        }

        let ctx = TypeCtx {
            env: Map::from([
                (id("x"), Type::NumT(0..10)),
                (id("y"), Type::NumT(0..10)),
                (id("z"), Type::NumT(0..100)),
                (id("flag1"), Type::BoolT),
                (id("flag2"), Type::BoolT),
            ]),
            funcs: &Map::new(),
        };

        let x_expr = Expr::Var(id("x"));
        let y_expr = Expr::Var(id("y"));
        let z_expr = Expr::Var(id("z"));
        let flag1_expr = Expr::Var(id("flag1"));
        let flag2_expr = Expr::Var(id("flag2"));
        let valid_ops = [
            (
                Expr::BinOp {
                    lhs: Box::new(x_expr.clone()),
                    op: BOp::Add,
                    rhs: Box::new(y_expr.clone()),
                },
                Type::NumT(0..10),
            ),
            (
                Expr::BinOp {
                    lhs: Box::new(x_expr.clone()),
                    op: BOp::Lt,
                    rhs: Box::new(y_expr.clone()),
                },
                Type::BoolT,
            ),
            (
                Expr::BinOp {
                    lhs: Box::new(flag1_expr.clone()),
                    op: BOp::And,
                    rhs: Box::new(flag2_expr.clone()),
                },
                Type::BoolT,
            ),
        ];
        for (expr, expected_type) in valid_ops {
            assert_eq!(typeck_expr(&expr, &ctx).unwrap(), expected_type);
        }

        let invalid_ops = [
            Expr::BinOp {
                lhs: Box::new(x_expr.clone()),
                op: BOp::Add,
                rhs: Box::new(flag1_expr.clone()),
            },
            Expr::BinOp {
                lhs: Box::new(x_expr.clone()),
                op: BOp::Add,
                rhs: Box::new(z_expr.clone()),
            },
        ];
        for expr in invalid_ops {
            assert!(typeck_expr(&expr, &ctx).is_err());
        }
    }

    #[test]
    fn unary_operations() {
        let ctx = TypeCtx {
            env: Map::from([
                (id("x"), Type::NumT(0..10)),
                (id("y"), Type::NumT(0..10)),
                (id("z"), Type::NumT(-5..5)),
                (id("flag1"), Type::BoolT),
                (id("flag2"), Type::BoolT),
            ]),
            funcs: &Map::new(),
        };
        let x_expr = Expr::Var(id("x"));
        let flag_expr = Expr::Var(id("flag1"));
        let valid_operations: [(UOp, Box<Expr>, Result<Type, TypeError>); 2] = [
            (UOp::Not, Box::new(flag_expr.clone()), Ok(Type::BoolT)),
            (UOp::Negate, Box::new(x_expr.clone()), Ok(Type::NumT(0..10))),
        ];
        for (op, inner, expected) in valid_operations {
            let unary_op = Expr::UOp { op, inner };
            match expected {
                Ok(Type::BoolT) => assert_eq!(typeck_expr(&unary_op, &ctx).unwrap(), Type::BoolT),
                Ok(Type::NumT(range)) => {
                    assert_eq!(typeck_expr(&unary_op, &ctx).unwrap(), Type::NumT(range))
                }
                _ => panic!("Unexpected expected type"),
            }
        }

        let invalid_operations: [(UOp, Box<Expr>); 2] = [
            (UOp::Not, Box::new(x_expr.clone())),
            (UOp::Negate, Box::new(flag_expr.clone())),
        ];
        for (op, inner) in invalid_operations {
            let unary_op = Expr::UOp { op, inner };
            assert!(typeck_expr(&unary_op, &ctx).is_err());
        }
    }

    #[test]
    fn casting() {
        let ctx = TypeCtx {
            env: Map::from([
                (id("x"), Type::NumT(0..10)),
                (id("y"), Type::NumT(0..10)),
                (id("flag"), Type::BoolT),
                (id("sym"), Type::SymT),
            ]),
            funcs: &Map::new(),
        };
        let cast1 = Expr::Cast {
            inner: Box::new(Expr::Var(id("x"))),
            typ: Type::NumT(0..100),
            overflow: Overflow::Fail,
        };
        let cast2 = Expr::Cast {
            inner: Box::new(Expr::Var(id("y"))),
            typ: Type::NumT(0..5),
            overflow: Overflow::Wraparound, // Allow overflow with wraparound
        };
        let cast3 = Expr::Cast {
            inner: Box::new(Expr::Num(42, Type::NumT(0..100))),
            typ: Type::NumT(0..50),
            overflow: Overflow::Saturate,
        };
        let cast4 = Expr::Cast {
            inner: Box::new(Expr::BinOp {
                lhs: Box::new(Expr::Var(id("x"))),
                op: BOp::Add,
                rhs: Box::new(Expr::Var(id("x"))),
            }),
            typ: Type::NumT(0..20),
            overflow: Overflow::Fail,
        };
        assert_eq!(typeck_expr(&cast1, &ctx).unwrap(), Type::NumT(0..100));
        assert_eq!(typeck_expr(&cast2, &ctx).unwrap(), Type::NumT(0..5));
        assert_eq!(typeck_expr(&cast3, &ctx).unwrap(), Type::NumT(0..50));
        assert_eq!(typeck_expr(&cast4, &ctx).unwrap(), Type::NumT(0..20));

        let invalid_cast1 = Expr::Cast {
            inner: Box::new(Expr::Var(id("flag"))),
            typ: Type::NumT(0..1),
            overflow: Overflow::Fail,
        };
        assert!(typeck_expr(&invalid_cast1, &ctx).is_err());

        let invalid_cast2 = Expr::Cast {
            inner: Box::new(Expr::Var(id("sym"))),
            typ: Type::NumT(0..255),
            overflow: Overflow::Fail,
        };
        assert!(typeck_expr(&invalid_cast2, &ctx).is_err());

        let invalid_cast3 = Expr::Cast {
            inner: Box::new(Expr::Var(id("x"))),
            typ: Type::BoolT,
            overflow: Overflow::Fail,
        };
        assert!(typeck_expr(&invalid_cast3, &ctx).is_err());

        let nested_cast = Expr::Cast {
            inner: Box::new(Expr::Cast {
                inner: Box::new(Expr::Var(id("x"))),
                typ: Type::NumT(0..50),
                overflow: Overflow::Fail,
            }),
            typ: Type::NumT(0..25),
            overflow: Overflow::Fail,
        };
        assert_eq!(typeck_expr(&nested_cast, &ctx).unwrap(), Type::NumT(0..25));
    }

    #[test]
    fn function_calls() {
        let ctx = TypeCtx {
            env: Map::from([
                (id("x"), Type::NumT(-10..10)),
                (id("flag"), Type::BoolT),
                (id("sym"), Type::SymT),
            ]),
            funcs: &Map::from([
                (
                    id("is_positive"),
                    Function {
                        params: vec![(id("n"), Type::NumT(-10..10))],
                        ret_typ: Type::BoolT,
                        body: Expr::Bool(true), // dummy body, not used in type checking
                    },
                ),
                (
                    id("to_number"),
                    Function {
                        params: vec![(id("b"), Type::BoolT)],
                        ret_typ: Type::NumT(0..1),
                        body: Expr::Num(0, Type::NumT(0..1)), // not used in type checking
                    },
                ),
                (
                    id("complex_func"),
                    Function {
                        params: vec![
                            (id("n1"), Type::NumT(-10..10)),
                            (id("n2"), Type::NumT(-10..10)),
                            (id("b"), Type::BoolT),
                        ],
                        ret_typ: Type::NumT(0..20),
                        body: Expr::Num(0, Type::NumT(0..20)), // not used in type checking
                    },
                ),
            ]),
        };

        let call1 = Expr::Call {
            callee: id("is_positive"),
            args: vec![Expr::Var(id("x"))],
        };
        assert_eq!(typeck_expr(&call1, &ctx).unwrap(), Type::BoolT);

        let call2 = Expr::Call {
            callee: id("to_number"),
            args: vec![Expr::Var(id("flag"))],
        };
        assert_eq!(typeck_expr(&call2, &ctx).unwrap(), Type::NumT(0..1));

        let call3 = Expr::Call {
            callee: id("complex_func"),
            args: vec![
                Expr::Var(id("x")),
                Expr::Var(id("x")),
                Expr::Var(id("flag")),
            ],
        };
        assert_eq!(typeck_expr(&call3, &ctx).unwrap(), Type::NumT(0..20));

        let invalid_call1 = Expr::Call {
            callee: id("undefined_func"),
            args: vec![Expr::Var(id("x"))],
        };
        assert!(typeck_expr(&invalid_call1, &ctx).is_err());

        let invalid_call3 = Expr::Call {
            callee: id("is_positive"),
            args: vec![Expr::Var(id("flag"))],
        };
        assert!(typeck_expr(&invalid_call3, &ctx).is_err());
    }

    #[test]
    fn pattern_matching() {
        let ctx = TypeCtx {
            env: Map::from([
                (id("x"), Type::NumT(0..10)),
                (id("flag"), Type::BoolT),
                (id("sym"), Type::SymT),
            ]),
            funcs: &Map::new(),
        };

        let num_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![
                Case {
                    pattern: Pattern::Num(5),
                    guard: Expr::Bool(true),
                    result: Expr::Bool(true),
                },
                Case {
                    pattern: Pattern::Var(id("n")),
                    guard: Expr::Bool(true),
                    result: Expr::Bool(false),
                },
            ],
        };
        assert_eq!(typeck_expr(&num_match, &ctx).unwrap(), Type::BoolT);

        let bool_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("flag"))),
            cases: vec![
                Case {
                    pattern: Pattern::Bool(true),
                    guard: Expr::Bool(true),
                    result: Expr::Num(1, Type::NumT(0..10)),
                },
                Case {
                    pattern: Pattern::Bool(false),
                    guard: Expr::Bool(true),
                    result: Expr::Num(0, Type::NumT(0..10)),
                },
            ],
        };
        assert_eq!(typeck_expr(&bool_match, &ctx).unwrap(), Type::NumT(0..10));

        let sym_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("sym"))),
            cases: vec![
                Case {
                    pattern: Pattern::Sym(Symbol('a')),
                    guard: Expr::Bool(true),
                    result: Expr::Var(id("x")),
                },
                Case {
                    pattern: Pattern::Var(id("s")),
                    guard: Expr::Bool(true),
                    result: Expr::Var(id("x")),
                },
            ],
        };
        assert_eq!(typeck_expr(&sym_match, &ctx).unwrap(), Type::NumT(0..10));

        let invalid_match1 = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![Case {
                pattern: Pattern::Bool(true), // Boolean pattern for numeric scrutinee
                guard: Expr::Bool(true),
                result: Expr::Bool(true),
            }],
        };
        assert!(typeck_expr(&invalid_match1, &ctx).is_err());

        let invalid_match2 = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("flag"))),
            cases: vec![
                Case {
                    pattern: Pattern::Bool(true),
                    guard: Expr::Bool(true),
                    result: Expr::Bool(true), // Boolean result
                },
                Case {
                    pattern: Pattern::Bool(false),
                    guard: Expr::Bool(true),
                    result: Expr::Num(0, Type::NumT(0..10)), // Numeric result
                },
            ],
        };
        assert!(typeck_expr(&invalid_match2, &ctx).is_err());

        let invalid_match3 = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![Case {
                pattern: Pattern::Var(id("n")),
                guard: Expr::Var(id("x")), // Numeric guard instead of boolean
                result: Expr::Bool(true),
            }],
        };
        assert!(typeck_expr(&invalid_match3, &ctx).is_err());
    }

    #[test]
    fn stmt() {
        //test assign
        //Create a type environment with a few variables
        let mut env = Map::new();
        env.insert(id("A"), Type::NumT(0..3));
        env.insert(id("B"), Type::SymT);
        env.insert(id("C"), Type::BoolT);
        //variables that will be used for err cases
        env.insert(id("X"), Type::NumT(0..1));
        env.insert(id("Y"), Type::BoolT);
        env.insert(id("Z"), Type::SymT);
        //variables for if case
        env.insert(id("result1"), Type::NumT(0..2));
        env.insert(id("result2"), Type::NumT(0..2));

        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        //make expressions
        let e1 = Expr::Num(2, Type::NumT(0..3));
        let e2 = Expr::Sym('x');
        let e3 = Expr::Bool(true);

        let assign1 = Stmt::Assign(id("A"), e1.clone());
        let assign2 = Stmt::Assign(id("B"), e2.clone());
        let assign3 = Stmt::Assign(id("C"), e3.clone());

        //check OK
        assert!(typeck_stmt(&assign1, &ctx).is_ok());
        assert!(typeck_stmt(&assign2, &ctx).is_ok());
        assert!(typeck_stmt(&assign3, &ctx).is_ok());

        let err1 = Stmt::Assign(id("X"), e1);
        let err2 = Stmt::Assign(id("Y"), e2);
        let err3 = Stmt::Assign(id("Z"), e3);

        //check ERR
        assert!(typeck_stmt(&err1, &ctx).is_err());
        assert!(typeck_stmt(&err2, &ctx).is_err());
        assert!(typeck_stmt(&err3, &ctx).is_err());

        let tb1 = Stmt::Assign(id("result1"), Expr::Num(1, Type::NumT(0..2)));
        let fb1 = Stmt::Assign(id("result2"), Expr::Num(2, Type::NumT(0..2)));
        let e4 = Expr::Bool(true);

        //test stmt::if
        let if1 = Stmt::If {
            cond: e4,
            true_branch: vec![tb1],
            false_branch: vec![fb1],
        };

        assert!(typeck_stmt(&if1, &ctx).is_ok());
    }

    #[test]
    fn block() {
        //Create a type environment with a few variables
        let mut env = Map::new();
        env.insert(id("A"), Type::NumT(0..3));
        env.insert(id("B"), Type::SymT);
        env.insert(id("C"), Type::BoolT);

        //variables that will be used for err cases
        env.insert(id("X"), Type::NumT(0..1));
        env.insert(id("Y"), Type::BoolT);
        env.insert(id("Z"), Type::SymT);

        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        //make expressions for use in stmt blocks
        let e1 = Expr::Num(1, Type::NumT(0..3));
        let e2 = Expr::Sym('x');
        let e3 = Expr::Bool(true);

        //assign statements
        let s1 = Stmt::Assign(id("A"), e1.clone());
        let s2 = Stmt::Assign(id("B"), e2.clone());
        let s3 = Stmt::Assign(id("C"), e3.clone());

        let b_ok: Block = vec![s1, s2, s3];

        assert!(typeck_block(&b_ok, &ctx).is_ok());

        //assign statements that should result in error
        let err1 = Stmt::Assign(id("X"), e1);
        let err2 = Stmt::Assign(id("Y"), e2);
        let err3 = Stmt::Assign(id("Z"), e3);

        let b_err: Block = vec![err1, err2, err3];

        assert!(typeck_block(&b_err, &ctx).is_err());
    }

    #[test]
    fn functions() {
        let ctx = TypeCtx {
            env: Map::new(),
            funcs: &Map::new(),
        };

        let fun_ok = Function {
            params: vec![],
            ret_typ: Type::NumT(0..1),
            body: Expr::BinOp {
                lhs: (Box::new(Expr::Num(1, Type::NumT(0..1)))),
                op: (BOp::Add),
                rhs: (Box::new(Expr::Num(1, Type::NumT(0..1)))),
            },
        };

        assert!(typeck_function(&fun_ok, &ctx).is_ok());

        let fun_err = Function {
            params: vec![],
            ret_typ: Type::BoolT,
            body: Expr::BinOp {
                lhs: (Box::new(Expr::Num(1, Type::NumT(0..1)))),
                op: (BOp::Add),
                rhs: (Box::new(Expr::Num(1, Type::NumT(0..1)))),
            },
        };

        assert!(typeck_function(&fun_err, &ctx).is_err());

        //test nested env, following example div3.un program
        let mut env_map = Map::new();
        env_map.insert(id("rem"), Type::NumT(0..3));

        let mut functions = Map::new();
        functions.insert(
            id("char_to_bit"),
            Function {
                params: vec![(id("c"), Type::SymT)],
                ret_typ: Type::NumT(0..2),
                body: Expr::Match {
                    scrutinee: Box::new(Expr::Var(id("c"))),
                    cases: vec![
                        // Case 1: '0' -> 0 as int[2]
                        Case {
                            pattern: Pattern::Sym(Symbol('0')),
                            guard: Expr::Bool(true),
                            result: Expr::Num(0, Type::NumT(0..2)),
                        },
                        // Case 2: '1' -> 1 as int[2]
                        Case {
                            pattern: Pattern::Sym(Symbol('1')),
                            guard: Expr::Bool(true),
                            result: Expr::Num(1, Type::NumT(0..2)),
                        },
                    ],
                },
            },
        );

        let nest = TypeCtx {
            env: env_map.clone(),
            funcs: &functions.clone(),
        };

        let nest_check = TypeCtx {
            env: env_map.clone(),
            funcs: &functions.clone(),
        };

        //Check typeck_function works without err if passed in populated ctx and passed in
        //function from the function map
        assert!(typeck_function(nest.funcs.get(&id("char_to_bit")).unwrap(), &nest).is_ok());
        //test that ctx and nested maps are not changed through typeck_function
        assert_eq!(nest.env, nest_check.env);
        assert_eq!(nest.funcs, nest_check.funcs);
    }

    #[test]
    fn programs() {
        let program1_input = r#"
                alphabet: {'a'}
                fn add(a: int[3], b: int[0..3]) -> int[0..3] = a + b
                let x: int[3];
                on input y {
                    x = add(1 as int[3], 2 as int[3]);
                    x = 3 as int[3] + - 4 as int[3];
                    x = 3 as int[3] + 4 as int[3] wraparound;
                    if x < 3 as int[3] {
                        y = 'a';
                    } else {
                        x = match y {
                            'a' -> 1 as int[3]
                            x if true -> 2 as int[3]
                        };
                    }
                }
                accept if x == 3 as int[3]
                "#;
        let program1: Program = parse(program1_input).unwrap();
        assert!(typecheck_program(&program1).is_ok());

        let program2_input = r#"
                alphabet: {'a'}
                fn is_a(c: sym) -> bool = c == 'a'
                let flag: bool;
                on input c {
                    flag = is_a(c);
                    if flag {
                        c = 'b';
                    }
                }
                accept if flag
                "#;
        let program2: Program = parse(program2_input).unwrap();
        assert!(typecheck_program(&program2).is_ok());

        // Test program with invalid function return type
        let program3_input = r#"
                alphabet: {'b', 'c'}
                fn invalid_helper(c: sym) -> int[3] = c == 'b'
                let flag: bool;
                on input c {
                    flag = is_a(c);
                    if flag {
                        c = 'b';
                    }
                }
                accept if flag
                "#;
        let program3: Program = parse(program3_input).unwrap();
        assert!(typecheck_program(&program3).is_err());

        // Test program with invalid start
        let program4_input = r#"
                alphabet: {'a'}
                let flag: bool;
                on input c {
                    flag = 'c';
                }
                accept if flag
                "#;
        let program4: Program = parse(program4_input).unwrap();
        assert!(typecheck_program(&program4).is_err());

        // Test program with invalid action block
        let program5_input = r#"
                alphabet: {'a'}
                let flag: bool;
                on input c {
                    if b == 'a' {
                        flag = 1 as bool;
                    }
                }
                accept if flag
                "#;
        let program5: Program = parse(program5_input).unwrap();
        assert!(typecheck_program(&program5).is_err());
    }
}
