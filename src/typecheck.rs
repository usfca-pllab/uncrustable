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
    /// Wrong number of arguments
    #[error("Wrong number of arguments: expected {expected}, actual {actual}")]
    WrongNumberOfArguments {
        /// Expected number of arguments
        expected: usize,
        /// Actual number of arguments
        actual: usize,
    },
    /// Empty match cases
    #[error("Empty match cases")]
    EmptyMatchCases,
    /// Type mismatch between pattern and scrutinee
    #[error("Type mismatch between pattern and scrutinee")]
    MismatchPatternScrutinee {
        /// Expected type
        expected: Type,
        /// Actual pattern
        actual: Pattern,
    },
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
        // Variables are always of the type of the variable in the environment
        Expr::Var(id) => {
            // Get the type of the variable from the environment
            ctx.env
                .get(id)
                .cloned()
                .ok_or(TypeError::UndefinedVariable(*id))
        }
        // Boolean literals are always of type BoolT
        Expr::Bool(_) => Ok(Type::BoolT),
        // Numeric literals are always of the type they are given
        Expr::Num(_n, t) => Ok(t.clone()),
        // Function calls are always of the type of the function
        Expr::Sym(_) => Ok(Type::SymT),
        // Binary operations are always of the type of the left hand side
        Expr::BinOp { lhs, op, rhs } => {
            let lhs_type = typeck_expr(lhs, ctx)?;
            let rhs_type = typeck_expr(rhs, ctx)?;
            match op {
                // Arithmetic operations are always of type NumT(0..1)
                BOp::Add | BOp::Sub | BOp::Mul | BOp::Div | BOp::Rem | BOp::Shl | BOp::Shr => {
                    if let Type::NumT(_) = lhs_type {
                        if lhs_type != rhs_type {
                            return Err(TypeError::TypeMismatch {
                                expected: lhs_type,
                                actual: rhs_type,
                            });
                        }
                        Ok(lhs_type)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::NumT(0..1),
                            actual: lhs_type,
                        })
                    }
                }
                // Comparison operations
                BOp::Lt | BOp::Lte => {
                    if lhs_type != rhs_type {
                        return Err(TypeError::TypeMismatch {
                            expected: lhs_type,
                            actual: rhs_type,
                        });
                    }
                    if let Type::NumT(_) = lhs_type {
                        Ok(Type::BoolT)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::NumT(0..1),
                            actual: lhs_type,
                        })
                    }
                }
                // Equality and inequality can compare symbols as well as numbers
                BOp::Eq | BOp::Ne => {
                    if lhs_type != rhs_type {
                        return Err(TypeError::TypeMismatch {
                            expected: lhs_type,
                            actual: rhs_type,
                        });
                    }
                    // Allow comparison of numbers or symbols
                    if matches!(lhs_type, Type::NumT(_) | Type::SymT) {
                        Ok(Type::BoolT)
                    } else {
                        Ok(Type::BoolT)
                    }
                }
                // Logical operations are always of type BoolT
                BOp::And | BOp::Or => {
                    if lhs_type != rhs_type {
                        return Err(TypeError::TypeMismatch {
                            expected: lhs_type,
                            actual: rhs_type,
                        });
                    }
                    if lhs_type == Type::BoolT {
                        Ok(Type::BoolT)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::BoolT,
                            actual: lhs_type,
                        })
                    }
                }
            }
        }
        // Unary operations are always of the type of the inner expression
        Expr::UOp { op, inner } => {
            let inner_type = typeck_expr(inner, ctx)?;
            match op {
                UOp::Not => {
                    if inner_type == Type::BoolT {
                        Ok(Type::BoolT)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::BoolT,
                            actual: inner_type,
                        })
                    }
                }
                UOp::Negate => {
                    if let Type::NumT(range) = inner_type {
                        Ok(Type::NumT(range))
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::NumT(0..1),
                            actual: inner_type,
                        })
                    }
                }
            }
        }
        // Casting is always of the type of the inner expression
        Expr::Cast {
            inner,
            typ,
            overflow: _overflow,
        } => {
            // First, type check the inner expression
            let inner_type = typeck_expr(inner, ctx)?;

            // According to the [T-Cast] rule, both the inner expression and the target type
            // must be numeric types with bounds
            match (&inner_type, typ) {
                (Type::NumT(_), Type::NumT(_)) => {
                    // The cast is valid, return the target type
                    Ok(typ.clone())
                }
                (_, Type::NumT(_)) => {
                    // The inner expression is not a numeric type
                    Err(TypeError::TypeMismatch {
                        expected: Type::NumT(0..1), // Example numeric type
                        actual: inner_type,
                    })
                }
                (Type::NumT(_), _) => {
                    // The target type is not a numeric type
                    Err(TypeError::TypeMismatch {
                        expected: Type::NumT(0..1), // Example numeric type
                        actual: typ.clone(),
                    })
                }
                _ => {
                    // Neither the inner expression nor the target type are numeric types
                    Err(TypeError::TypeMismatch {
                        expected: Type::NumT(0..1), // Example numeric type
                        actual: inner_type,
                    })
                }
            }
        }
        // Call expressions are always of the type of the function
        Expr::Call { callee, args } => {
            // Type check each argument
            let mut arg_types: Vec<Type> = Vec::new();
            for arg in args {
                arg_types.push(typeck_expr(arg, ctx)?);
            }

            // Look up the function in the function environment
            let function = ctx
                .funcs
                .get(callee)
                .ok_or(TypeError::UndefinedFunction(*callee))?;

            // Check that the number of arguments matches the number of parameters
            if arg_types.len() != function.params.len() {
                return Err(TypeError::WrongNumberOfArguments {
                    expected: function.params.len(),
                    actual: arg_types.len(),
                });
            }

            // Verify that argument types match parameter types
            for (arg_type, (_, param_type)) in arg_types.iter().zip(function.params.iter()) {
                if arg_type != param_type {
                    return Err(TypeError::TypeMismatch {
                        expected: param_type.clone(),
                        actual: arg_type.clone(),
                    });
                }
            }

            // Return the function's return type
            Ok(function.ret_typ.clone())
        }
        // Pattern matching is always of the type of the scrutinee
        Expr::Match { scrutinee, cases } => {
            // Type check the scrutinee
            let scrutinee_type = typeck_expr(scrutinee, ctx)?;

            // If there are no cases, return an error
            if cases.is_empty() {
                return Err(TypeError::EmptyMatchCases);
            }

            // Type check each case and collect result types
            let mut result_type: Option<Type> = None;

            for case in cases {
                // Check pattern compatibility with scrutinee type
                let mut case_env = ctx.env.clone();
                match &case.pattern {
                    // Variable patterns can match any type
                    Pattern::Var(id) => {
                        // Create environment with the pattern variable
                        case_env.insert(*id, scrutinee_type.clone());
                    }
                    // Type-specific patterns must match their corresponding types
                    Pattern::Num(_) if matches!(scrutinee_type, Type::NumT(_)) => {}
                    Pattern::Bool(_) if scrutinee_type == Type::BoolT => {}
                    Pattern::Sym(_) if scrutinee_type == Type::SymT => {}
                    // Pattern type doesn't match scrutinee type
                    _ => {
                        return Err(TypeError::MismatchPatternScrutinee {
                            expected: scrutinee_type.clone(),
                            actual: case.pattern.clone(),
                        });
                    }
                };

                // Check guard is boolean
                let guard_type = typeck_expr(
                    &case.guard,
                    &TypeCtx {
                        env: case_env.clone(),
                        funcs: ctx.funcs,
                    },
                )?;
                if guard_type != Type::BoolT {
                    return Err(TypeError::TypeMismatch {
                        expected: Type::BoolT,
                        actual: guard_type,
                    });
                }

                // Check result type
                let case_result_type = typeck_expr(
                    &case.result,
                    &TypeCtx {
                        env: case_env,
                        funcs: ctx.funcs,
                    },
                )?;

                // Ensure consistency with previous cases
                if let Some(ref t) = result_type {
                    if *t != case_result_type {
                        return Err(TypeError::TypeMismatch {
                            expected: t.clone(),
                            actual: case_result_type,
                        });
                    }
                } else {
                    result_type = Some(case_result_type);
                }
            }

            // Return the type of the result
            result_type.ok_or(TypeError::MismatchPatternScrutinee {
                expected: scrutinee_type,
                actual: cases[0].pattern.clone(),
            })
        }
    }
}

/// Typecheck a statement in a given environment
pub fn typeck_stmt(stmt: &Stmt, ctx: &TypeCtx) -> Result<(), TypeError> {
    // Either an asignment statemnt (x = 5)
    // Or an if statment (if condition - true block - or  - false block)
    // need to be able to assign a var or an expression

    match stmt {
        Stmt::Assign(id, expr) => {
            // Get the type of the variable from the environment
            let var_type = ctx
                .env
                .get(id)
                .cloned()
                .ok_or(TypeError::UndefinedVariable(*id))?;

            // Type check the expression
            let expr_type = typeck_expr(expr, ctx)?;

            // Check that the expression's type matches the variable's type
            if var_type != expr_type {
                return Err(TypeError::TypeMismatch {
                    expected: var_type,
                    actual: expr_type,
                });
            }
            Ok(())
        }
        // If statement: check that the condition is a boolean and type check both branches
        Stmt::If {
            cond,
            true_branch,
            false_branch,
        } => {
            // Type check the condition
            let cond_type = typeck_expr(cond, ctx)?;

            // Check that the condition is a boolean
            if cond_type != Type::BoolT {
                return Err(TypeError::TypeMismatch {
                    expected: Type::BoolT,
                    actual: cond_type,
                });
            };

            // Type check both branches
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

    if e == fun.ret_typ {
        Ok(())
    } else {
        let t = fun.ret_typ.clone();
        Err(TypeError::TypeMismatch {
            expected: t,
            actual: e,
        })
    }
}

/// Type check a program
pub fn typecheck_program(program: &Program) -> Result<(), TypeError> {
    // Create initial context
    let ctx = TypeCtx {
        env: program.locals.clone(),
        funcs: &program.helpers,
    };

    // Check that all helper functions are well-typed
    for (func_name, function) in &program.helpers {
        if let Err(err) = typeck_function(function, &ctx) {
            println!("Type error in function '{}': {}", func_name, err);
            return Err(err);
        }
    }

    // Type check all statements in the block
    if let Err(err) = typeck_block(&program.start, &ctx) {
        println!("Type error in block: {}", err);
        return Err(err);
    }

    // Type check the action handler
    let (input_var, action_block) = &program.action;

    // Add the input variable to the environment as a symbol type if it exists
    let mut action_env = ctx.env.clone();
    if let Some(var) = input_var {
        action_env.insert(*var, Type::SymT);
    }

    // Type check the action block
    typeck_block(
        action_block,
        &TypeCtx {
            env: action_env,
            funcs: ctx.funcs,
        },
    )?;

    // Type check the accept condition (must be a boolean)
    let final_type = typeck_expr(&program.accept, &ctx)?;
    if final_type != Type::BoolT {
        return Err(TypeError::TypeMismatch {
            expected: Type::BoolT,
            actual: final_type,
        });
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::syntax::*;

    #[test]
    fn variables() {
        // Create a type environment with a few variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("b"), Type::BoolT);
        env.insert(id("s"), Type::SymT);

        // Create context
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Test variable lookup for existing variables
        let x_expr = Expr::Var(id("x"));
        let b_expr = Expr::Var(id("b"));
        let s_expr = Expr::Var(id("s"));

        assert!(matches!(typeck_expr(&x_expr, &ctx), Ok(Type::NumT(range)) if range == (0..10)));
        assert!(matches!(typeck_expr(&b_expr, &ctx), Ok(Type::BoolT)));
        assert!(matches!(typeck_expr(&s_expr, &ctx), Ok(Type::SymT)));

        // Test variable lookup for non-existent variable
        let unknown_expr = Expr::Var(id("unknown"));
        assert!(typeck_expr(&unknown_expr, &ctx).is_err());
    }

    #[test]
    fn undefined_variables() {
        // Create an empty type environment
        let env = Map::new();
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Try to access an undefined variable
        let undefined_expr = Expr::Var(id("undefined_flag"));

        // Should result in an error
        assert!(typeck_expr(&undefined_expr, &ctx).is_err());

        // Create an environment with some variables, but not the one we'll try to access
        let mut env = Map::new();
        env.insert(id("existing_flag"), Type::BoolT);
        env.insert(id("count"), Type::NumT(0..100));
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Try to access a different undefined variable
        let another_undefined_expr = Expr::Var(id("another_flag"));

        // Should also result in an error
        assert!(typeck_expr(&another_undefined_expr, &ctx).is_err());
    }

    #[test]
    fn literals() {
        let ctx = TypeCtx {
            env: Map::new(),
            funcs: &Map::new(),
        };

        // Test boolean literals
        assert!(matches!(
            typeck_expr(&Expr::Bool(true), &ctx),
            Ok(Type::BoolT)
        ));
        assert!(matches!(
            typeck_expr(&Expr::Bool(false), &ctx),
            Ok(Type::BoolT)
        ));

        // Test numeric literals with different ranges
        let ranges = [(0..10), (0..100), (-10..10)];
        let values = [5, 42, -5];

        for (_i, (range, value)) in ranges.iter().zip(values.iter()).enumerate() {
            let num = Expr::Num(*value, Type::NumT(range.clone()));
            assert!(matches!(typeck_expr(&num, &ctx), Ok(Type::NumT(r)) if r == *range));
        }

        // Test symbol literals
        assert!(matches!(typeck_expr(&Expr::Sym('a'), &ctx), Ok(Type::SymT)));
    }

    #[test]
    fn binary_operations() {
        // Create a type environment
        let ctx = TypeCtx {
            env: Map::new(),
            funcs: &Map::new(),
        };

        // Define numeric types and expressions for testing
        let num_type1 = Type::NumT(0..3);
        let num_type2 = Type::NumT(0..10);

        let num1 = Expr::Num(1, num_type1.clone());
        let num2 = Expr::Num(5, num_type2.clone());
        let bool_true = Expr::Bool(true);
        let bool_false = Expr::Bool(false);

        // 1. Arithmetic operations with matching types (should succeed)
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
            assert!(matches!(typeck_expr(&bin_op1, &ctx), Ok(Type::NumT(r)) if r == (0..3)));
        }

        // 2. Comparison operations with matching types (should return bool)
        let comparison_ops = [BOp::Lt, BOp::Lte, BOp::Eq, BOp::Ne];
        for op in &comparison_ops {
            let bin_op = Expr::BinOp {
                lhs: Box::new(num1.clone()),
                op: op.clone(),
                rhs: Box::new(num1.clone()),
            };
            assert!(matches!(typeck_expr(&bin_op, &ctx), Ok(Type::BoolT)));
        }

        // 3. Logical operations on booleans (should return bool)
        let logical_ops = [BOp::And, BOp::Or];
        for op in &logical_ops {
            let bin_op = Expr::BinOp {
                lhs: Box::new(bool_true.clone()),
                op: op.clone(),
                rhs: Box::new(bool_false.clone()),
            };
            assert!(matches!(typeck_expr(&bin_op, &ctx), Ok(Type::BoolT)));
        }

        // 4. Type mismatches (should fail)
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

        // 5. Test with variables (ensure the rest of the test remains)
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("y"), Type::NumT(0..10));
        env.insert(id("z"), Type::NumT(-5..5));
        env.insert(id("flag1"), Type::BoolT);
        env.insert(id("flag2"), Type::BoolT);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Create variable expressions
        let x_expr = Expr::Var(id("x"));
        let y_expr = Expr::Var(id("y"));
        let z_expr = Expr::Var(id("z"));
        let flag1_expr = Expr::Var(id("flag1"));
        let flag2_expr = Expr::Var(id("flag2"));

        // Valid operations with variables
        let valid_ops = [
            // x + y (same range)
            (
                Expr::BinOp {
                    lhs: Box::new(x_expr.clone()),
                    op: BOp::Add,
                    rhs: Box::new(y_expr.clone()),
                },
                Type::NumT(0..10),
            ),
            // x < y (comparison)
            (
                Expr::BinOp {
                    lhs: Box::new(x_expr.clone()),
                    op: BOp::Lt,
                    rhs: Box::new(y_expr.clone()),
                },
                Type::BoolT,
            ),
            // flag1 && flag2 (logical)
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
            assert!(matches!(typeck_expr(&expr, &ctx), Ok(t) if t == expected_type));
        }

        // Invalid operations with variables
        let invalid_ops = [
            // Type mismatch: x + flag1
            Expr::BinOp {
                lhs: Box::new(x_expr.clone()),
                op: BOp::Add,
                rhs: Box::new(flag1_expr.clone()),
            },
            // Different ranges: x + z
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
        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("flag"), Type::BoolT);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Create variable expressions
        let x_expr = Expr::Var(id("x"));
        let flag_expr = Expr::Var(id("flag"));

        // Test valid unary operations
        let valid_operations: [(UOp, Box<Expr>, Result<Type, TypeError>); 2] = [
            // Not with boolean
            (UOp::Not, Box::new(flag_expr.clone()), Ok(Type::BoolT)),
            // Negate with numeric
            (UOp::Negate, Box::new(x_expr.clone()), Ok(Type::NumT(0..10))),
        ];

        for (op, inner, expected) in valid_operations {
            let unary_op = Expr::UOp { op, inner };

            match expected {
                Ok(Type::BoolT) => assert!(matches!(typeck_expr(&unary_op, &ctx), Ok(Type::BoolT))),
                Ok(Type::NumT(range)) => {
                    assert!(matches!(typeck_expr(&unary_op, &ctx), Ok(Type::NumT(r)) if r == range))
                }
                _ => panic!("Unexpected expected type"),
            }
        }

        // Test invalid unary operations
        let invalid_operations: [(UOp, Box<Expr>); 2] = [
            // Not with numeric
            (UOp::Not, Box::new(x_expr.clone())),
            // Negate with boolean
            (UOp::Negate, Box::new(flag_expr.clone())),
        ];

        for (op, inner) in invalid_operations {
            let unary_op = Expr::UOp { op, inner };
            assert!(typeck_expr(&unary_op, &ctx).is_err());
        }
    }

    #[test]
    fn casting() {
        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("y"), Type::NumT(-5..5));
        env.insert(id("flag"), Type::BoolT);
        env.insert(id("sym"), Type::SymT);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Test valid numeric casts

        // Cast from one numeric range to another
        let cast1 = Expr::Cast {
            inner: Box::new(Expr::Var(id("x"))),
            typ: Type::NumT(0..100),
            overflow: Overflow::Fail,
        };
        assert!(matches!(typeck_expr(&cast1, &ctx), Ok(Type::NumT(range)) if range == (0..100)));

        // Cast from negative range to positive range
        let cast2 = Expr::Cast {
            inner: Box::new(Expr::Var(id("y"))),
            typ: Type::NumT(0..10),
            overflow: Overflow::Wraparound, // Allow overflow with wraparound
        };
        assert!(matches!(typeck_expr(&cast2, &ctx), Ok(Type::NumT(range)) if range == (0..10)));

        // Cast from numeric literal to different range
        let cast3 = Expr::Cast {
            inner: Box::new(Expr::Num(42, Type::NumT(0..100))),
            typ: Type::NumT(0..50),
            overflow: Overflow::Saturate,
        };
        assert!(matches!(typeck_expr(&cast3, &ctx), Ok(Type::NumT(range)) if range == (0..50)));

        // Cast from result of binary operation
        let cast4 = Expr::Cast {
            inner: Box::new(Expr::BinOp {
                lhs: Box::new(Expr::Var(id("x"))),
                op: BOp::Add,
                rhs: Box::new(Expr::Var(id("x"))),
            }),
            typ: Type::NumT(0..20),
            overflow: Overflow::Fail,
        };
        assert!(matches!(typeck_expr(&cast4, &ctx), Ok(Type::NumT(range)) if range == (0..20)));

        // Test invalid casts

        // Cast from boolean to numeric
        let invalid_cast1 = Expr::Cast {
            inner: Box::new(Expr::Var(id("flag"))),
            typ: Type::NumT(0..1),
            overflow: Overflow::Fail,
        };
        assert!(matches!(
            typeck_expr(&invalid_cast1, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));

        // Cast from symbol to numeric
        let invalid_cast2 = Expr::Cast {
            inner: Box::new(Expr::Var(id("sym"))),
            typ: Type::NumT(0..255),
            overflow: Overflow::Fail,
        };
        assert!(matches!(
            typeck_expr(&invalid_cast2, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));

        // Cast from numeric to boolean (invalid target type)
        let invalid_cast3 = Expr::Cast {
            inner: Box::new(Expr::Var(id("x"))),
            typ: Type::BoolT,
            overflow: Overflow::Fail,
        };
        assert!(matches!(
            typeck_expr(&invalid_cast3, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));

        // Cast from numeric to symbol (invalid target type)
        let invalid_cast4 = Expr::Cast {
            inner: Box::new(Expr::Var(id("x"))),
            typ: Type::SymT,
            overflow: Overflow::Fail,
        };
        assert!(matches!(
            typeck_expr(&invalid_cast4, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));

        // Nested casts
        let nested_cast = Expr::Cast {
            inner: Box::new(Expr::Cast {
                inner: Box::new(Expr::Var(id("x"))),
                typ: Type::NumT(0..50),
                overflow: Overflow::Fail,
            }),
            typ: Type::NumT(0..25),
            overflow: Overflow::Fail,
        };
        assert!(
            matches!(typeck_expr(&nested_cast, &ctx), Ok(Type::NumT(range)) if range == (0..25))
        );
    }

    #[test]
    fn function_calls() {
        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(-10..10));
        env.insert(id("flag"), Type::BoolT);

        // Create a function environment with functions
        let mut function_env = Map::new();

        // Add a function that takes a numeric parameter and returns a boolean
        function_env.insert(
            id("is_positive"),
            Function {
                params: vec![(id("n"), Type::NumT(-10..10))],
                ret_typ: Type::BoolT,
                body: Expr::Bool(true), // Dummy body, not used in type checking
            },
        );

        // Add a function that takes a boolean parameter and returns a numeric value
        function_env.insert(
            id("to_number"),
            Function {
                params: vec![(id("b"), Type::BoolT)],
                ret_typ: Type::NumT(0..1),
                body: Expr::Num(0, Type::NumT(0..1)), // Dummy body, not used in type checking
            },
        );

        // Add a function that takes multiple parameters
        function_env.insert(
            id("complex_func"),
            Function {
                params: vec![
                    (id("n1"), Type::NumT(-10..10)),
                    (id("n2"), Type::NumT(-10..10)),
                    (id("b"), Type::BoolT),
                ],
                ret_typ: Type::NumT(0..20),
                body: Expr::Num(0, Type::NumT(0..20)), // Dummy body, not used in type checking
            },
        );

        let ctx = TypeCtx {
            env,
            funcs: &function_env,
        };

        // Test valid function calls

        // Call is_positive with a numeric variable
        let call1 = Expr::Call {
            callee: id("is_positive"),
            args: vec![Expr::Var(id("x"))],
        };
        assert!(matches!(typeck_expr(&call1, &ctx), Ok(Type::BoolT)));

        // Call to_number with a boolean variable
        let call2 = Expr::Call {
            callee: id("to_number"),
            args: vec![Expr::Var(id("flag"))],
        };
        assert!(matches!(typeck_expr(&call2, &ctx), Ok(Type::NumT(range)) if range == (0..1)));

        // Call complex_func with appropriate arguments
        let call3 = Expr::Call {
            callee: id("complex_func"),
            args: vec![
                Expr::Var(id("x")),
                Expr::Var(id("x")),
                Expr::Var(id("flag")),
            ],
        };
        assert!(matches!(typeck_expr(&call3, &ctx), Ok(Type::NumT(range)) if range == (0..20)));

        // Test invalid function calls

        // Call undefined function
        let invalid_call1 = Expr::Call {
            callee: id("undefined_func"),
            args: vec![Expr::Var(id("x"))],
        };
        assert!(matches!(
            typeck_expr(&invalid_call1, &ctx),
            Err(TypeError::UndefinedFunction(_))
        ));

        // Call with wrong number of arguments
        let invalid_call2 = Expr::Call {
            callee: id("is_positive"),
            args: vec![Expr::Var(id("x")), Expr::Var(id("flag"))],
        };
        assert!(matches!(
            typeck_expr(&invalid_call2, &ctx),
            Err(TypeError::WrongNumberOfArguments {
                expected: 1,
                actual: 2
            })
        ));

        // Call with wrong argument type
        let invalid_call3 = Expr::Call {
            callee: id("is_positive"),
            args: vec![Expr::Var(id("flag"))],
        };
        assert!(matches!(
            typeck_expr(&invalid_call3, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn pattern_matching() {
        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("flag"), Type::BoolT);
        env.insert(id("sym"), Type::SymT);
        let ctx = TypeCtx {
            env,
            funcs: &Map::new(),
        };

        // Test pattern matching with numeric scrutinee
        let num_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![
                // Case 1: Match a specific number
                Case {
                    pattern: Pattern::Num(5),
                    guard: Expr::Bool(true),
                    result: Expr::Bool(true),
                },
                // Case 2: Match any number and bind to variable
                Case {
                    pattern: Pattern::Var(id("n")),
                    guard: Expr::Bool(true),
                    result: Expr::Bool(false),
                },
            ],
        };
        assert!(matches!(typeck_expr(&num_match, &ctx), Ok(Type::BoolT)));

        // Test pattern matching with boolean scrutinee
        let bool_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("flag"))),
            cases: vec![
                // Case 1: Match true
                Case {
                    pattern: Pattern::Bool(true),
                    guard: Expr::Bool(true),
                    result: Expr::Num(1, Type::NumT(0..10)),
                },
                // Case 2: Match false
                Case {
                    pattern: Pattern::Bool(false),
                    guard: Expr::Bool(true),
                    result: Expr::Num(0, Type::NumT(0..10)),
                },
            ],
        };
        assert!(
            matches!(typeck_expr(&bool_match, &ctx), Ok(Type::NumT(range)) if range == (0..10))
        );

        // Test pattern matching with symbol scrutinee
        let sym_match = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("sym"))),
            cases: vec![
                // Case 1: Match specific symbol
                Case {
                    pattern: Pattern::Sym(Symbol('a')),
                    guard: Expr::Bool(true),
                    result: Expr::Var(id("x")),
                },
                // Case 2: Match any symbol and bind to variable
                Case {
                    pattern: Pattern::Var(id("s")),
                    guard: Expr::Bool(true),
                    result: Expr::Var(id("x")),
                },
            ],
        };
        assert!(matches!(typeck_expr(&sym_match, &ctx), Ok(Type::NumT(range)) if range == (0..10)));

        // Test invalid pattern matching

        // Type mismatch between pattern and scrutinee
        let invalid_match1 = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![Case {
                pattern: Pattern::Bool(true), // Boolean pattern for numeric scrutinee
                guard: Expr::Bool(true),
                result: Expr::Bool(true),
            }],
        };
        assert!(matches!(
            typeck_expr(&invalid_match1, &ctx),
            Err(TypeError::MismatchPatternScrutinee { .. })
        ));

        // Type mismatch between case results
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
        assert!(matches!(
            typeck_expr(&invalid_match2, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));

        // Invalid guard type
        let invalid_match3 = Expr::Match {
            scrutinee: Box::new(Expr::Var(id("x"))),
            cases: vec![Case {
                pattern: Pattern::Var(id("n")),
                guard: Expr::Var(id("x")), // Numeric guard instead of boolean
                result: Expr::Bool(true),
            }],
        };
        assert!(matches!(
            typeck_expr(&invalid_match3, &ctx),
            Err(TypeError::TypeMismatch { .. })
        ));
    }

    #[test]
    fn stmt() {
        //test assign
        //make variables with types
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

        //test if
        let if1 = Stmt::If {
            cond: e4,
            true_branch: vec![tb1],
            false_branch: vec![fb1],
        };

        assert!(typeck_stmt(&if1, &ctx).is_ok());
    }

    // todo use the block field here directly, and pass in a block.
    // still needs a if block test, might have failed merge, I saw you had it somewhere
    #[test]
    fn block() {
        // maintain the same style of comments as the rest of the file
        // e.g Create a type environment with a few variables
        //make variables with types
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

        //make expressions
        let e1 = Expr::Num(1, Type::NumT(0..3));
        let e2 = Expr::Sym('x');
        let e3 = Expr::Bool(true);

        let s1 = Stmt::Assign(id("A"), e1.clone());
        let s2 = Stmt::Assign(id("B"), e2.clone());
        let s3 = Stmt::Assign(id("C"), e3.clone());

        let b = vec![s1, s2, s3];

        assert!(typeck_block(&b, &ctx).is_ok());

        /* Using the block field directly
        let block: Block = vec![
            Stmt::Assign(id("A"), e1.clone()),
            Stmt::Assign(id("B"), e2.clone()),
            Stmt::Assign(id("C"), e3.clone()),
        ];
        assert!(typeck_block(&block, &ctx).is_ok());
        */

        let err1 = Stmt::Assign(id("X"), e1);
        let err2 = Stmt::Assign(id("Y"), e2);
        let err3 = Stmt::Assign(id("Z"), e3);

        // todo use different names for err and if blocks
        let b = vec![err1, err2, err3];

        assert!(typeck_block(&b, &ctx).is_err());
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
        // Test a valid program with helper functions and explicit castings
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
        let result1 = typecheck_program(&program1);
        assert!(result1.is_ok());

        // Test a program with a function that returns a boolean
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
        let result2 = typecheck_program(&program2);
        assert!(result2.is_ok());

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
        let result3 = typecheck_program(&program3);
        assert!(result3.is_err());

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
        let result4 = typecheck_program(&program4);
        assert!(result4.is_err());

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
        let result5 = typecheck_program(&program5);
        assert!(result5.is_err());
    }
}
