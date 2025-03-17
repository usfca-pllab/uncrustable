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
    }
}

/// Type environment mapping variables to their types
type TypeEnv = Map<Id, Type>;

/// Function environment mapping function names to their definitions
type FunctionEnv = Map<Id, Function>;

/// Type check an expression in a given environment
pub fn typeck_expr(
    expr: &Expr,
    env: &TypeEnv,
    function_env: &FunctionEnv,
) -> Result<Type, TypeError> {
    match expr {
        // Variables are always of the type of the variable in the environment
        Expr::Var(id) => {
            // Get the type of the variable from the environment
            env.get(id)
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
            let lhs_type = typeck_expr(lhs, env)?;
            let rhs_type = typeck_expr(rhs, env)?;
            if lhs_type != rhs_type {
                return Err(TypeError::TypeMismatch {
                    expected: lhs_type,
                    actual: rhs_type,
                });
            }
            match op {
                // Arithmetic operations are always of type NumT(0..1)
                BOp::Add | BOp::Sub | BOp::Mul | BOp::Div | BOp::Rem | BOp::Shl | BOp::Shr => {
                    if let Type::NumT(_) = lhs_type {
                        Ok(lhs_type)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::NumT(0..1),
                            actual: lhs_type,
                        })
                    }
                }
                // Comparison operations are always of type BoolT
                BOp::Lt | BOp::Lte | BOp::Eq | BOp::Ne => {
                    if let Type::NumT(_) = lhs_type {
                        Ok(Type::BoolT)
                    } else {
                        Err(TypeError::TypeMismatch {
                            expected: Type::NumT(0..1),
                            actual: lhs_type,
                        })
                    }
                }
                // Logical operations are always of type BoolT
                BOp::And | BOp::Or => {
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
            let inner_type = typeck_expr(inner, env)?;
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
            let inner_type = typeck_expr(inner, env)?;

            // According to the [T-Cast] rule, both the inner expression and the target type
            // must be numeric types with bounds
            match (&inner_type, typ) {
                (Type::NumT(_), Type::NumT(_)) => {
                    // The cast is valid, return the target type
                    Ok(typ.clone())
                },
                (_, Type::NumT(_)) => {
                    // The inner expression is not a numeric type
                    Err(TypeError::TypeMismatch {
                        expected: Type::NumT(0..1), // Example numeric type
                        actual: inner_type,
                    })
                },
                (Type::NumT(_), _) => {
                    // The target type is not a numeric type
                    Err(TypeError::TypeMismatch {
                        expected: Type::NumT(0..1), // Example numeric type
                        actual: typ.clone(),
                    })
                },
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
                arg_types.push(typeck_expr(arg, env, function_env)?);
            }

            // Look up the function in the function environment
            let function = function_env
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
            todo!()
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{id, Expr, Map, Type};

    #[test]
    fn variables() {
        // Create a type environment with a few variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("b"), Type::BoolT);
        env.insert(id("s"), Type::SymT);

        // Test variable lookup for existing variables
        let x_expr = Expr::Var(id("x"));
        let b_expr = Expr::Var(id("b"));
        let s_expr = Expr::Var(id("s"));

        assert!(matches!(typeck_expr(&x_expr, &env), Ok(Type::NumT(range)) if range == (0..10)));
        assert!(matches!(typeck_expr(&b_expr, &env), Ok(Type::BoolT)));
        assert!(matches!(typeck_expr(&s_expr, &env), Ok(Type::SymT)));

        // Test variable lookup for non-existent variable
        let unknown_expr = Expr::Var(id("unknown"));
        assert!(typeck_expr(&unknown_expr, &env).is_err());
    }

    #[test]
    fn undefined_variables() {
        // Create an empty type environment
        let env = Map::new();

        // Try to access an undefined variable
        let undefined_expr = Expr::Var(id("undefined_flag"));

        // Should result in an error
        assert!(typeck_expr(&undefined_expr, &env).is_err());

        // Create an environment with some variables, but not the one we'll try to access
        let mut env = Map::new();
        env.insert(id("existing_flag"), Type::BoolT);
        env.insert(id("count"), Type::NumT(0..100));

        // Try to access a different undefined variable
        let another_undefined_expr = Expr::Var(id("another_flag"));

        // Should also result in an error
        assert!(typeck_expr(&another_undefined_expr, &env).is_err());
    }

    #[test]
    fn literals() {
        let env = Map::new();

        // Test boolean literals
        assert!(matches!(
            typeck_expr(&Expr::Bool(true), &env),
            Ok(Type::BoolT)
        ));
        assert!(matches!(
            typeck_expr(&Expr::Bool(false), &env),
            Ok(Type::BoolT)
        ));

        // Test numeric literals with different ranges
        let ranges = [(0..10), (0..100), (-10..10)];
        let values = [5, 42, -5];

        for (_i, (range, value)) in ranges.iter().zip(values.iter()).enumerate() {
            let num = Expr::Num(*value, Type::NumT(range.clone()));
            assert!(matches!(typeck_expr(&num, &env), Ok(Type::NumT(r)) if r == *range));
        }

        // Test symbol literals
        assert!(matches!(typeck_expr(&Expr::Sym('a'), &env), Ok(Type::SymT)));
    }

    #[test]
    fn binary_operations() {
        // Create a type environment
        let env = Map::new();

        // Define test types and expressions
        let num_type1 = Type::NumT(0..1);
        let num_type2 = Type::NumT(0..10);
        let num_type3 = Type::NumT(-5..5);

        let num1 = Expr::Num(0, num_type1.clone());
        let num2 = Expr::Num(5, num_type2.clone());
        let num3 = Expr::Num(-2, num_type3.clone());
        let bool_expr = Expr::Bool(true);
        let false_expr = Expr::Bool(false);

        // Test arithmetic operations
        let arithmetic_ops = [
            BOp::Add,
            BOp::Sub,
            BOp::Mul,
            BOp::Div,
            BOp::Rem,
            BOp::Shl,
            BOp::Shr,
        ];
        let test_cases = [
            (&num1, &num_type1, &Expr::Num(1, num_type1.clone())),
            (&num2, &num_type2, &Expr::Num(3, num_type2.clone())),
            (&num3, &num_type3, &Expr::Num(0, num_type3.clone())),
        ];

        for op in &arithmetic_ops {
            for (lhs, expected_type, rhs) in &test_cases {
                let bin_op = Expr::BinOp {
                    lhs: Box::new((*lhs).clone()),
                    op: op.clone(),
                    rhs: Box::new((*rhs).clone()),
                };

                let expected_range = match expected_type {
                    Type::NumT(range) => range.clone(),
                    _ => panic!("Expected numeric type"),
                };

                assert!(
                    matches!(typeck_expr(&bin_op, &env), Ok(Type::NumT(range)) if range == expected_range)
                );
            }
        }

        // Test comparison operations
        let comparison_ops = [BOp::Lt, BOp::Lte, BOp::Eq, BOp::Ne];

        for op in &comparison_ops {
            for (lhs, _, rhs) in &test_cases {
                let bin_op = Expr::BinOp {
                    lhs: Box::new((*lhs).clone()),
                    op: op.clone(),
                    rhs: Box::new((*rhs).clone()),
                };

                assert!(matches!(typeck_expr(&bin_op, &env), Ok(Type::BoolT)));
            }
        }

        // Test logical operations
        let logical_ops = [BOp::And, BOp::Or];

        for op in &logical_ops {
            let bin_op = Expr::BinOp {
                lhs: Box::new(bool_expr.clone()),
                op: op.clone(),
                rhs: Box::new(false_expr.clone()),
            };

            assert!(matches!(typeck_expr(&bin_op, &env), Ok(Type::BoolT)));
        }

        // Test type mismatches
        let invalid_add = Expr::BinOp {
            lhs: Box::new(num1.clone()),
            op: BOp::Add,
            rhs: Box::new(bool_expr.clone()),
        };
        assert!(typeck_expr(&invalid_add, &env).is_err());

        let invalid_type_add = Expr::BinOp {
            lhs: Box::new(num1.clone()),
            op: BOp::Add,
            rhs: Box::new(num2.clone()),
        };
        assert!(typeck_expr(&invalid_type_add, &env).is_err());

        let invalid_and = Expr::BinOp {
            lhs: Box::new(bool_expr.clone()),
            op: BOp::And,
            rhs: Box::new(num1.clone()),
        };
        assert!(typeck_expr(&invalid_and, &env).is_err());

        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("y"), Type::NumT(0..10));
        env.insert(id("z"), Type::NumT(-5..5));
        env.insert(id("flag1"), Type::BoolT);
        env.insert(id("flag2"), Type::BoolT);

        // Create variable expressions
        let x_expr = Expr::Var(id("x"));
        let y_expr = Expr::Var(id("y"));
        let z_expr = Expr::Var(id("z"));
        let flag1_expr = Expr::Var(id("flag1"));
        let flag2_expr = Expr::Var(id("flag2"));

        // Test arithmetic, comparison, and logical operations
        let operations: [(Box<Expr>, BOp, Box<Expr>, Result<Type, TypeError>); 4] = [
            // Arithmetic with same range (x + y)
            (
                Box::new(x_expr.clone()),
                BOp::Add,
                Box::new(y_expr.clone()),
                Ok(Type::NumT(0..10)),
            ),
            // Arithmetic with same range (z - z)
            (
                Box::new(z_expr.clone()),
                BOp::Sub,
                Box::new(z_expr.clone()),
                Ok(Type::NumT(-5..5)),
            ),
            // Comparison (x < y)
            (
                Box::new(x_expr.clone()),
                BOp::Lt,
                Box::new(y_expr.clone()),
                Ok(Type::BoolT),
            ),
            // Logical (flag1 && flag2)
            (
                Box::new(flag1_expr.clone()),
                BOp::And,
                Box::new(flag2_expr.clone()),
                Ok(Type::BoolT),
            ),
        ];

        for (lhs, op, rhs, expected) in operations {
            let bin_op = Expr::BinOp { lhs, op, rhs };

            match expected {
                Ok(Type::BoolT) => assert!(matches!(typeck_expr(&bin_op, &env), Ok(Type::BoolT))),
                Ok(Type::NumT(range)) => {
                    assert!(matches!(typeck_expr(&bin_op, &env), Ok(Type::NumT(r)) if r == range))
                }
                _ => panic!("Unexpected expected type"),
            }
        }

        // Test type errors
        let invalid_operations: [(Box<Expr>, BOp, Box<Expr>); 2] = [
            // Type mismatch (x + flag1)
            (
                Box::new(x_expr.clone()),
                BOp::Add,
                Box::new(flag1_expr.clone()),
            ),
            // Different ranges (x + z)
            (Box::new(x_expr.clone()), BOp::Add, Box::new(z_expr.clone())),
        ];

        for (lhs, op, rhs) in invalid_operations {
            let bin_op = Expr::BinOp { lhs, op, rhs };
            assert!(typeck_expr(&bin_op, &env).is_err());
        }
    }

    #[test]
    fn unary_operations() {
        // Create a type environment with variables
        let mut env = Map::new();
        env.insert(id("x"), Type::NumT(0..10));
        env.insert(id("flag"), Type::BoolT);

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
                Ok(Type::BoolT) => assert!(matches!(typeck_expr(&unary_op, &env), Ok(Type::BoolT))),
                Ok(Type::NumT(range)) => {
                    assert!(matches!(typeck_expr(&unary_op, &env), Ok(Type::NumT(r)) if r == range))
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
            assert!(typeck_expr(&unary_op, &env).is_err());
        }
    }
}
