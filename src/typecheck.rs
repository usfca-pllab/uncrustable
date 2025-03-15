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
}

/// Type environment mapping variables to their types
type TypeEnv = Map<Id, Type>;

/// Type check an expression in a given environment
pub fn typeck_expr(expr: &Expr, env: &TypeEnv) -> Result<Type, TypeError> {
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
        Expr::Sym(c) => {
            todo!()
        }
        // Binary operations are always of the type of the left hand side
        Expr::BinOp { lhs, op, rhs } => {
            todo!()
        }
        // Unary operations are always of the type of the inner expression
        Expr::UOp { op, inner } => {
            todo!()
        }
        // Casting is always of the type of the inner expression
        Expr::Cast {
            inner,
            typ,
            overflow,
        } => {
            todo!()
        }
        // Pattern matching is always of the type of the scrutinee
        Expr::Call { callee, args } => {
            todo!()
        }
        // Pattern matching is always of the type of the scrutinee
        Expr::Match { scrutinee, cases } => {
            todo!()
        }
    }
}

//TODO
// in progress, need to verify understanding and test
pub fn typeck_stmt(stmt: &Statement, env: &mut TypeEnv) -> Result<Type, TypeError> {
    // check if it is an expression, and a variable type
    // TODO: Γ ⊢ x = e ok
    // OK identifier = expression
    // or err
    let t = typeck_expr(&Statement, &TypeEnv);
    if t == TypeError {
        Err(t.err()) //CHECK: set error to the issue with the expression??
    } else {
        let updt = env.get(t); //CHECK: assuming t == id??
        Ok(env.insert(&Statement, updt)); //change id to expression
        env.remove(t);
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
    fn booleans() {
        // Create a type environment
        let env = Map::new();

        // Test boolean literals
        let true_expr = Expr::Bool(true);
        let false_expr = Expr::Bool(false);

        // Both should be of type BoolT
        assert!(matches!(typeck_expr(&true_expr, &env), Ok(Type::BoolT)));
        assert!(matches!(typeck_expr(&false_expr, &env), Ok(Type::BoolT)));
    }

    #[test]
    fn booleans_in_context() {
        // Create a type environment with a boolean variable
        let mut env = Map::new();
        env.insert(id("flag"), Type::BoolT);

        // Test variable lookup for boolean variable
        let flag_expr = Expr::Var(id("flag"));
        assert!(matches!(typeck_expr(&flag_expr, &env), Ok(Type::BoolT)));

        // Test boolean literal in the same context
        let bool_expr = Expr::Bool(true);
        assert!(matches!(typeck_expr(&bool_expr, &env), Ok(Type::BoolT)));
    }

    #[test]
    fn numbers() {
        // Create a type environment
        let env = Map::new();

        // Test numeric literals with different ranges
        let num1 = Expr::Num(5, Type::NumT(0..10));
        let num2 = Expr::Num(42, Type::NumT(0..100));
        let num3 = Expr::Num(-5, Type::NumT(-10..10));

        // Each should have the type specified in its constructor
        assert!(matches!(typeck_expr(&num1, &env), Ok(Type::NumT(range)) if range == (0..10)));
        assert!(matches!(typeck_expr(&num2, &env), Ok(Type::NumT(range)) if range == (0..100)));
        assert!(matches!(typeck_expr(&num3, &env), Ok(Type::NumT(range)) if range == (-10..10)));
    }

    #[test]
    fn numbers_in_context() {
        // Create a type environment with numeric variables
        let mut env = Map::new();
        env.insert(id("small"), Type::NumT(0..10));
        env.insert(id("large"), Type::NumT(0..1000));
        env.insert(id("signed"), Type::NumT(-100..100));

        // Test variable lookup for numeric variables
        let small_expr = Expr::Var(id("small"));
        let large_expr = Expr::Var(id("large"));
        let signed_expr = Expr::Var(id("signed"));

        assert!(
            matches!(typeck_expr(&small_expr, &env), Ok(Type::NumT(range)) if range == (0..10))
        );
        assert!(
            matches!(typeck_expr(&large_expr, &env), Ok(Type::NumT(range)) if range == (0..1000))
        );
        assert!(
            matches!(typeck_expr(&signed_expr, &env), Ok(Type::NumT(range)) if range == (-100..100))
        );

        // Test numeric literal in the same context
        let num_expr = Expr::Num(5, Type::NumT(0..10));
        assert!(matches!(typeck_expr(&num_expr, &env), Ok(Type::NumT(range)) if range == (0..10)));
    }
}
