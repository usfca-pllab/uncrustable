use uncrustable::syntax::{id, Expr, Map, Type};
use uncrustable::typecheck::{typeck_expr, TypeError};

#[test]
fn test_typeck_bool() {
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
fn test_bool_in_context() {
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
fn undefined_var() {
    // Create an empty type environment
    let env = Map::new();

    // Try to access an undefined boolean variable
    let undefined_expr = Expr::Var(id("undefined_flag"));

    // Should result in an error, without specifying the exact error type
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
