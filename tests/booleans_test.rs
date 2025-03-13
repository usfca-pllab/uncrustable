use uncrustable::syntax::{id, Expr, Map, Type};
use uncrustable::typecheck::TypeError;

#[test]
fn test_typeck_bool() {
    // Create a type environment
    let env = Map::new();

    // Test boolean literals
    let true_expr = Expr::Bool(true);
    let false_expr = Expr::Bool(false);

    // Both should be of type BoolT
    assert!(matches!(true_expr.typeck_expr(&env), Ok(Type::BoolT)));
    assert!(matches!(false_expr.typeck_expr(&env), Ok(Type::BoolT)));
}

#[test]
fn test_bool_in_context() {
    // Create a type environment with a boolean variable
    let mut env = Map::new();
    env.insert(id("flag"), Type::BoolT);

    // Test variable lookup for boolean variable
    let flag_expr = Expr::Var(id("flag"));
    assert!(matches!(flag_expr.typeck_expr(&env), Ok(Type::BoolT)));

    // Test boolean literal in the same context
    let bool_expr = Expr::Bool(true);
    assert!(matches!(bool_expr.typeck_expr(&env), Ok(Type::BoolT)));
}

#[test]
fn test_undefined_bool_var() {
    // Create an empty type environment
    let env = Map::new();

    // Try to access an undefined boolean variable
    let undefined_expr = Expr::Var(id("undefined_flag"));

    // Should result in an UndefinedVariable error
    assert!(matches!(
        undefined_expr.typeck_expr(&env),
        Err(TypeError::UndefinedVariable(var)) if var == id("undefined_flag")
    ));

    // Create an environment with some variables, but not the one we'll try to access
    let mut env_with_other_vars = Map::new();
    env_with_other_vars.insert(id("existing_flag"), Type::BoolT);
    env_with_other_vars.insert(id("count"), Type::NumT(0..100));

    // Try to access a different undefined variable
    let another_undefined_expr = Expr::Var(id("another_flag"));

    // Should also result in an UndefinedVariable error
    assert!(matches!(
        another_undefined_expr.typeck_expr(&env_with_other_vars),
        Err(TypeError::UndefinedVariable(var)) if var == id("another_flag")
    ));
}
