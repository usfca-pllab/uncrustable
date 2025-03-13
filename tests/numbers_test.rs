use uncrustable::syntax::{id, Expr, Map, Type};
use uncrustable::typecheck::{typeck_expr, TypeError};

#[test]
fn test_typeck_num() {
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
fn test_num_in_context() {
    // Create a type environment with numeric variables
    let mut env = Map::new();
    env.insert(id("small"), Type::NumT(0..10));
    env.insert(id("large"), Type::NumT(0..1000));
    env.insert(id("signed"), Type::NumT(-100..100));

    // Test variable lookup for numeric variables
    let small_expr = Expr::Var(id("small"));
    let large_expr = Expr::Var(id("large"));
    let signed_expr = Expr::Var(id("signed"));

    assert!(matches!(typeck_expr(&small_expr, &env), Ok(Type::NumT(range)) if range == (0..10)));
    assert!(matches!(typeck_expr(&large_expr, &env), Ok(Type::NumT(range)) if range == (0..1000)));
    assert!(matches!(typeck_expr(&signed_expr, &env), Ok(Type::NumT(range)) if range == (-100..100)));

    // Test numeric literal in the same context
    let num_expr = Expr::Num(5, Type::NumT(0..10));
    assert!(matches!(typeck_expr(&num_expr, &env), Ok(Type::NumT(range)) if range == (0..10)));
}

#[test]
fn undefined_var() {
    // Create an empty type environment
    let env = Map::new();

    // Try to access an undefined numeric variable
    let undefined_expr = Expr::Var(id("undefined_num"));

    // Should result in an UndefinedVariable error
    assert!(typeck_expr(&undefined_expr, &env).is_err());

    // Create an environment with some variables, but not the one we'll try to access
    let mut env = Map::new();
    env.insert(id("x"), Type::NumT(0..10));
    env.insert(id("flag"), Type::BoolT);

    // Try to access a different undefined variable
    let another_undefined_expr = Expr::Var(id("y"));

    // Should also result in an UndefinedVariable error
    assert!(typeck_expr(&another_undefined_expr, &env).is_err());

    // Test with a variable name that might be confused with a different type
    let type_confusion_expr = Expr::Var(id("bool_as_num"));

    // Should still result in an UndefinedVariable error
    assert!(typeck_expr(&type_confusion_expr, &env).is_err());
}
