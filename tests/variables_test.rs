use uncrustable::syntax::{id, Expr, Map, Type};
use uncrustable::typecheck::{typeck_expr, TypeError};

#[test]
fn typeck_var() {
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
