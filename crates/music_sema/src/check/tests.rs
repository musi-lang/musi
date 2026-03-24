use music_builtins::types::BuiltinType;
use music_db::Db;
use music_found::{Interner, SourceMap};
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::queries::ResolveDb;

use crate::env::TypeEnv;
use crate::errors::SemaError;
use crate::type_check;
use crate::types::Ty;

fn check_source(source: &str) -> (TypeEnv, Vec<SemaError>) {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, _) = parse(&tokens, source, &mut interner);
    lower(&mut ast);
    let db = Db::new(ast, interner, SourceMap::default());
    let mut rdb = ResolveDb::new(db);
    rdb.seed_builtins();
    rdb.resolve_module();
    let (db, resolution, _) = rdb.finish();
    type_check(&db, &resolution, None)
}

/// Returns the type of the last top-level expression.
fn last_expr_ty(env: &TypeEnv) -> &Ty {
    let last = env
        .type_map
        .iter()
        .max_by_key(|(id, _)| id.raw())
        .map(|(_, &ty)| ty)
        .expect("no expressions in type_map");
    env.types.get(last)
}

#[test]
fn literal_int() {
    let (env, errors) = check_source("42");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(matches!(last_expr_ty(&env), Ty::Builtin(BuiltinType::Int)));
}

#[test]
fn literal_float() {
    let (env, errors) = check_source("2.72");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(matches!(
        last_expr_ty(&env),
        Ty::Builtin(BuiltinType::Float)
    ));
}

#[test]
fn literal_string() {
    let (env, errors) = check_source("\"hello\"");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(matches!(
        last_expr_ty(&env),
        Ty::Builtin(BuiltinType::String)
    ));
}

#[test]
fn let_binding_produces_unit() {
    let (env, errors) = check_source("let x := 42");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // The let expression itself should be Unit
    let has_unit = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Unit));
    assert!(has_unit, "expected Unit type for let binding");
}

#[test]
fn seq_returns_last() {
    let (env, errors) = check_source("(1; 2; 3)");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // The Seq expression's type should be Int (the type of 3)
    let has_int = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Builtin(BuiltinType::Int)));
    assert!(has_int, "expected Int type in seq");
}

#[test]
fn tuple_literal() {
    let (env, errors) = check_source("(1, 2.0)");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_tuple = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Tuple(_)));
    assert!(has_tuple, "expected Tuple type");
}

#[test]
fn array_literal_uniform() {
    let (env, errors) = check_source("[1, 2, 3]");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_array = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Array(_)));
    assert!(has_array, "expected Array type");
}

#[test]
fn binop_add_int() {
    let (env, errors) = check_source("1 + 2");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_int = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Builtin(BuiltinType::Int)));
    assert!(has_int, "expected Int type for addition");
}

#[test]
fn binop_eq_produces_bool() {
    let (env, errors) = check_source("1 = 2");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_bool = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Builtin(BuiltinType::Bool)));
    assert!(has_bool, "expected Bool type for equality");
}

#[test]
fn branch_unifies_arms() {
    let (env, errors) = check_source("(1 if .True | 2 if _)");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_int = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Builtin(BuiltinType::Int)));
    assert!(has_int, "expected Int type for branch");
}

#[test]
fn lambda_produces_arrow() {
    let (env, errors) = check_source("(x) => x");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_arrow = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Arrow { .. }));
    assert!(has_arrow, "expected Arrow type for lambda");
}

#[test]
fn assign_produces_unit() {
    let (env, errors) = check_source("let mut x := 1; x <- 2");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_unit = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Unit));
    assert!(has_unit, "expected Unit type for assignment");
}

#[test]
fn return_produces_never() {
    let (env, errors) = check_source("return 42");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    let has_never = env
        .type_map
        .values()
        .any(|&ty| matches!(env.types.get(ty), Ty::Never));
    assert!(has_never, "expected Never type for return");
}

#[test]
fn dispatch_recorded_for_binop() {
    let (env, _errors) = check_source("1 + 2");
    assert!(!env.dispatch.is_empty(), "expected dispatch info for binop");
}
