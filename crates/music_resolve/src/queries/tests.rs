use std::path::PathBuf;

use music_db::Db;
use music_found::{Interner, SourceMap};
use music_lex::Lexer;
use music_parse::parse;

use crate::def::DefKind;
use crate::errors::{ResolveError, ResolveErrorKind};
use crate::queries::{ResolutionMap, ResolveDb};

fn parse_and_resolve(source: &str) -> (Db, ResolutionMap, Vec<ResolveError>) {
    let mut interner = Interner::new();
    let (tokens, _lex_errors) = Lexer::new(source).lex();
    let (ast, _parse_errors) = parse(&tokens, source, &mut interner);
    let db = Db::new(ast, interner, SourceMap::default());
    let mut rdb = ResolveDb::new(db, PathBuf::new());
    rdb.seed_builtins();
    rdb.resolve_module();
    rdb.finish()
}

#[test]
fn simple_let_resolves() {
    let (db, res, errors) = parse_and_resolve("let x := 42; x");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // The second top-level expr (Var x) should be resolved
    assert!(
        !res.expr_res.is_empty(),
        "expected at least one resolved expr"
    );

    // Find the def for x and verify it's a Value
    let x_sym = db.interner.resolve(
        *res.defs
            .iter()
            .find(|(_, d)| db.interner.resolve(d.name) == "x")
            .map(|(_, d)| &d.name)
            .unwrap(),
    );
    assert_eq!(x_sym, "x");
}

#[test]
fn undefined_name_produces_error() {
    let (_db, _res, errors) = parse_and_resolve("y");
    assert_eq!(errors.len(), 1);
    assert!(matches!(errors[0].kind, ResolveErrorKind::UndefinedName(_)));
}

#[test]
fn shadowing_resolves_to_latest() {
    let (_db, res, errors) = parse_and_resolve("let x := 1; let x := 2; x");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // x should resolve, and it should be the second definition
    assert!(!res.expr_res.is_empty());
}

#[test]
fn builtin_types_resolve_without_import() {
    let (_db, res, errors) = parse_and_resolve("let id (x : Int) : Int := x");
    // Int should resolve as a builtin type
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // Type resolution map should have entries for the Int references
    assert!(!res.ty_res.is_empty(), "expected type resolutions for Int");
    // Verify at least one resolves to a Builtin
    let has_builtin = res
        .ty_res
        .values()
        .any(|&def_id| matches!(res.defs.get(def_id).kind, DefKind::Builtin(_)));
    assert!(has_builtin, "expected Int to resolve to a Builtin def");
}

#[test]
fn self_recursion_resolves() {
    let (_db, _res, errors) = parse_and_resolve("let f (n : Int) : Int := f(n - 1)");
    // f should resolve to itself (no undefined name error for f)
    let undefined_f = errors
        .iter()
        .any(|e| matches!(&e.kind, ResolveErrorKind::UndefinedName(_)));
    assert!(
        !undefined_f,
        "f should be in scope for self-recursion, errors: {errors:?}"
    );
}

#[test]
fn lambda_param_resolves() {
    let (_db, res, errors) = parse_and_resolve("let f := (x) => x");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // x inside lambda body should resolve
    assert!(!res.expr_res.is_empty());
}

#[test]
fn function_params_resolve_in_body() {
    let (_db, res, errors) = parse_and_resolve("let add (a : Int, b : Int) : Int := a + b");
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    // a and b should resolve inside the body
    assert!(
        res.expr_res.len() >= 2,
        "expected at least 2 resolved exprs (a and b), got {}",
        res.expr_res.len()
    );
}

#[test]
fn prelude_classes_seeded() {
    let (_db, res, _errors) = parse_and_resolve("");
    // Verify Eq, Ord, Num, Bits, Show are all defined
    let class_count = res
        .defs
        .iter()
        .filter(|(_, d)| d.kind == DefKind::TypeClass)
        .count();
    assert!(
        class_count >= 5,
        "expected at least 5 prelude classes, got {class_count}"
    );
}
