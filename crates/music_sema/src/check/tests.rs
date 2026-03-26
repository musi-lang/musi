use std::path::PathBuf;

use music_builtins::types::BuiltinType;
use music_db::Db;
use music_found::{Interner, SourceMap};
use music_hir::lower;
use music_lex::Lexer;
use music_parse::parse;
use music_resolve::queries::ResolveDb;

use crate::env::TypeEnv;
use crate::errors::{SemaError, SemaErrorKind};
use crate::type_check;
use crate::types::Ty;

fn check_source(source: &str) -> (TypeEnv, Vec<SemaError>) {
    let mut interner = Interner::new();
    let (tokens, _) = Lexer::new(source).lex();
    let (mut ast, _) = parse(&tokens, source, &mut interner);
    lower(&mut ast);
    let db = Db::new(ast, interner, SourceMap::default());
    let mut rdb = ResolveDb::new(db, PathBuf::new());
    rdb.seed_builtins();
    rdb.resolve_module();
    let (db, resolution, _) = rdb.finish();
    let (_, _, env, errors) = type_check(db, resolution, None);
    (env, errors)
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
    let (env, errors) = check_source("let _x := 42");
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
        .any(|&ty| matches!(env.types.get(ty), Ty::Empty));
    assert!(has_never, "expected Empty type for return");
}

#[test]
fn dispatch_recorded_for_binop() {
    let (env, _errors) = check_source("1 + 2");
    assert!(!env.dispatch.is_empty(), "expected dispatch info for binop");
}

// --- Fix 1: type annotation checking ---

#[test]
fn type_annotation_mismatch() {
    let (_, errors) = check_source("let x : Int := \"hello\";");
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        SemaErrorKind::TypeMismatch { .. } | SemaErrorKind::CannotUnify { .. }
    ));
}

#[test]
fn type_annotation_match() {
    let (_, errors) = check_source("let _x : Int := 42;");
    assert!(errors.is_empty(), "errors: {errors:?}");
}

// --- Fix 2: arity mismatch ---

#[test]
fn arity_mismatch_too_many_args() {
    let (_, errors) = check_source("let f (x : Int) : Int := x; f(1, 2);");
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        SemaErrorKind::ArityMismatch { .. }
    ));
}

// --- Fix 3: field access on non-record ---

#[test]
fn field_access_on_non_record() {
    let (_, errors) = check_source("let x := 42; x.foo;");
    assert!(!errors.is_empty());
    assert!(matches!(
        errors[0].kind,
        SemaErrorKind::UndefinedField { .. }
    ));
}

// --- Semantic check: export only at top level (#1) ---

#[test]
fn export_inside_function() {
    let (_, errors) = check_source("let _f := (x) => (export let _y := x;)");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::ExportNotTopLevel)),
        "expected ExportNotTopLevel, got: {errors:?}"
    );
}

// --- Semantic check: opaque requires export (#2) ---

#[test]
fn export_opaque_no_error() {
    let (_, errors) = check_source("export opaque let _T := record { x : Int }");
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::OpaqueWithoutExport)),
        "export opaque should not produce OpaqueWithoutExport: {errors:?}"
    );
}

// --- Semantic check: foreign only at top level (#3) ---

#[test]
fn foreign_inside_function() {
    let (_, errors) = check_source("let _f := (x) => (foreign \"C\" let _g := x;)");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::ForeignNotTopLevel)),
        "expected ForeignNotTopLevel, got: {errors:?}"
    );
}

// --- Semantic check: assign to non-lvalue (#7) ---

#[test]
fn assign_to_non_lvalue() {
    let (_, errors) = check_source("42 <- 1");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::InvalidAssignTarget)),
        "expected InvalidAssignTarget, got: {errors:?}"
    );
}

// --- Semantic check: assign to immutable (#6) ---

#[test]
fn assign_to_immutable() {
    let (_, errors) = check_source("let x := 1; x <- 2");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::MutabilityViolation)),
        "expected MutabilityViolation, got: {errors:?}"
    );
}

// --- Semantic check: splice outside quote (#19) ---

#[test]
fn splice_outside_quote() {
    let (_, errors) = check_source("#x");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::SpliceOutsideQuote)),
        "expected SpliceOutsideQuote, got: {errors:?}"
    );
}

// --- Semantic check: unreachable code after return (#22) ---

#[test]
fn unreachable_after_return() {
    let (_, errors) = check_source("(return 1; 42)");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::UnreachableCode)),
        "expected UnreachableCode, got: {errors:?}"
    );
}

// --- Semantic check: unreachable pattern after wildcard (#18) ---

#[test]
fn unreachable_pattern_after_wildcard() {
    let (_, errors) = check_source("case 1 of (| _ => 0 | _ => 1)");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::UnreachablePattern)),
        "expected UnreachablePattern, got: {errors:?}"
    );
}

// --- Semantic check: unused binding (#20) ---

#[test]
fn unused_binding_reported() {
    let (_, errors) = check_source("let x := 42;");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::UnusedBinding { .. })),
        "expected UnusedBinding, got: {errors:?}"
    );
}

#[test]
fn underscore_prefixed_binding_not_reported() {
    let (_, errors) = check_source("let _x := 42;");
    assert!(
        !errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::UnusedBinding { .. })),
        "underscore-prefixed bindings should not trigger UnusedBinding: {errors:?}"
    );
}

// --- Semantic check: index on non-indexable (#5) ---

#[test]
fn index_on_non_indexable() {
    let (_, errors) = check_source("let _x := 42; _x.[0];");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::NotIndexable)),
        "expected NotIndexable, got: {errors:?}"
    );
}

// --- Semantic check: non-exhaustive match (#1) ---

#[test]
fn non_exhaustive_match() {
    let (_, errors) = check_source("case 42 of (| 1 => .True)");
    assert!(
        errors
            .iter()
            .any(|e| matches!(e.kind, SemaErrorKind::NonExhaustiveMatch)),
        "expected NonExhaustiveMatch, got: {errors:?}"
    );
}

#[test]
fn exhaustive_match_with_wildcard() {
    let (_, errors) = check_source("case 42 of (| 1 => .True | _ => .False)");
    let has_non_exhaustive = errors
        .iter()
        .any(|e| matches!(e.kind, SemaErrorKind::NonExhaustiveMatch));
    assert!(
        !has_non_exhaustive,
        "wildcard arm should make match exhaustive: {errors:?}"
    );
}

#[test]
fn exhaustive_match_with_bind() {
    let (_, errors) = check_source("case 42 of (| 1 => .True | _x => .False)");
    let has_non_exhaustive = errors
        .iter()
        .any(|e| matches!(e.kind, SemaErrorKind::NonExhaustiveMatch));
    assert!(
        !has_non_exhaustive,
        "bind pattern should make match exhaustive: {errors:?}"
    );
}

// --- Semantic check: instance coherence (#4) ---

#[test]
fn duplicate_instance_detected() {
    let (_, errors) = check_source(
        "let Eq := class { let eq (a : Int, b : Int) : Bool }; \
         let _i1 := instance Eq of Int { let eq (a, b) := .True }; \
         let _i2 := instance Eq of Int { let eq (a, b) := .False }",
    );
    let has_dup = errors
        .iter()
        .any(|e| matches!(e.kind, SemaErrorKind::DuplicateInstance { .. }));
    assert!(has_dup, "expected DuplicateInstance, got: {errors:?}");
}

#[test]
fn intrinsic_method_dispatch_recorded() {
    use crate::env::DispatchInfo;
    use music_il::opcode::Opcode;
    let (env, _errors) = check_source(
        "let Bits [T] := class { \
             @builtin(opcode := 0x23) \
             let shl (a : T, n : Int) : T \
         }; \
         shl(1, 2)",
    );
    let has_static = env
        .dispatch
        .values()
        .any(|d| matches!(d, DispatchInfo::Static { opcode } if *opcode == Opcode::Shl));
    assert!(
        has_static,
        "expected Static dispatch for shl, got: {:?}",
        env.dispatch
    );
}

#[test]
fn intrinsic_method_positional_opcode() {
    use crate::env::DispatchInfo;
    use music_il::opcode::Opcode;
    let (env, _errors) = check_source(
        "let Bits [T] := class { \
             @builtin(0x24) \
             let shr (a : T, n : Int) : T \
         }; \
         shr(1, 1)",
    );
    let has_static = env
        .dispatch
        .values()
        .any(|d| matches!(d, DispatchInfo::Static { opcode } if *opcode == Opcode::Shr));
    assert!(
        has_static,
        "expected Static dispatch for shr, got: {:?}",
        env.dispatch
    );
}
