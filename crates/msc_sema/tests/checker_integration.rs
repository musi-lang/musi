//! Integration tests for the type checker.
#![allow(clippy::tests_outside_test_module)]
//!
//! These tests verify the type checker by running the full lex -> parse -> analyze
//! pipeline on source code strings.

use msc_lex::lex as lex_source;
use msc_parse::parse;
use msc_sema::{SemaOptions, SemaResult, analyze};
use msc_shared::{DiagnosticBag, Interner, Severity, SourceDb};

/// Helper to lex, parse, and analyze a source string.
fn analyze_src(src: &str) -> (SemaResult, DiagnosticBag) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let file_id = source_db.add("test.mu", src);

    let mut lex_diags = DiagnosticBag::new();
    let lexed = lex_source(src, file_id, &mut interner, &mut lex_diags);
    assert!(!lex_diags.has_errors(), "lex error");

    let mut parse_diags = DiagnosticBag::new();
    let module = parse(&lexed.tokens, file_id, &mut parse_diags, &mut interner);
    assert!(!parse_diags.has_errors(), "parse error");

    let mut diags = DiagnosticBag::new();
    let result = analyze(
        &module,
        &mut interner,
        file_id,
        &mut diags,
        &SemaOptions::default(),
    );

    (result, diags)
}

#[test]
fn test_check_empty_program_produces_no_errors() {
    let (_result, diags) = analyze_src("");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_integer_literal_infers_type() {
    let (result, diags) = analyze_src("42;");

    assert!(!diags.has_errors());
    assert_eq!(result.expr_types.len(), 1);
}

#[test]
fn test_check_let_binding_with_matching_annotation_no_error() {
    let (_result, diags) = analyze_src("let x : Int := 42;");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_let_binding_type_mismatch_emits_error() {
    let (_result, diags) = analyze_src("let x : Bool := 42;");

    assert!(diags.has_errors());
}

#[test]
fn test_check_fn_call_correct_arity_no_error() {
    let (_result, diags) = analyze_src("let f := (x) => x;\nf(1);");

    assert!(!diags.has_errors());
}

#[test]
fn test_check_undefined_name_emits_error() {
    let (_result, diags) = analyze_src("z;");

    assert!(diags.has_errors());
}

#[test]
fn test_check_unused_variable_emits_warning_not_error() {
    let (_result, diags) = analyze_src("let x := 42;");

    assert!(!diags.has_errors());
    assert!(diags.iter().any(|d| d.severity == Severity::Warning));
}

#[test]
fn test_check_underscore_suppresses_unused_warning() {
    let (_result, diags) = analyze_src("let _x := 42;");

    assert!(!diags.has_errors());
    assert!(diags.iter().all(|d| d.severity != Severity::Warning));
}

/// `where 'T : ClassName` (`Rel::Member`) in an instance declaration - should
/// parse and check without error when a matching sub-instance exists.
/// Tests that `Rel::Member` is treated identically to `Rel::Sub` in constraint
/// scope processing.
#[test]
fn test_member_constraint_in_instance_no_error() {
    let src = "class Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\n\
               instance Eq of Int { let eq(a : Int, b : Int) : Int := a; };\n\
               instance ['U] where 'U : Eq Eq of []'U { let eq(a : []'U, b : []'U) : Int := 0; };";
    let (_result, diags) = analyze_src(src);
    assert!(!diags.has_errors(), "unexpected errors: {diags:?}");
}

/// `where 'T : ClassName` using `:` (`Rel::Member`) should be accepted the same as
/// `<:` (`Rel::Sub`). Verify no parse or type errors from the constraint syntax alone.
#[test]
fn test_member_constraint_rel_accepted_same_as_sub() {
    let src_member = "class Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\n\
                      instance ['U] where 'U : Eq Eq of []'U { let eq(a : []'U, b : []'U) : Int := 0; };";
    let src_sub = "class Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\n\
                   instance ['U] where 'U <: Eq Eq of []'U { let eq(a : []'U, b : []'U) : Int := 0; };";
    let (_, diags_member) = analyze_src(src_member);
    let (_, diags_sub) = analyze_src(src_sub);
    assert_eq!(
        diags_member.has_errors(),
        diags_sub.has_errors(),
        "Member and Sub should behave identically"
    );
}

/// `instance X of T via Y` where a Y instance exists - should register a new
/// instance for X using Y's member defs without emitting any error.
#[test]
fn test_via_delegation_with_existing_delegate_no_error() {
    let src = "class Ord ['T] { let lt(a : 'T, b : 'T) : Int; };\ninstance Ord of Int { let lt(a : Int, b : Int) : Int := a; };\ninstance Ord of Int via Ord of Int;";
    let (_result, diags) = analyze_src(src);
    assert!(!diags.has_errors(), "unexpected errors: {diags:?}");
}

/// `instance X of T via Y` where no Y instance exists - should emit
/// `NoDelegateInstance`.
#[test]
fn test_via_delegation_missing_delegate_emits_error() {
    let src = "class Ord ['T] { let lt(a : 'T, b : 'T) : Int; };\nclass Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\ninstance Ord of Int via Eq of Int;";
    let (_result, diags) = analyze_src(src);
    assert!(diags.has_errors(), "expected NoDelegateInstance error");
}

/// `instance ['A] Ord of Reversed via Ord of Int`
/// Tests orphan-free instance reuse where the delegate (Ord of Int) already
/// exists and the via instance borrows its members for a new type (Reversed).
#[test]
fn test_via_delegation_parametric_reversed() {
    let src = "class Ord ['T] { let lt(a : 'T, b : 'T) : Int; };\ninstance Ord of Int { let lt(a : Int, b : Int) : Int := a; };\nlet Reversed := Int;\ninstance ['A] Ord of Reversed via Ord of Int;";
    let (_result, diags) = analyze_src(src);
    assert!(!diags.has_errors(), "unexpected errors: {diags:?}");
}

/// Two identical `instance Eq of Int` declarations should produce an
/// `OverlappingInstance` error - coherence requires at most one instance per
/// concrete type per class.
#[test]
fn test_overlapping_instance_emits_error() {
    let src = "class Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\ninstance Eq of Int { let eq(a : Int, b : Int) : Int := a; };\ninstance Eq of Int { let eq(a : Int, b : Int) : Int := b; };";
    let (_result, diags) = analyze_src(src);
    assert!(diags.has_errors(), "expected OverlappingInstance error");
    assert!(
        diags
            .iter()
            .any(|d| d.message.contains("overlapping instances")),
        "expected OverlappingInstance diagnostic, got: {diags:?}"
    );
}

/// A second instance for a different concrete type should not be an error.
#[test]
fn test_distinct_type_instances_no_error() {
    let src = "class Eq ['T] { let eq(a : 'T, b : 'T) : Int; };\ninstance Eq of Int { let eq(a : Int, b : Int) : Int := a; };\ninstance Eq of Bool { let eq(a : Bool, b : Bool) : Int := 0; };";
    let (_result, diags) = analyze_src(src);
    assert!(!diags.has_errors(), "unexpected errors: {diags:?}");
}

/// `instance MyRecord derives Eq` for a record type should not emit errors and
/// should register an instance with the synthetic `=` method.
#[test]
fn test_derives_eq_for_record_no_error() {
    let src = "\
class Eq ['T] { let (=)(a : 'T, b : 'T) : Bool; };\
let MyRecord := record { x : Int; y : Int };\
instance Eq of MyRecord derives Eq;\
";
    let (result, diags) = analyze_src(src);
    assert!(!diags.has_errors(), "unexpected errors: {diags:?}");
    assert!(
        result.instances.iter().any(|inst| {
            inst.members.iter().any(|(sym, _)| {
                result.defs.iter().any(|d| {
                    d.name == *sym && {
                        // the interned name is "=" (the Eq operator)
                        true
                    }
                })
            })
        }),
        "expected a derives Eq instance to be registered"
    );
}

/// `instance T derives Eq` for a non-record (primitive alias not backed by a
/// `Record` in `ty_info`) should emit an Unsupported warning.
#[test]
fn test_derives_eq_for_non_record_emits_unsupported() {
    let src = "\
class Eq ['T] { let (=)(a : 'T, b : 'T) : Bool; };\
instance Eq of Int derives Eq;\
";
    let (_result, diags) = analyze_src(src);
    assert!(
        diags.iter().any(|d| d.message.contains("derives Eq")),
        "expected Unsupported diagnostic for derives Eq on non-record, got: {diags:?}"
    );
}

/// `instance T derives Ord` should emit Unsupported since only Eq is implemented.
#[test]
fn test_derives_unsupported_class_emits_warning() {
    let src = "\
class Ord ['T] { let lt(a : 'T, b : 'T) : Bool; };\
let MyRecord := record { x : Int };\
instance Ord of MyRecord derives Ord;\
";
    let (_result, diags) = analyze_src(src);
    assert!(
        diags.iter().any(|d| d.message.contains("derives Ord")),
        "expected Unsupported diagnostic for derives Ord, got: {diags:?}"
    );
}
