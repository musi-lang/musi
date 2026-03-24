use music_found::{Interner, Span};

use crate::errors::{SemaError, SemaErrorKind};
use crate::types::Ty;

fn make_error(kind: SemaErrorKind) -> SemaError {
    SemaError {
        kind,
        span: Span::new(10, 20),
        context: None,
    }
}

fn make_error_with_context(kind: SemaErrorKind, context: &'static str) -> SemaError {
    SemaError {
        kind,
        span: Span::new(10, 20),
        context: Some(context),
    }
}

#[test]
fn span_accessor() {
    let err = make_error(SemaErrorKind::MissingAnnotation);
    assert_eq!(err.span(), Span::new(10, 20));
}

#[test]
fn display_type_mismatch() {
    use music_arena::Arena;
    let mut arena = Arena::<Ty>::new();
    let expected = arena.alloc(Ty::Unit);
    let found = arena.alloc(Ty::Never);
    let err = make_error(SemaErrorKind::TypeMismatch { expected, found });
    let msg = err.to_string();
    assert!(msg.contains("type mismatch"));
    assert!(msg.contains("expected"));
    assert!(msg.contains("found"));
}

#[test]
fn display_cannot_unify() {
    use music_arena::Arena;
    let mut arena = Arena::<Ty>::new();
    let left = arena.alloc(Ty::Unit);
    let right = arena.alloc(Ty::Never);
    let err = make_error(SemaErrorKind::CannotUnify { left, right });
    assert!(err.to_string().contains("cannot unify"));
}

#[test]
fn display_undefined_field() {
    let mut interner = Interner::new();
    let field = interner.intern("foo");
    let err = make_error(SemaErrorKind::UndefinedField { field });
    assert!(err.to_string().contains("undefined field"));
}

#[test]
fn display_missing_annotation() {
    let err = make_error(SemaErrorKind::MissingAnnotation);
    assert_eq!(err.to_string(), "missing type annotation");
}

#[test]
fn display_non_exhaustive_match() {
    let err = make_error(SemaErrorKind::NonExhaustiveMatch);
    assert_eq!(err.to_string(), "non-exhaustive match");
}

#[test]
fn display_purity_violation() {
    let mut interner = Interner::new();
    let effect = interner.intern("Console");
    let err = make_error(SemaErrorKind::PurityViolation { effect });
    assert!(err.to_string().contains("purity violation"));
}

#[test]
fn display_missing_handler() {
    let mut interner = Interner::new();
    let op = interner.intern("read");
    let err = make_error(SemaErrorKind::MissingHandler { op });
    assert!(err.to_string().contains("missing handler"));
}

#[test]
fn display_arity_mismatch() {
    let err = make_error(SemaErrorKind::ArityMismatch {
        expected: 2,
        found: 3,
    });
    let msg = err.to_string();
    assert!(msg.contains("arity mismatch"));
    assert!(msg.contains('2'));
    assert!(msg.contains('3'));
}

#[test]
fn display_no_instance() {
    let mut interner = Interner::new();
    let class = interner.intern("Num");
    let err = make_error(SemaErrorKind::NoInstance { class });
    assert!(err.to_string().contains("no instance"));
}

#[test]
fn display_mutability_violation() {
    let err = make_error(SemaErrorKind::MutabilityViolation);
    assert_eq!(err.to_string(), "mutability violation");
}

#[test]
fn display_invalid_assign_target() {
    let err = make_error(SemaErrorKind::InvalidAssignTarget);
    assert_eq!(err.to_string(), "invalid assignment target");
}

#[test]
fn display_not_callable() {
    let err = make_error(SemaErrorKind::NotCallable);
    assert!(err.to_string().contains("not callable"));
}

#[test]
fn display_not_indexable() {
    let err = make_error(SemaErrorKind::NotIndexable);
    assert!(err.to_string().contains("not indexable"));
}

#[test]
fn display_constraint_not_satisfied() {
    let mut interner = Interner::new();
    let constraint = interner.intern("Ord");
    let err = make_error(SemaErrorKind::ConstraintNotSatisfied { constraint });
    assert!(err.to_string().contains("constraint"));
}

#[test]
fn display_infinite_type() {
    let err = make_error(SemaErrorKind::InfiniteType);
    assert_eq!(err.to_string(), "infinite type");
}

#[test]
fn display_occurs_check() {
    let err = make_error(SemaErrorKind::OccursCheck { var: 7 });
    assert!(err.to_string().contains("occurs check"));
}

#[test]
fn display_with_context() {
    let err = make_error_with_context(SemaErrorKind::MissingAnnotation, "let binding");
    assert!(err.to_string().contains("in let binding"));
}

#[test]
fn error_equality() {
    let e1 = make_error(SemaErrorKind::MissingAnnotation);
    let e2 = make_error(SemaErrorKind::MissingAnnotation);
    assert_eq!(e1, e2);

    let e3 = make_error(SemaErrorKind::InfiniteType);
    assert_ne!(e1, e3);
}

#[test]
fn display_export_not_top_level() {
    let err = make_error(SemaErrorKind::ExportNotTopLevel);
    assert_eq!(err.to_string(), "export is only allowed at the top level");
}

#[test]
fn display_opaque_without_export() {
    let err = make_error(SemaErrorKind::OpaqueWithoutExport);
    assert_eq!(err.to_string(), "opaque requires export");
}

#[test]
fn display_foreign_not_top_level() {
    let err = make_error(SemaErrorKind::ForeignNotTopLevel);
    assert_eq!(
        err.to_string(),
        "foreign declaration is only allowed at the top level"
    );
}

#[test]
fn display_splice_outside_quote() {
    let err = make_error(SemaErrorKind::SpliceOutsideQuote);
    assert_eq!(err.to_string(), "splice outside quote");
}

#[test]
fn display_unreachable_pattern() {
    let err = make_error(SemaErrorKind::UnreachablePattern);
    assert_eq!(err.to_string(), "unreachable pattern after wildcard");
}

#[test]
fn display_unreachable_code() {
    let err = make_error(SemaErrorKind::UnreachableCode);
    assert_eq!(
        err.to_string(),
        "unreachable code after diverging expression"
    );
}

#[test]
fn display_unused_binding() {
    let mut interner = Interner::new();
    let name = interner.intern("x");
    let err = make_error(SemaErrorKind::UnusedBinding { name });
    assert!(err.to_string().contains("unused binding"));
}

#[test]
fn display_unused_parameter() {
    let mut interner = Interner::new();
    let name = interner.intern("y");
    let err = make_error(SemaErrorKind::UnusedParameter { name });
    assert!(err.to_string().contains("unused parameter"));
}

#[test]
fn display_or_pattern_mismatch() {
    let err = make_error(SemaErrorKind::OrPatternMismatch);
    assert!(
        err.to_string()
            .contains("or-pattern alternatives bind different names")
    );
}

#[test]
fn display_duplicate_instance() {
    let mut interner = Interner::new();
    let class = interner.intern("Eq");
    let ty = interner.intern("MyType");
    let err = make_error(SemaErrorKind::DuplicateInstance { class, ty });
    let msg = err.to_string();
    assert!(msg.contains("duplicate instance"));
}

#[test]
fn display_resume_on_never() {
    let mut interner = Interner::new();
    let op = interner.intern("abort");
    let err = make_error(SemaErrorKind::ResumeOnNever { op });
    let msg = err.to_string();
    assert!(msg.contains("resume"));
    assert!(msg.contains("Never"));
}
