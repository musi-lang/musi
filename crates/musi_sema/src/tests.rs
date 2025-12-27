use crate::SemanticModel;
use crate::binder::bind;
use crate::builtins::Builtins;
use crate::ty_repr::{IntWidth, TyRepr};
use crate::unifier::Unifier;
use musi_basic::diagnostic::DiagnosticBag;
use musi_basic::interner::Interner;
use musi_basic::source::SourceFile;
use musi_lex::lexer::tokenize;
use musi_parse::parse;

struct TestContext {
    interner: Interner,
    builtins: Builtins,
}

impl TestContext {
    fn new() -> Self {
        let mut interner = Interner::new();
        let builtins = Builtins::from_interner(&mut interner);
        Self { interner, builtins }
    }

    fn check_bind(&mut self, code: &str) -> (SemanticModel, DiagnosticBag) {
        let source = SourceFile::new("test.ms".to_string(), code.to_string(), 0);
        let (tokens, lex_errs) = tokenize(&source, &mut self.interner);
        assert!(lex_errs.is_empty(), "Lexer errors: {lex_errs:?}");

        let parse_result = parse(&tokens);
        assert!(
            parse_result.diagnostics.is_empty(),
            "Parser errors: {:?}",
            parse_result.diagnostics
        );

        bind(
            &parse_result.arena,
            &self.interner,
            &parse_result.prog,
            &self.builtins,
        )
    }
}

mod unifier {
    use super::*;

    #[test]
    fn unify_same_types() {
        let mut unifier = Unifier::new();
        let int32 = TyRepr::int(IntWidth::I32);
        assert!(unifier.unify(&int32, &int32).is_ok());
    }

    #[test]
    fn unify_var_with_concrete() {
        let mut unifier = Unifier::new();
        let var = unifier.fresh_var();
        let int32 = TyRepr::int(IntWidth::I32);

        assert!(unifier.unify(&var, &int32).is_ok());
        let resolved = unifier.apply(&var);
        assert_eq!(resolved, int32);
    }

    #[test]
    fn unify_mismatch() {
        let mut unifier = Unifier::new();
        let int32 = TyRepr::int(IntWidth::I32);
        let bool_ty = TyRepr::bool();

        assert!(unifier.unify(&int32, &bool_ty).is_err());
    }

    #[test]
    fn unify_any_with_anything() {
        let mut unifier = Unifier::new();
        let any = TyRepr::any();
        let int32 = TyRepr::int(IntWidth::I32);

        assert!(unifier.unify(&any, &int32).is_ok());
    }
}

mod binder {
    use super::*;

    #[test]
    fn bind_inference_simple() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := 10;");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_inference_transitive() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := 10; val y := x;");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_gradual_any() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x: Any := 10; val y: Int := x;");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_mutability_ok() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("var x := 10; x <- 22;");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_mutability_error() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := 10; x <- 20;");
        assert!(
            !diags.is_empty(),
            "expected error for assigning to immutable binding"
        );
        let msg = diags.diagnostics[0].message.clone();
        assert!(
            msg.contains("cannot assign to immutable binding"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn bind_scope_shadowing() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := 10; { val x := true; };");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_scope_access_error() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("{ val x := 10; }; x;");
        assert!(
            !diags.is_empty(),
            "expected error for accessing variable out of scope"
        );
        let msg = diags.diagnostics[0].message.clone();
        assert!(
            msg.contains("undefined identifier"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn bind_if_expr_types() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := if true { 10 } else { 20 };");
        assert!(diags.is_empty(), "bind errors: {diags:?}");
    }

    #[test]
    fn bind_if_expr_mismatch() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("val x := if true { 10 } else { true };");
        assert!(!diags.is_empty(), "Expected error for type mismatch");
        let msg = diags.diagnostics[0].message.clone();
        assert!(
            msg.contains("cannot convert type"),
            "Unexpected error: {msg}"
        );
    }

    #[test]
    fn bind_while_scope() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("while true { val x := 10; }; x;");
        assert!(
            !diags.is_empty(),
            "expected error for accessing while-loop variable out of scope"
        );
        let msg = diags.diagnostics[0].message.clone();
        assert!(
            msg.contains("undefined identifier"),
            "unexpected error: {msg}"
        );
    }

    #[test]
    fn bind_for_scope() {
        let mut ctx = TestContext::new();
        let (_, diags) = ctx.check_bind("for i in [1, 2, 3] { i; }; i;");
        assert!(
            !diags.is_empty(),
            "expected error for accessing for-loop variable out of scope"
        );
        let msg = diags.diagnostics[0].message.clone();
        assert!(
            msg.contains("undefined identifier"),
            "unexpected error: {msg}"
        );
    }
}
