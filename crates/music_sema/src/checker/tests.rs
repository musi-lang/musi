//! Unit tests for the type checker.
//!
//! Tests construct AST manually to test the checker module in isolation.
//! These unit tests verify type inference and checking behavior without
//! relying on the lexer and parser.

use music_ast::expr::{Arrow, BindKind, Expr, LetFields, Param, ParamMode};
use music_ast::pat::Pat;
use music_ast::{AstArenas, Lit, ParsedModule};
use music_shared::{DiagnosticBag, FileId, Interner, Severity, Span};

use crate::{analyze_setup, CheckContext, Checker};

/// Helper to construct a Stmt.
fn stmt(expr_idx: music_shared::Idx<Expr>) -> music_ast::Stmt {
    music_ast::Stmt {
        expr: expr_idx,
        span: Span::DUMMY,
    }
}

/// Helper to construct a minimal `ParsedModule` with statements.
fn make_module(arenas: AstArenas, stmts: Vec<music_ast::Stmt>) -> ParsedModule {
    ParsedModule {
        arenas,
        stmts,
        span: Span::DUMMY,
    }
}

/// Helper to construct a Lit Int expression.
fn lit_int(value: i64) -> Expr {
    Expr::Lit {
        lit: Lit::Int {
            value,
            span: Span::DUMMY,
        },
        span: Span::DUMMY,
    }
}

/// Helper to construct a Name expression.
fn name_expr(sym: music_shared::Symbol) -> Expr {
    Expr::Name {
        name: sym,
        span: Span::DUMMY,
    }
}

/// Helper to construct a binding pattern.
fn bind_pat(sym: music_shared::Symbol) -> Pat {
    Pat::Bind {
        kind: BindKind::Immut,
        name: sym,
        inner: None,
        span: Span::DUMMY,
    }
}

/// Check an AST module and return diagnostics.
fn check_module(interner: &mut Interner, module: &ParsedModule) -> DiagnosticBag {
    let mut diags_setup = DiagnosticBag::new();
    let (mut defs, well_known, mut scopes, module_scope, resolved) =
        analyze_setup(module, interner, FileId(0), &mut diags_setup);

    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id: FileId(0),
        well_known: &well_known,
        expr_defs: &resolved.expr_defs,
    };
    let mut diags = DiagnosticBag::new();
    let mut checker = Checker::new(ctx, &mut diags, &mut defs, &mut scopes, module_scope);

    for st in &module.stmts {
        let _ty = checker.synth(st.expr);
    }

    checker.resolve_obligations();
    crate::analyze_emit_unused_warnings(&defs, interner, FileId(0), &mut diags);

    diags
}

#[test]
fn test_check_empty_program_produces_no_errors() {
    let mut interner = Interner::new();
    let arenas = AstArenas::new();
    let module = make_module(arenas, vec![]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
}

#[test]
fn test_check_integer_literal_infers_type() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let lit = arenas.exprs.alloc(lit_int(42));

    let module = make_module(arenas, vec![stmt(lit)]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
}

#[test]
fn test_check_underscore_binding_suppresses_unused_warning() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym_x = interner.intern("_x");

    // Binding: let _x := 42; (underscore prefix suppresses warning)
    let pat = arenas.pats.alloc(bind_pat(sym_x));
    let value = arenas.exprs.alloc(lit_int(42));
    let fields = LetFields {
        kind: BindKind::Immut,
        heap: false,
        pat,
        ty: None,
        value,
        span: Span::DUMMY,
    };
    let binding = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields,
        span: Span::DUMMY,
    });

    let module = make_module(arenas, vec![stmt(binding)]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
    // Underscore prefix should suppress the unused variable warning
    assert!(diags.iter().all(|d| d.severity != Severity::Warning));
}

#[test]
fn test_check_fn_parameter_scope_in_body() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym_f = interner.intern("f");
    let sym_x = interner.intern("x");

    // Lambda body: x (reference to parameter)
    let param_ref = arenas.exprs.alloc(name_expr(sym_x));

    // Lambda: (x) -> x
    let param = Param {
        mode: ParamMode::Plain,
        name: sym_x,
        ty: None,
        default: None,
        span: Span::DUMMY,
    };
    let lambda = arenas.exprs.alloc(Expr::Fn {
        params: vec![param],
        arrow: Arrow::Pure,
        ret_ty: None,
        body: param_ref,
        span: Span::DUMMY,
    });

    // Binding: let f := (x) -> x;
    let pat_f = arenas.pats.alloc(bind_pat(sym_f));
    let fields = LetFields {
        kind: BindKind::Immut,
        heap: false,
        pat: pat_f,
        ty: None,
        value: lambda,
        span: Span::DUMMY,
    };
    let binding = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields,
        span: Span::DUMMY,
    });

    let module = make_module(arenas, vec![stmt(binding)]);
    let diags = check_module(&mut interner, &module);

    // Function definition should succeed
    assert!(!diags.has_errors());
}

#[test]
fn test_check_multiple_statements() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();

    // First statement: 42;
    let lit1 = arenas.exprs.alloc(lit_int(42));

    // Second statement: 100;
    let lit2 = arenas.exprs.alloc(lit_int(100));

    let module = make_module(arenas, vec![stmt(lit1), stmt(lit2)]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
}

#[test]
fn test_check_binding_then_reference() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym_x = interner.intern("x");

    // First stmt: let x := 42;
    let pat = arenas.pats.alloc(bind_pat(sym_x));
    let value = arenas.exprs.alloc(lit_int(42));
    let fields = LetFields {
        kind: BindKind::Immut,
        heap: false,
        pat,
        ty: None,
        value,
        span: Span::DUMMY,
    };
    let binding = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields,
        span: Span::DUMMY,
    });

    // Second stmt: x;
    let name_ref = arenas.exprs.alloc(name_expr(sym_x));

    let module = make_module(arenas, vec![stmt(binding), stmt(name_ref)]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
}
