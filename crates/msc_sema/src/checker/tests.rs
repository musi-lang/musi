//! Unit tests for the type checker.
//!
//! Tests construct AST manually to test the checker module in isolation.
//! These unit tests verify type inference and checking behavior without
//! relying on the lexer and parser.

use msc_ast::expr::{BindKind, Expr, LetFields, Param, ParamMode};
use msc_ast::pat::Pat;
use msc_ast::{AstArenas, ExprIdx, Lit, NameRef, ParsedModule, Stmt};
use std::collections::HashMap;

use msc_shared::{DiagnosticBag, FileId, Interner, Severity, Span, Symbol};

use crate::checker::{CheckContext, Checker};
use crate::{SemaOptions, UnifyTable, pipeline};

/// Helper to construct a Stmt.
fn stmt(expr_idx: ExprIdx) -> Stmt {
    Stmt {
        expr: expr_idx,
        span: Span::DUMMY,
    }
}

/// Helper to construct a minimal `ParsedModule` with statements.
fn make_module(arenas: AstArenas, stmts: Vec<Stmt>) -> ParsedModule {
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

/// Helper to construct and allocate a Name expression.
fn alloc_name_expr(arenas: &mut AstArenas, sym: Symbol) -> ExprIdx {
    let name_ref = arenas.name_refs.alloc(NameRef {
        name: sym,
        span: Span::DUMMY,
    });
    arenas.exprs.alloc(Expr::Name {
        name_ref,
        span: Span::DUMMY,
    })
}

/// Helper to construct a binding pattern.
fn bind_pat(sym: Symbol) -> Pat {
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
    let (mut defs, well_known, mut scopes, module_scope, resolved, types) =
        pipeline::analyze_setup(module, interner, FileId(0), &mut diags_setup);

    let empty_imports = HashMap::new();
    let ctx = CheckContext {
        ast: &module.arenas,
        interner,
        file_id: FileId(0),
        well_known: &well_known,
        expr_defs: &resolved.expr_defs,
        pat_defs: &resolved.pat_defs,
        import_types: &empty_imports,
        law_inferred_vars: &resolved.law_inferred_vars,
        class_op_members: &resolved.class_op_members,
    };
    let mut diags = DiagnosticBag::new();
    let mut checker = Checker::new_with_state(
        ctx,
        &mut diags,
        &mut defs,
        &mut scopes,
        module_scope,
        types,
        UnifyTable::new(),
    );

    for st in &module.stmts {
        let _ty = checker.synth(st.expr);
    }

    checker.resolve_obligations();
    pipeline::analyze_emit_unused_warnings(
        &defs,
        interner,
        FileId(0),
        &mut diags,
        &SemaOptions::default(),
    );

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
        pat,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value),
        with_effects: None,
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
    // Underscore prefix should suppress the unused binding warning
    assert!(diags.iter().all(|d| d.severity != Severity::Warning));
}

#[test]
fn test_check_fn_parameter_scope_in_body() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym_f = interner.intern("f");
    let sym_x = interner.intern("x");

    // Lambda body: x (reference to parameter)
    let param_ref = alloc_name_expr(&mut arenas, sym_x);

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
        ret_ty: None,
        body: param_ref,
        span: Span::DUMMY,
    });

    // Binding: let f := (x) -> x;
    let pat_f = arenas.pats.alloc(bind_pat(sym_f));
    let fields = LetFields {
        kind: BindKind::Immut,
        pat: pat_f,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(lambda),
        with_effects: None,
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
        pat,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value),
        with_effects: None,
        span: Span::DUMMY,
    };
    let binding = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields,
        span: Span::DUMMY,
    });

    // Second stmt: x;
    let name_ref = alloc_name_expr(&mut arenas, sym_x);

    let module = make_module(arenas, vec![stmt(binding), stmt(name_ref)]);
    let diags = check_module(&mut interner, &module);

    assert!(!diags.has_errors());
}

#[test]
fn test_insert_cast_records_any_boundary() {
    let mut interner = Interner::new();
    let arenas = AstArenas::new();
    let module = make_module(arenas, vec![]);

    let mut diags_setup = DiagnosticBag::new();
    let (mut defs, well_known, mut scopes, module_scope, resolved, types) =
        pipeline::analyze_setup(&module, &mut interner, FileId(0), &mut diags_setup);

    let empty_imports = HashMap::new();
    let ctx = CheckContext {
        ast: &module.arenas,
        interner: &mut interner,
        file_id: FileId(0),
        well_known: &well_known,
        expr_defs: &resolved.expr_defs,
        pat_defs: &resolved.pat_defs,
        import_types: &empty_imports,
        law_inferred_vars: &resolved.law_inferred_vars,
        class_op_members: &resolved.class_op_members,
    };
    let mut diags = DiagnosticBag::new();
    let mut checker = Checker::new_with_state(
        ctx,
        &mut diags,
        &mut defs,
        &mut scopes,
        module_scope,
        types,
        crate::UnifyTable::new(),
    );

    let any_ty = checker.named_ty(well_known.any);
    let int_ty = checker.named_ty(well_known.ints.int);
    let dummy_expr_idx = msc_ast::ExprIdx::from_raw(0u32);

    // Any ~ Int: inserting a cast should record it.
    checker.insert_cast(dummy_expr_idx, any_ty, int_ty, Span::DUMMY);
    assert!(checker.store.casts.contains_key(&dummy_expr_idx));

    // Identical types: no cast recorded.
    let dummy_expr_idx2 = msc_ast::ExprIdx::from_raw(1u32);
    checker.insert_cast(dummy_expr_idx2, int_ty, int_ty, Span::DUMMY);
    assert!(!checker.store.casts.contains_key(&dummy_expr_idx2));
}
