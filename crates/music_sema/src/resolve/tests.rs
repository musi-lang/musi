//! Unit tests for name resolution.
//!
//! Tests construct AST manually to test the resolve module in isolation,
//! without depending on the lexer or parser.

use music_ast::expr::{BindKind, Expr, LetFields, Param, ParamMode};
use music_ast::pat::Pat;
use music_ast::{AstArenas, ExprIdx, Lit, NameRef, ParsedModule, Stmt};
use music_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::def::DefTable;
use crate::resolve;
use crate::scope::ScopeTree;
use crate::well_known;

/// Helper to construct a Stmt.
fn stmt(expr_idx: ExprIdx) -> Stmt {
    Stmt {
        expr: expr_idx,
        span: Span::DUMMY,
    }
}

/// Helper to construct a minimal `ParsedModule` with just statements.
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

/// Resolve an AST module and return the output and diagnostics.
fn resolve_module(
    interner: &mut Interner,
    module: &ParsedModule,
) -> (resolve::ResolveOutput, DiagnosticBag) {
    let mut defs = DefTable::new();
    let mut scopes = ScopeTree::new();
    let module_scope = scopes.push_root();
    let _well_known = well_known::init_well_known(interner, &mut defs, module_scope, &mut scopes);

    let mut diags = DiagnosticBag::new();
    let resolved = resolve::resolve(
        module,
        interner,
        FileId(0),
        &mut diags,
        &mut defs,
        &mut scopes,
        module_scope,
    );

    (resolved, diags)
}

#[test]
fn test_resolve_empty_program_produces_no_defs_and_no_errors() {
    let mut interner = Interner::new();
    let arenas = AstArenas::new();
    let module = make_module(arenas, vec![]);
    let (output, diags) = resolve_module(&mut interner, &module);

    assert!(output.pat_defs.is_empty());
    assert!(output.expr_defs.is_empty());
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_let_binding_registers_pat_def() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym = interner.intern("x");
    let pat = arenas.pats.alloc(bind_pat(sym));
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
    let (output, diags) = resolve_module(&mut interner, &module);

    assert_eq!(output.pat_defs.len(), 1);
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_name_reference_creates_expr_def() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym = interner.intern("x");

    // First stmt: let x := 42;
    let pat = arenas.pats.alloc(bind_pat(sym));
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
    let name_ref = alloc_name_expr(&mut arenas, sym);

    let module = make_module(arenas, vec![stmt(binding), stmt(name_ref)]);
    let (output, diags) = resolve_module(&mut interner, &module);

    // One pat def (x binding) + one expr def (x reference)
    assert_eq!(output.expr_defs.len(), 1);
    assert!(!diags.has_errors());
}

#[test]
fn test_resolve_undefined_name_emits_error() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym = interner.intern("z");

    // Stmt: z; (no prior binding)
    let name_ref = alloc_name_expr(&mut arenas, sym);

    let module = make_module(arenas, vec![stmt(name_ref)]);
    let (_output, diags) = resolve_module(&mut interner, &module);

    assert!(diags.has_errors());
}

#[test]
fn test_resolve_duplicate_top_level_binding_emits_error() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym = interner.intern("x");

    // First binding: let x := 1;
    let pat1 = arenas.pats.alloc(bind_pat(sym));
    let value1 = arenas.exprs.alloc(lit_int(1));
    let fields1 = LetFields {
        kind: BindKind::Immut,
        pat: pat1,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value1),
        with_effects: None,
        span: Span::DUMMY,
    };
    let binding1 = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields: fields1,
        span: Span::DUMMY,
    });

    // Second binding: let x := 2;
    let pat2 = arenas.pats.alloc(bind_pat(sym));
    let value2 = arenas.exprs.alloc(lit_int(2));
    let fields2 = LetFields {
        kind: BindKind::Immut,
        pat: pat2,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value2),
        with_effects: None,
        span: Span::DUMMY,
    };
    let binding2 = arenas.exprs.alloc(Expr::Binding {
        exported: false,
        fields: fields2,
        span: Span::DUMMY,
    });

    let module = make_module(arenas, vec![stmt(binding1), stmt(binding2)]);
    let (_output, diags) = resolve_module(&mut interner, &module);

    assert!(diags.has_errors());
}

#[test]
fn test_resolve_fn_param_creates_def_in_body() {
    let mut interner = Interner::new();
    let mut arenas = AstArenas::new();
    let sym_f = interner.intern("f");
    let sym_p = interner.intern("p");

    // Body: p (reference to parameter)
    let param_ref = alloc_name_expr(&mut arenas, sym_p);

    // Lambda: (p) -> p
    let param = Param {
        mode: ParamMode::Plain,
        name: sym_p,
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

    // Top-level: let f := (p) -> p;
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
    let (output, diags) = resolve_module(&mut interner, &module);

    // At least one pat_def: f (parameter defs are tracked but may not appear in output)
    assert!(!output.pat_defs.is_empty());
    // One expr_def: p reference in body
    assert_eq!(output.expr_defs.len(), 1);
    assert!(!diags.has_errors());
}
