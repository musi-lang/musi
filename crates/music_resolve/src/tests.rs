#![allow(unused_imports)]

use std::collections::BTreeMap;

use music_base::SourceId;
use music_hir::HirExprKind;
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::{Interner, NameBindingKind, NameSite};
use music_syntax::{Lexer, SyntaxNodeKind, SyntaxTree, TokenKind, canonical_name_text, parse};

use crate::{ResolveDiagKind, ResolveOptions, resolve_diag_kind, resolve_module};

#[derive(Default)]
struct TestImportEnv {
    modules: BTreeMap<String, ModuleKey>,
}

impl TestImportEnv {
    fn with_module(mut self, spec: &str, key: &str) -> Self {
        let _prev = self.modules.insert(spec.into(), ModuleKey::new(key));
        self
    }
}

impl ImportEnv for TestImportEnv {
    fn resolve(&self, _from: &ModuleKey, spec: &ModuleSpecifier) -> ImportResolveResult {
        self.modules
            .get(spec.as_str())
            .cloned()
            .ok_or_else(|| ImportError::new(ImportErrorKind::ModuleNotFound, spec.as_str()))
    }
}

fn find_nth_name_site(
    source_id: SourceId,
    tree: &SyntaxTree,
    spelling: &str,
    nth: usize,
) -> Option<NameSite> {
    let mut hits = 0usize;
    let mut stack = vec![tree.root()];
    while let Some(node) = stack.pop() {
        if node.kind() == SyntaxNodeKind::NameExpr {
            let tok = node
                .child_tokens()
                .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent));
            if let Some(tok) = tok {
                if let Some(raw) = tok.text() {
                    let canon = canonical_name_text(tok.kind(), raw);
                    if canon == spelling {
                        if hits == nth {
                            return Some(NameSite::new(source_id, tok.span()));
                        }
                        hits += 1;
                    }
                }
            }
        }
        for child in node.child_nodes() {
            stack.push(child);
        }
    }
    None
}

fn assert_name_binding(
    src: &str,
    source_id: SourceId,
    spelling: &str,
    nth: usize,
    expected_kind: NameBindingKind,
) {
    let module_key = ModuleKey::new("main");
    let parsed = parse(Lexer::new(src).lex());
    assert!(parsed.errors().is_empty());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        source_id,
        &module_key,
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            inject_compiler_prelude: true,
            ..ResolveOptions::default()
        },
    );
    let site = find_nth_name_site(source_id, parsed.tree(), spelling, nth).expect("use site");
    let binding_id = resolved.names.refs.get(&site).copied().expect("binding");
    let binding = resolved.names.bindings.get(binding_id);
    assert_eq!(binding.kind, expected_kind);
    assert_eq!(interner.resolve(binding.name), spelling);
}

mod success {
    use super::*;

    #[test]
    fn resolves_let_name_use() {
        assert_name_binding(
            "let x := 1; x;",
            SourceId::from_raw(1),
            "x",
            0,
            NameBindingKind::Let,
        );
    }

    #[test]
    fn resolves_rec_name_use_in_rhs() {
        assert_name_binding(
            "let rec f := f;",
            SourceId::from_raw(2),
            "f",
            0,
            NameBindingKind::Let,
        );
    }

    #[test]
    fn lowers_receiver_first_callable_as_plain_let_params() {
        let src = "let abs (self : Int) : Int := self;";
        let source_id = SourceId::from_raw(12);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                ..ResolveOptions::default()
            },
        );
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);

        let root_expr = resolved.module.store.exprs.get(resolved.module.root);
        let expr_ids = match &root_expr.kind {
            HirExprKind::Sequence { exprs } => resolved.module.store.expr_ids.get(*exprs),
            other => panic!("unexpected root kind: {other:?}"),
        };
        let let_id = expr_ids[0];
        let let_expr = resolved.module.store.exprs.get(let_id);
        let params = match &let_expr.kind {
            HirExprKind::Let { params, .. } => resolved.module.store.params.get(params.clone()),
            other => panic!("unexpected root stmt kind: {other:?}"),
        };

        assert_eq!(params.len(), 1);
        assert_eq!(interner.resolve(params[0].name.name), "self");
        let receiver_ty = params[0].ty.expect("receiver type");
        match &resolved.module.store.exprs.get(receiver_ty).kind {
            HirExprKind::Name { name } => assert_eq!(interner.resolve(name.name), "Int"),
            other => panic!("unexpected receiver type kind: {other:?}"),
        }
    }

    #[test]
    fn lowers_receiver_method_signature_from_declared_return_type() {
        let src = "export let(self : String).length () : Int := self;";
        let source_id = SourceId::from_raw(14);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                ..ResolveOptions::default()
            },
        );
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);

        let root_expr = resolved.module.store.exprs.get(resolved.module.root);
        let expr_ids = match &root_expr.kind {
            HirExprKind::Sequence { exprs } => resolved.module.store.expr_ids.get(*exprs),
            other => panic!("unexpected root kind: {other:?}"),
        };
        let let_expr = resolved.module.store.exprs.get(expr_ids[0]);
        let (receiver, sig) = match &let_expr.kind {
            HirExprKind::Let { receiver, sig, .. } => (receiver, sig),
            other => panic!("unexpected root stmt kind: {other:?}"),
        };

        assert!(receiver.is_some());
        let sig = sig.expect("return signature");
        match &resolved.module.store.exprs.get(sig).kind {
            HirExprKind::Name { name } => assert_eq!(interner.resolve(name.name), "Int"),
            other => panic!("unexpected signature kind: {other:?}"),
        }
    }

    #[test]
    fn lowers_mut_receiver_first_callable_as_plain_let_params() {
        let src = "let push (self : mut Int, value : Int) := self;";
        let source_id = SourceId::from_raw(13);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                ..ResolveOptions::default()
            },
        );
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);

        let root_expr = resolved.module.store.exprs.get(resolved.module.root);
        let expr_ids = match &root_expr.kind {
            HirExprKind::Sequence { exprs } => resolved.module.store.expr_ids.get(*exprs),
            other => panic!("unexpected root kind: {other:?}"),
        };
        let let_id = expr_ids[0];
        let let_expr = resolved.module.store.exprs.get(let_id);
        let params = match &let_expr.kind {
            HirExprKind::Let { params, .. } => resolved.module.store.params.get(params.clone()),
            other => panic!("unexpected root stmt kind: {other:?}"),
        };

        assert_eq!(params.len(), 2);
        assert_eq!(interner.resolve(params[0].name.name), "self");
        let receiver_ty = params[0].ty.expect("receiver type");
        assert!(matches!(
            resolved.module.store.exprs.get(receiver_ty).kind,
            HirExprKind::Prefix { .. }
        ));
    }

    #[test]
    fn resolves_case_pat_binder_in_arm() {
        let src = "let x := 0; match x (| .Some(y) => y | _ => x);";
        let source_id = SourceId::from_raw(3);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                ..ResolveOptions::default()
            },
        );
        let y_site = find_nth_name_site(source_id, parsed.tree(), "y", 0).expect("y use site");
        let y_binding = resolved
            .names
            .refs
            .get(&y_site)
            .copied()
            .expect("y binding");
        assert_eq!(
            resolved.names.bindings.get(y_binding).kind,
            NameBindingKind::PatternBind
        );

        let x_site = find_nth_name_site(source_id, parsed.tree(), "x", 0).expect("x use site");
        let x_binding = resolved
            .names
            .refs
            .get(&x_site)
            .copied()
            .expect("x binding");
        assert_eq!(
            resolved.names.bindings.get(x_binding).kind,
            NameBindingKind::Let
        );
    }

    #[test]
    fn lowers_pipe_into_call_and_prepends_left_value() {
        let src = r"let add := \(left, right) => left + right; let value := 1 |> add(2);";
        let source_id = SourceId::from_raw(33);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions::default(),
        );
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);

        let root_expr = resolved.module.store.exprs.get(resolved.module.root);
        let expr_ids = match &root_expr.kind {
            HirExprKind::Sequence { exprs } => resolved.module.store.expr_ids.get(*exprs),
            other => panic!("unexpected root kind: {other:?}"),
        };
        let let_expr = resolved.module.store.exprs.get(expr_ids[1]);
        let body = match &let_expr.kind {
            HirExprKind::Let { value, .. } => *value,
            other => panic!("unexpected let kind: {other:?}"),
        };
        let body_expr = resolved.module.store.exprs.get(body);
        let (callee, args) = match &body_expr.kind {
            HirExprKind::Call { callee, args } => {
                (*callee, resolved.module.store.args.get(args.clone()))
            }
            other => panic!("unexpected body kind: {other:?}"),
        };
        assert_eq!(args.len(), 2);
        match &resolved.module.store.exprs.get(callee).kind {
            HirExprKind::Name { name } => assert_eq!(interner.resolve(name.name), "add"),
            other => panic!("unexpected callee kind: {other:?}"),
        }
        assert!(matches!(
            resolved.module.store.exprs.get(args[0].expr).kind,
            HirExprKind::Lit { .. }
        ));
        assert!(matches!(
            resolved.module.store.exprs.get(args[1].expr).kind,
            HirExprKind::Lit { .. }
        ));
    }

    #[test]
    fn lowers_named_call_arguments_into_hir_args() {
        let src = r"
        let render (port, secure) := port;
        let value := render(port := 8080, secure := 0 = 0);
    ";
        let source_id = SourceId::from_raw(44);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions::default(),
        );
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);

        let root_expr = resolved.module.store.exprs.get(resolved.module.root);
        let expr_ids = match &root_expr.kind {
            HirExprKind::Sequence { exprs } => resolved.module.store.expr_ids.get(*exprs),
            other => panic!("unexpected root kind: {other:?}"),
        };
        let let_expr = resolved.module.store.exprs.get(expr_ids[1]);
        let call = match &let_expr.kind {
            HirExprKind::Let { value, .. } => *value,
            other => panic!("unexpected let kind: {other:?}"),
        };
        let HirExprKind::Call { args, .. } = &resolved.module.store.exprs.get(call).kind else {
            panic!("call expr expected");
        };
        let args = resolved.module.store.args.get(args.clone());
        assert_eq!(args.len(), 2);
        assert_eq!(interner.resolve(args[0].name.expect("name").name), "port");
        assert_eq!(interner.resolve(args[1].name.expect("name").name), "secure");
    }

    #[test]
    fn resolves_lambda_param_in_body() {
        assert_name_binding(
            r"\(x : Int) => x;",
            SourceId::from_raw(4),
            "x",
            0,
            NameBindingKind::Param,
        );
    }

    #[test]
    fn resolves_pi_binder_in_ret() {
        assert_name_binding(
            "(x: Type) -> x;",
            SourceId::from_raw(5),
            "x",
            0,
            NameBindingKind::PiBinder,
        );
    }

    #[test]
    fn data_declarations_do_not_report_variant_or_field_names_as_unbound() {
        let src = r"
        let Option[T] := data { | Some(T) | None | };
        let Pair[T] := data { left : T; right : T; };
    ";
        let source_id = SourceId::from_raw(6);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions::default(),
        );

        let unbound_count = resolved
            .diags
            .iter()
            .filter(|diag| resolve_diag_kind(diag) == Some(ResolveDiagKind::UnboundName))
            .count();
        assert_eq!(unbound_count, 0, "{:?}", resolved.diags);
    }

    #[test]
    fn static_imports_resolve_but_do_not_open_export_names() {
        let src = r#"
        let IO := import "std/io";
        IO;
        read;
    "#;
        let source_id = SourceId::from_raw(7);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default().with_module("std/io", "std/io");
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        assert_eq!(resolved.imports.len(), 1);
        assert_eq!(resolved.imports[0].spec.as_str(), "std/io");
        assert_eq!(resolved.imports[0].to.as_str(), "std/io");

        let io_site = find_nth_name_site(source_id, parsed.tree(), "IO", 0).expect("IO use site");
        let io_binding = resolved
            .names
            .refs
            .get(&io_site)
            .copied()
            .expect("IO binding");
        assert_eq!(
            resolved.names.bindings.get(io_binding).kind,
            NameBindingKind::Let
        );
        assert!(
            resolved
                .diags
                .iter()
                .any(|diag| resolve_diag_kind(diag) == Some(ResolveDiagKind::UnboundName))
        );
    }

    #[test]
    fn import_resolution_only_creates_explicit_let_binding() {
        let src = r#"
        let IO := import "std/io";
        IO;
    "#;
        let source_id = SourceId::from_raw(8);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default().with_module("std/io", "std/io");
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        let let_binding_count = resolved
            .names
            .bindings
            .iter()
            .filter(|(_, binding)| binding.kind == NameBindingKind::Let)
            .count();
        assert_eq!(let_binding_count, 1);
    }

    #[test]
    fn static_template_imports_resolve_from_import_env() {
        let src = r"
        let IO := import `std/io`;
    ";
        let source_id = SourceId::from_raw(9);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default().with_module("std/io", "std/io");
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        assert_eq!(resolved.imports.len(), 1);
        assert_eq!(resolved.imports[0].spec.as_str(), "std/io");
        assert_eq!(resolved.imports[0].to.as_str(), "std/io");
        assert!(resolved.diags.is_empty(), "{:?}", resolved.diags);
    }

    #[test]
    fn module_loads_report_invalid_specifier() {
        let src = r#"
        let path := "std/io";
        import path;
    "#;
        let source_id = SourceId::from_raw(12);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default().with_module("std/io", "std/io");
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        assert!(resolved.imports.is_empty());
        let diag = resolved
            .diags
            .iter()
            .find(|diag| resolve_diag_kind(diag) == Some(ResolveDiagKind::InvalidImportSpec))
            .expect("expected invalid import specifier diagnostic");
        assert_eq!(diag.message(), ResolveDiagKind::InvalidImportSpec.message());
        assert_eq!(
            diag.labels()[0].message(),
            ResolveDiagKind::InvalidImportSpec.label()
        );
    }

    #[test]
    fn handle_answer_name_resolves() {
        let src = "let h := answer x; handle x answer h;";
        let source_id = SourceId::from_raw(13);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions::default(),
        );

        let site = find_nth_name_site(source_id, parsed.tree(), "h", 0).expect("use site");
        let binding = resolved.names.refs.get(&site).copied().expect("binding");
        assert_eq!(
            resolved.names.bindings.get(binding).kind,
            NameBindingKind::Let
        );
    }

    #[test]
    fn resolved_module_keeps_module_key_and_export_summary() {
        let src = r"
        export let x := 1;
        export let eq := 2;
    ";
        let source_id = SourceId::from_raw(14);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions::default(),
        );

        assert_eq!(resolved.module_key.as_str(), "main");
        assert!(resolved.export_summary.exports().any(|name| name == "x"));
        assert!(resolved.export_summary.exports().any(|name| name == "eq"));
    }
}

mod failure {
    use super::*;

    #[test]
    fn unresolved_static_imports_emit_diag() {
        let src = r#"
        import "std/missing";
    "#;
        let source_id = SourceId::from_raw(10);
        let module_key = ModuleKey::new("main");
        let parsed = parse(Lexer::new(src).lex());
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default();
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        assert!(resolved.imports.is_empty());
        let diag = resolved
            .diags
            .iter()
            .find(|diag| resolve_diag_kind(diag) == Some(ResolveDiagKind::ImportResolveFailed))
            .expect("expected import resolve diag");
        assert_eq!(
            diag.message(),
            "unresolved source import specifier `std/missing`"
        );
        assert_eq!(diag.labels()[0].message(), "module not found `std/missing`");
        assert_eq!(diag.hint(), None);
    }

    #[test]
    fn invalid_string_imports_emit_invalid_spec_diag() {
        let src = r#"
        import "\x0";
    "#;
        let source_id = SourceId::from_raw(11);
        let module_key = ModuleKey::new("main");
        let lexed = Lexer::new(src).lex();
        assert!(!lexed.errors().is_empty(), "{:?}", lexed.errors());
        let parsed = parse(lexed);
        assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

        let env = TestImportEnv::default();
        let mut interner = Interner::new();
        let resolved = resolve_module(
            source_id,
            &module_key,
            parsed.tree(),
            &mut interner,
            ResolveOptions {
                inject_compiler_prelude: true,
                prelude: Vec::new(),
                import_env: Some(&env),
                ..ResolveOptions::default()
            },
        );

        assert!(resolved.imports.is_empty());
        let diag = resolved
            .diags
            .iter()
            .find(|diag| resolve_diag_kind(diag) == Some(ResolveDiagKind::InvalidImportSpec))
            .expect("expected invalid import specifier diag");
        assert_eq!(diag.message(), ResolveDiagKind::InvalidImportSpec.message());
        assert_eq!(
            diag.labels()[0].message(),
            ResolveDiagKind::InvalidImportSpec.label()
        );
        assert_eq!(diag.hint(), None);
    }
}
