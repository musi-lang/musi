use std::collections::BTreeMap;

use music_base::SourceId;
use music_hir::{HirExprId, HirExprKind, HirTyKind};
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_syntax::{Lexer, parse};

use super::{SemaModule, SemaOptions, check_module};

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
            .ok_or_else(|| ImportError::new(ImportErrorKind::NotFound, spec.as_str()))
    }
}

fn check(src: &str) -> SemaModule {
    check_with_env(src, None)
}

fn check_with_env(src: &str, import_env: Option<&dyn ImportEnv>) -> SemaModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(1),
        &ModuleKey::new("main"),
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            prelude: Vec::new(),
            import_env,
        },
    );
    check_module(resolved, &mut interner, SemaOptions::default())
}

#[test]
fn import_exprs_type_as_opaque_module() {
    let src = r#"
        let IO := import "std/io";
        IO;
    "#;
    let env = TestImportEnv::default().with_module("std/io", "std/io");
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(11),
        &ModuleKey::new("main"),
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            prelude: Vec::new(),
            import_env: Some(&env),
        },
    );
    let sema = check_module(resolved, &mut interner, SemaOptions::default());
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::Module
    ));
}

#[test]
fn quote_expr_types_as_syntax() {
    let sema = check("quote (1);");
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::Syntax
    ));
}

#[test]
fn handle_consumes_named_effect() {
    let sema = check(
        r#"
        let Console := effect {
          let readln () : String;
        };
        handle perform Console.readln() with Console of (
        | value => value
        | readln(k) => resume "ok"
        );
    "#,
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::String
    ));
    assert!(sema.expr_effects(root).is_pure(), "{:?}", sema.diags());
}

#[test]
fn class_and_instance_are_collected() {
    let sema = check(
        r"
        let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
          law reflexive (x : T) := true;
        };
        let eqInt := instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    ",
    );
    let class_id = find_expr(&sema, |kind| matches!(kind, HirExprKind::Class { .. }))
        .expect("expected class expr");
    let instance_id = find_expr(&sema, |kind| matches!(kind, HirExprKind::Instance { .. }))
        .expect("expected instance expr");
    assert!(sema.has_class_facts(class_id));
    assert!(sema.has_instance_facts(instance_id));
}

#[test]
fn invalid_link_attr_target_reports_diag() {
    let sema = check("@link(name := \"c\") let x := 1;");
    assert!(
        sema.diags()
            .iter()
            .any(|diag| diag.message() == "attr invalid target"),
        "{:?}",
        sema.diags()
    );
}

fn find_expr(sema: &SemaModule, predicate: impl Fn(&HirExprKind) -> bool) -> Option<HirExprId> {
    sema.module()
        .store
        .exprs
        .iter()
        .find_map(|(id, expr)| predicate(&expr.kind).then_some(id))
}
