use std::collections::BTreeMap;

use music_base::SourceId;
use music_hir::{HirExprId, HirExprKind, HirTyKind};
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_syntax::{Lexer, parse};

use super::{EffectKey, EffectRow, ModuleSurface, SemaEnv, SemaModule, SemaOptions, check_module};

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

#[derive(Default)]
struct TestSemaEnv {
    modules: BTreeMap<String, ModuleSurface>,
}

impl TestSemaEnv {
    fn with_surface(mut self, key: &str, surface: ModuleSurface) -> Self {
        let _prev = self.modules.insert(key.into(), surface);
        self
    }
}

impl SemaEnv for TestSemaEnv {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface> {
        self.modules.get(key.as_str()).cloned()
    }
}

fn check(src: &str) -> SemaModule {
    check_module_src(1, "main", src, None, None)
}

fn check_module_src(
    source_id_raw: u32,
    module_key: &str,
    src: &str,
    import_env: Option<&dyn ImportEnv>,
    sema_env: Option<&dyn SemaEnv>,
) -> SemaModule {
    let lexed = Lexer::new(src).lex();
    let parsed = parse(lexed);
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());

    let mut interner = Interner::new();
    let resolved = resolve_module(
        SourceId::from_raw(source_id_raw),
        &ModuleKey::new(module_key),
        parsed.tree(),
        &mut interner,
        ResolveOptions {
            prelude: Vec::new(),
            import_env,
        },
    );
    check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: sema_env,
        },
    )
}

#[test]
fn import_exprs_type_as_opaque_module() {
    let src = r#"
        let IO := import "std/io";
        IO;
    "#;
    let env = TestImportEnv::default().with_module("std/io", "std/io");
    let sema = check_module_src(11, "main", src, Some(&env), None);
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::Module
    ));
    assert_eq!(
        sema.expr_module_target(
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Import { .. }))
                .expect("import expr")
        )
        .map(ModuleKey::as_str),
        Some("std/io")
    );
}

#[test]
fn imported_module_field_access_uses_export_surface() {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        12,
        "std/io",
        r"
        export let read (path : String) : String := path;
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
    let sema = check_module_src(
        13,
        "main",
        r#"
        let IO := import "std/io";
        IO.read;
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::Arrow { .. }
    ));
    assert!(sema.diags().is_empty(), "{:?}", sema.diags());
}

#[test]
fn imported_module_record_pattern_binds_exported_values() {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        14,
        "std/io",
        r"
        export let read (path : String) : String := path;
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
    let sema = check_module_src(
        15,
        "main",
        r#"
        let IO := import "std/io";
        let {read} := IO;
        read;
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::Arrow { .. }
    ));
    assert!(
        !sema
            .diags()
            .iter()
            .any(|diag| diag.message() == "unknown export"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn imported_effect_alias_handles_perform_and_handle() {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        16,
        "std/io",
        r"
        export let Console := effect {
          let readln () : String;
        };
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
    let sema = check_module_src(
        17,
        "main",
        r#"
        let IO := import "std/io";
        let Console := IO.Console;
        handle perform Console.readln() with Console of (
        | value => value
        | readln(k) => resume "ok"
        );
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::String
    ));
    assert!(sema.expr_effects(root).is_pure(), "{:?}", sema.diags());
}

#[test]
fn perform_effects_expose_textual_names() {
    let sema = check(
        r#"
        let Console := effect {
          let readln () : String;
        };
        perform Console.readln();
    "#,
    );
    let root = sema.module().root;
    let effects = sema.expr_effects(root);
    assert!(
        effects
            .items
            .iter()
            .any(|item| item.name.as_ref() == "Console"),
        "{:?}",
        effects
    );
    assert!(effects.open.is_none(), "{:?}", effects);
}

#[test]
fn destructured_effect_alias_handles_perform_and_handle() {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        25,
        "std/io",
        r"
        export let Console := effect {
          let readln () : String;
        };
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
    let sema = check_module_src(
        26,
        "main",
        r#"
        let IO := import "std/io";
        let {Console} := IO;
        handle perform Console.readln() with Console of (
        | value => value
        | readln(k) => resume "ok"
        );
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.expr_ty(root)).kind,
        HirTyKind::String
    ));
    assert!(sema.expr_effects(root).is_pure(), "{:?}", sema.diags());
    assert!(
        !sema
            .diags()
            .iter()
            .any(|diag| diag.message() == "unknown effect"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn effect_rows_union_and_remove_by_text() {
    let mut row = EffectRow::empty();
    row.add(EffectKey {
        name: "Console".into(),
        arg: None,
    });

    let mut other = EffectRow::empty();
    other.add(EffectKey {
        name: "State".into(),
        arg: None,
    });
    other.open = Some("rest".into());

    row.union_with(&other);
    row.remove_by_name("Console");

    assert!(!row.items.iter().any(|item| item.name.as_ref() == "Console"));
    assert!(row.items.iter().any(|item| item.name.as_ref() == "State"));
    assert_eq!(row.open.as_deref(), Some("rest"));
}

#[test]
fn imported_class_alias_supports_instance_checking() {
    let import_env = TestImportEnv::default().with_module("std/types", "std/types");
    let types = check_module_src(
        18,
        "std/types",
        r"
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/types", types.surface().clone());
    let sema = check_module_src(
        19,
        "main",
        r#"
        let Types := import "std/types";
        let Eq := Types.Eq;
        let eqInt := instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let instance_id = find_expr(&sema, |kind| matches!(kind, HirExprKind::Instance { .. }))
        .expect("expected instance expr");
    assert!(sema.instance_facts(instance_id).is_some());
    assert!(
        !sema
            .diags()
            .iter()
            .any(|diag| diag.message() == "unknown class"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn destructured_class_alias_supports_instance_checking() {
    let import_env = TestImportEnv::default().with_module("std/types", "std/types");
    let types = check_module_src(
        27,
        "std/types",
        r"
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/types", types.surface().clone());
    let sema = check_module_src(
        28,
        "main",
        r#"
        let Types := import "std/types";
        let {Eq} := Types;
        let eqInt := instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let instance_id = find_expr(&sema, |kind| matches!(kind, HirExprKind::Instance { .. }))
        .expect("expected instance expr");
    assert!(sema.instance_facts(instance_id).is_some());
    assert!(
        !sema
            .diags()
            .iter()
            .any(|diag| diag.message() == "unknown class"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn imported_class_alias_ignores_symbol_allocation_order() {
    let import_env = TestImportEnv::default().with_module("std/types", "std/types");
    let types = check_module_src(
        23,
        "std/types",
        r#"
        let warmup := "noise";
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
    "#,
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/types", types.surface().clone());
    let sema = check_module_src(
        24,
        "main",
        r#"
        let scratch := 42;
        let Types := import "std/types";
        let Eq := Types.Eq;
        let eqInt := instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&sema_env),
    );
    let instance_id = find_expr(&sema, |kind| matches!(kind, HirExprKind::Instance { .. }))
        .expect("expected instance expr");
    assert!(sema.instance_facts(instance_id).is_some());
    assert!(
        !sema
            .diags()
            .iter()
            .any(|diag| diag.message() == "unknown class"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn class_and_instance_queries_return_facts() {
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
    assert!(sema.class_facts(class_id).is_some());
    assert!(sema.instance_facts(instance_id).is_some());
}

#[test]
fn duplicate_handler_clause_reports_diag() {
    let sema = check(
        r#"
        let Console := effect {
          let readln () : String;
        };
        handle perform Console.readln() with Console of (
        | value => value
        | readln(k) => resume "ok"
        | readln(k) => resume "ok"
        );
    "#,
    );
    assert!(
        sema.diags()
            .iter()
            .any(|diag| diag.message() == "duplicate handler clause"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn duplicate_class_member_reports_diag() {
    let sema = check(
        r"
        let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
          let (=) (a : T, b : T) : Bool;
        };
    ",
    );
    assert!(
        sema.diags()
            .iter()
            .any(|diag| diag.message() == "duplicate class member"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn missing_instance_member_reports_diag() {
    let sema = check(
        r"
        let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
          let compare (a : T, b : T) : Int;
        };
        let eqInt := instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    ",
    );
    assert!(
        sema.diags()
            .iter()
            .any(|diag| diag.message() == "missing instance member"),
        "{:?}",
        sema.diags()
    );
}

#[test]
fn reachable_exported_instances_participate_in_coherence() {
    let import_env = TestImportEnv::default()
        .with_module("a", "a")
        .with_module("b", "b");

    let module_a = check_module_src(
        20,
        "a",
        r"
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
        export instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    ",
        Some(&import_env),
        None,
    );
    let env_for_b = TestSemaEnv::default().with_surface("a", module_a.surface().clone());
    let module_b = check_module_src(
        21,
        "b",
        r#"
        let A := import "a";
        export let Eq := A.Eq;
        export instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&env_for_b),
    );
    let env_for_main = TestSemaEnv::default()
        .with_surface("a", module_a.surface().clone())
        .with_surface("b", module_b.surface().clone());
    let main = check_module_src(
        22,
        "main",
        r#"
        let A := import "a";
        let B := import "b";
        0;
    "#,
        Some(&import_env),
        Some(&env_for_main),
    );
    assert!(
        main.diags()
            .iter()
            .any(|diag| diag.message() == "duplicate instance"),
        "{:?}",
        main.diags()
    );
}

#[test]
fn non_exported_instances_do_not_participate_in_coherence() {
    let import_env = TestImportEnv::default()
        .with_module("a", "a")
        .with_module("b", "b");

    let module_a = check_module_src(
        29,
        "a",
        r"
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
        instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    ",
        Some(&import_env),
        None,
    );
    let env_for_b = TestSemaEnv::default().with_surface("a", module_a.surface().clone());
    let module_b = check_module_src(
        30,
        "b",
        r#"
        let A := import "a";
        export let Eq := A.Eq;
        export instance Eq[Int] {
          let (=) (a : Int, b : Int) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&env_for_b),
    );
    let env_for_main = TestSemaEnv::default()
        .with_surface("a", module_a.surface().clone())
        .with_surface("b", module_b.surface().clone());
    let main = check_module_src(
        31,
        "main",
        r#"
        let A := import "a";
        let B := import "b";
        0;
    "#,
        Some(&import_env),
        Some(&env_for_main),
    );
    assert!(
        !main
            .diags()
            .iter()
            .any(|diag| diag.message() == "duplicate instance"),
        "{:?}",
        main.diags()
    );
}

#[test]
fn imported_tuple_instances_match_local_coherence_keys() {
    let import_env = TestImportEnv::default().with_module("a", "a");
    let module_a = check_module_src(
        32,
        "a",
        r"
        export let Eq[T] := class {
          let (=) (a : T, b : T) : Bool;
        };
        export instance Eq[(Int, Int)] {
          let (=) (a : (Int, Int), b : (Int, Int)) : Bool := true;
        };
    ",
        Some(&import_env),
        None,
    );
    let env_for_main = TestSemaEnv::default().with_surface("a", module_a.surface().clone());
    let main = check_module_src(
        33,
        "main",
        r#"
        let A := import "a";
        let Eq := A.Eq;
        let eqPair := instance Eq[(Int, Int)] {
          let (=) (a : (Int, Int), b : (Int, Int)) : Bool := true;
        };
    "#,
        Some(&import_env),
        Some(&env_for_main),
    );
    assert!(
        main.diags()
            .iter()
            .any(|diag| diag.message() == "duplicate instance"),
        "{:?}",
        main.diags()
    );
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
