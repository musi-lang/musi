#![allow(unused_imports)]

use std::collections::BTreeMap;

use music_base::diag::Diag;
use music_base::{DiagCode, SourceId};
use music_hir::{HirExprId, HirExprKind, HirTyKind};
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_syntax::{Lexer, parse};

use super::{
    EffectKey, EffectRow, ExprMemberKind, ModuleSurface, SemaDataVariantDef, SemaDiagKind, SemaEnv,
    SemaModule, SemaOptions, SurfaceTyKind, check_module, sema_diag_kind,
};

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
            inject_compiler_prelude: true,
            prelude: Vec::new(),
            import_env,
            ..ResolveOptions::default()
        },
    );
    let module = check_module(
        resolved,
        &mut interner,
        SemaOptions {
            target: None,
            env: sema_env,
            prelude: None,
        },
    );
    assert_no_unrendered_templates(&module);
    module
}

fn assert_no_unrendered_templates(module: &SemaModule) {
    for diag in module.diags() {
        assert!(
            !diag.message().contains('{') && !diag.message().contains('}'),
            "diagnostic message contains unrendered template: {diag:?}"
        );
        for label in diag.labels() {
            assert!(
                !label.message().contains('{') && !label.message().contains('}'),
                "diagnostic label contains unrendered template: {diag:?}"
            );
        }
    }
}

fn has_diag(module: &SemaModule, kind: SemaDiagKind) -> bool {
    module
        .diags()
        .iter()
        .any(|diag| sema_diag_kind(diag) == Some(kind))
}

fn find_diag(module: &SemaModule, kind: SemaDiagKind) -> Option<&Diag> {
    module
        .diags()
        .iter()
        .find(|diag| sema_diag_kind(diag) == Some(kind))
}

fn check_with_imported_surface(
    source_id_raw: u32,
    source_a: &str,
    source_b: &str,
) -> (SemaModule, SemaModule) {
    let import_env = TestImportEnv::default().with_module("a", "a");
    let module_a = check_module_src(source_id_raw, "a", source_a, Some(&import_env), None);
    let sema_env = TestSemaEnv::default().with_surface("a", module_a.surface().clone());
    let module_b = check_module_src(
        source_id_raw + 1,
        "b",
        source_b,
        Some(&import_env),
        Some(&sema_env),
    );
    (module_a, module_b)
}

fn assert_effect_alias_request(binding: &str, source_id: u32) {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        source_id,
        "std/io",
        r"
        export let Console := effect {
          let readLine () : String;
        };
    ",
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
    let sema = check_module_src(
        source_id + 1,
        "main",
        &format!(
            r#"
        let IO := import "std/io";
        {binding}
        ask Console.readLine();
    "#
        ),
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
            .kind,
        HirTyKind::String
    ));
    assert!(
        !has_diag(&sema, SemaDiagKind::UnknownEffect),
        "{:?}",
        sema.diags()
    );
    let effects = sema
        .try_expr_effects(root)
        .expect("root expr effects missing");
    assert!(
        effects
            .items
            .iter()
            .any(|effect| effect.name.as_ref() == "Console"),
        "{effects:?}"
    );
}

fn assert_imported_callable_alias(
    source_id: u32,
    type_prelude: &str,
    main_prelude: &str,
    binding: &str,
) {
    let import_env = TestImportEnv::default().with_module("std/types", "std/types");
    let types = check_module_src(
        source_id,
        "std/types",
        &format!(
            r"
        {type_prelude}
        export let id[T] (value : T) : T := value;
    "
        ),
        Some(&import_env),
        None,
    );
    let sema_env = TestSemaEnv::default().with_surface("std/types", types.surface().clone());
    let sema = check_module_src(
        source_id + 1,
        "main",
        &format!(
            r#"
        {main_prelude}
        let Types := import "std/types";
        {binding}
        id[Int](1);
    "#
        ),
        Some(&import_env),
        Some(&sema_env),
    );
    let root = sema.module().root;
    assert!(matches!(
        sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
            .kind,
        HirTyKind::Int
    ));
    assert!(
        !has_diag(&sema, SemaDiagKind::UnknownExport),
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

mod success {
    use super::*;

    #[test]
    fn imported_data_alias_stays_constructible() {
        let (_module_a, module_b) = check_with_imported_surface(
            40,
            r"
        export let Token := data {
          | Token(Int)
        };
        export let makeToken (value : Int) : Token := .Token(value);
    ",
            r#"
        let A := import "a";
        let Token := A.Token;
        let makeToken := A.makeToken;
        let ok : Token := makeToken(1);
        ok;
    "#,
        );
        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
    }

    #[test]
    fn imported_effect_alias_exposes_ops() {
        let (_module_a, module_b) = check_with_imported_surface(
            42,
            r"
        export let Console := effect {
          let readLine () : Int;
        };
        export let readLine () : Int := ask Console.readLine();
    ",
            r#"
        let A := import "a";
        let Console := A.Console;
        let direct () : Int := ask Console.readLine();
    "#,
        );
        assert!(
            !has_diag(&module_b, SemaDiagKind::InvalidRequestTarget),
            "{:?}",
            module_b.diags()
        );
    }

    #[test]
    fn local_data_alias_exports_surface_shape() {
        let (module_a, module_b) = check_with_imported_surface(
            45,
            r"
        let TokenBase := data {
          | Token(Int)
        };
        let TokenAlias := TokenBase;
        export let Token := TokenAlias;
        export let makeToken (value : Int) : Token := .Token(value);
    ",
            r#"
        let A := import "a";
        let Token := A.Token;
        let makeToken := A.makeToken;
        let ok : Token := makeToken(1);
        ok;
    "#,
        );
        assert!(module_a.diags().is_empty(), "{:?}", module_a.diags());
        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
    }

    #[test]
    fn chained_local_data_alias_exports_surface_shape() {
        let (module_a, module_b) = check_with_imported_surface(
            47,
            r"
        let TokenBase := data {
          | Token(Int)
        };
        let TokenMid := TokenBase;
        let TokenTop := TokenMid;
        export let Token := TokenTop;
        export let makeToken (value : Int) : Token := .Token(value);
    ",
            r#"
        let A := import "a";
        let Token := A.Token;
        let makeToken := A.makeToken;
        let ok : Token := makeToken(1);
        ok;
    "#,
        );
        assert!(module_a.diags().is_empty(), "{:?}", module_a.diags());
        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
    }

    #[test]
    fn import_exprs_type_as_import_records() {
        let src = r#"
        let IO := import "std/io";
        IO;
    "#;
        let env = TestImportEnv::default().with_module("std/io", "std/io");
        let io = check_module_src(
            10,
            "std/io",
            r"export let read (path : String) : String := path;",
            Some(&env),
            None,
        );
        let sema_env = TestSemaEnv::default().with_surface("std/io", io.surface().clone());
        let sema = check_module_src(11, "main", src, Some(&env), Some(&sema_env));
        let import_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Import { .. }))
            .expect("import expr");
        assert!(matches!(
            sema.ty(sema
                .try_expr_ty(import_expr)
                .expect("import expr type missing"))
                .kind,
            HirTyKind::Record { .. }
        ));
        assert_eq!(
            sema.expr_import_record_target(import_expr)
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
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Arrow { .. }
        ));
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn imported_module_field_access_records_member_fact() {
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
        let io := import "std/io";
        io.read;
    "#,
            Some(&import_env),
            Some(&sema_env),
        );
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("module field expr");
        let fact = sema
            .expr_member_fact(field_expr)
            .expect("module member fact missing");
        assert_eq!(fact.kind, ExprMemberKind::ImportRecordExport);
        assert!(matches!(
            sema.ty(fact.ty).kind,
            HirTyKind::Arrow { .. } | HirTyKind::Pi { .. }
        ));
    }

    #[test]
    fn dynamic_import_field_access_rejects_result() {
        let sema = check_module_src(
            14,
            "main",
            r"
        export let read_any (name : String) : Any := (
          let loaded := import name;
          loaded.value
        );
    ",
            None,
            None,
        );
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("dynamic module field expr");
        assert!(matches!(
            sema.ty(sema
                .try_expr_ty(field_expr)
                .expect("field expr type missing"))
                .kind,
            HirTyKind::Unknown
        ));
    }

    #[test]
    fn dynamic_module_field_access_is_not_callable_without_cast() {
        let sema = check_module_src(
            15,
            "main",
            r"
        export let call_any (name : String) : Any := (
          let loaded := import name;
          loaded.value()
        );
    ",
            None,
            None,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn named_call_type_mismatch_names_argument() {
        let sema = check(
            r"
        let render (port : Int, secure : Bool) : Int := port;
        render(secure := 1, port := 8080);
    ",
        );

        let diag = find_diag(&sema, SemaDiagKind::TypeMismatch).expect("type mismatch diagnostic");
        assert_eq!(
            diag.message(),
            "call argument `secure` expected `Bool`, found `Int`"
        );
        assert!(
            diag.labels()[0]
                .message()
                .contains("call argument `secure`")
        );
    }

    #[test]
    fn dot_call_resolves_receiver_first_callable() {
        let sema = check(
            r"
        let inc (self : Int, by : Int) : Int := self + by;
        let one : Int := 1;
        one.inc(2);
    ",
        );
        let call_id =
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Call { .. })).expect("call expr");
        assert!(
            matches!(
                sema.ty(sema.try_expr_ty(call_id).expect("call expr type missing"))
                    .kind,
                HirTyKind::Int
            ),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidFieldTarget),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn receiver_method_dot_call_records_attached_method_fact() {
        let sema = check(
            r"
        let (selfValue : Int).inc(by : Int) : Int := selfValue + by;
        let one : Int := 1;
        one.inc(2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("attached method field expr");
        let fact = sema
            .expr_member_fact(field_expr)
            .expect("attached method fact missing");
        assert_eq!(fact.kind, ExprMemberKind::AttachedMethod);
        assert!(fact.binding.is_some());
    }

    #[test]
    fn receiver_method_type_namespace_records_attached_method_namespace_fact() {
        let sema = check(
            r"
        let (selfValue : Int).inc(by : Int) : Int := selfValue + by;
        Int.inc(1, 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("attached method namespace field expr");
        let fact = sema
            .expr_member_fact(field_expr)
            .expect("attached method namespace fact missing");
        assert_eq!(fact.kind, ExprMemberKind::AttachedMethodNamespace);
        assert!(fact.binding.is_some());
    }

    #[test]
    fn dot_call_records_dot_callable_member_fact() {
        let sema = check(
            r"
        let inc (self : Int, by : Int) : Int := self + by;
        let one : Int := 1;
        one.inc(2);
    ",
        );
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("dot-callable field expr");
        let fact = sema
            .expr_member_fact(field_expr)
            .expect("dot-callable fact missing");
        assert_eq!(fact.kind, ExprMemberKind::DotCallable);
        assert!(fact.binding.is_some());
    }

    #[test]
    fn dot_field_resolves_receiver_first_callable_bound_function() {
        let sema = check(
            r"
        let isPositive (self : Int) : Bool := self > 0;
        let one : Int := 1;
        one.isPositive;
    ",
        );
        let root = sema.module().root;
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Arrow { .. }
        ));
    }

    #[test]
    fn record_field_access_records_member_fact() {
        let sema = check(r"let record := { value := 42 }; record.value;");
        let field_expr = find_expr(&sema, |kind| matches!(kind, HirExprKind::Field { .. }))
            .expect("record field expr");
        let fact = sema
            .expr_member_fact(field_expr)
            .expect("record member fact missing");
        assert_eq!(fact.kind, ExprMemberKind::RecordField);
        assert!(matches!(
            sema.ty(fact.ty).kind,
            HirTyKind::Int | HirTyKind::NatLit(42)
        ));
    }

    #[test]
    fn record_shaped_data_accepts_record_literal_and_field_access() {
        let sema = check(
            r#"
        let Box[T] := data {
          value : T;
        };
        let boxedName : Box[String] := {
          value := "Nora"
        };
        boxedName.value;
    "#,
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::String
        ));
    }

    #[test]
    fn record_shaped_data_reports_bad_record_literals() {
        let wrong_type = check(
            r"
        let Box[T] := data { value : T; };
        let boxedName : Box[String] := { value := 42 };
    ",
        );
        assert!(
            has_diag(&wrong_type, SemaDiagKind::TypeMismatch),
            "{:?}",
            wrong_type.diags()
        );

        let unknown_field = check(
            r#"
        let Box[T] := data { value : T; };
        let boxedName : Box[String] := { other := "Nora" };
    "#,
        );
        assert!(
            has_diag(&unknown_field, SemaDiagKind::UnknownField),
            "{:?}",
            unknown_field.diags()
        );
        assert!(
            has_diag(&unknown_field, SemaDiagKind::MissingRecordField),
            "{:?}",
            unknown_field.diags()
        );
    }

    #[test]
    fn imported_record_shaped_data_preserves_field_types() {
        let (_module_a, module_b) = check_with_imported_surface(
            70,
            r"
        export let Box[T] := data {
          value : T;
        };
    ",
            r#"
        let Types := import "a";
        let Box := Types.Box;
        let boxedName : Box[String] := {
          value := "Nora"
        };
        boxedName.value;
    "#,
        );
        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
        let root = module_b.module().root;
        assert!(matches!(
            module_b
                .ty(module_b.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::String
        ));
    }

    #[test]
    fn rune_literal_has_rune_type() {
        let sema = check("'a';");
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Rune
        ));
    }

    #[test]
    fn type_params_allow_whitespace_before_brackets() {
        let sema = check("export let identity [T] (value : T) : T := value;");
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn named_call_arguments_reorder_by_parameter_name() {
        let sema = check(
            r"
        let render (port : Int, secure : Bool) : Int := port;
        render(secure := 0 = 0, port := 8080);
    ",
        );
        let call_id =
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Call { .. })).expect("call expr");
        assert!(matches!(
            sema.ty(sema.try_expr_ty(call_id).expect("call expr type missing"))
                .kind,
            HirTyKind::Int
        ));
        assert!(
            !has_diag(&sema, SemaDiagKind::CallArityMismatch),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::CallNamedArgumentUnknown),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn request_named_arguments_follow_effect_op_parameter_names() {
        let sema = check(
            r#"
        let Console := effect {
          let readLine (prompt : String) : String;
        };
        ask Console.readLine(prompt := ">");
    "#,
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidRequestTarget),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::CallNamedArgumentUnknown),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn dot_call_resolves_visible_first_param_function() {
        let sema = check(
            r"
        let add (x : Int, y : Int) : Int := x + y;
        let one : Int := 1;
        one.add(2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn dot_call_still_supports_callable_record_fields() {
        let sema = check(
            r"
        let add (x : Int, y : Int) : Int := x + y;
        let Math := { add := add };
        Math.add(1, 2);
    ",
        );
        let call_id =
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Call { .. })).expect("call expr");
        assert!(matches!(
            sema.ty(sema.try_expr_ty(call_id).expect("call expr type missing"))
                .kind,
            HirTyKind::Int
        ));
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
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
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Arrow { .. }
        ));
        assert!(
            !has_diag(&sema, SemaDiagKind::UnknownExport),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn imported_effect_alias_handles_perform_and_handle() {
        assert_effect_alias_request("let Console := IO.Console;", 16);
    }

    #[test]
    fn perform_effects_expose_textual_names() {
        let sema = check(
            r"
        let Console := effect {
          let readLine () : String;
        };
        ask Console.readLine();
    ",
        );
        let root = sema.module().root;
        let effects = sema
            .try_expr_effects(root)
            .expect("root expr effects missing");
        assert!(
            effects
                .items
                .iter()
                .any(|item| item.name.as_ref() == "Console"),
            "{effects:?}"
        );
        assert!(effects.open.is_none(), "{effects:?}");
    }

    #[test]
    fn destructured_effect_alias_handles_perform_and_handle() {
        assert_effect_alias_request("let {Console} := IO;", 25);
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
    fn imported_callable_alias_supports_call_checking() {
        assert_imported_callable_alias(18, "", "", "let id := Types.id;");
    }

    #[test]
    fn destructured_callable_alias_supports_call_checking() {
        assert_imported_callable_alias(27, "", "", "let {id} := Types;");
    }

    #[test]
    fn imported_callable_alias_ignores_symbol_allocation_order() {
        assert_imported_callable_alias(
            23,
            r#"let warmup := "noise";"#,
            "let scratch := 42;",
            "let id := Types.id;",
        );
    }

    #[test]
    fn old_capability_forms_return_no_facts() {
        let sema = check("let value := 1; value;");
        assert!(find_expr(&sema, |kind| matches!(kind, HirExprKind::Shape { .. })).is_none());
        assert!(find_expr(&sema, |kind| matches!(kind, HirExprKind::Given { .. })).is_none());
    }

    #[test]
    fn exported_effect_ops_keep_structured_params() {
        let sema = check(
            r"
        export let Console := effect {
          @comptimeSafe
          let readLine (prompt : String) : String;
        };
    ",
        );

        let surface = sema.surface();
        let effect = surface
            .exported_effects()
            .iter()
            .find(|item| item.key.name.as_ref() == "Console")
            .expect("expected exported effect");
        assert!(effect.ops[0].is_comptime_safe);
        assert_eq!(effect.ops[0].name.as_ref(), "readLine");
        assert_eq!(effect.ops[0].param_names[0].as_ref(), "prompt");
        let prompt_ty = surface
            .try_ty(effect.ops[0].params[0])
            .expect("expected effect op param type");
        assert!(matches!(&prompt_ty.kind, SurfaceTyKind::String));
    }

    #[test]
    fn handle_with_non_handler_answer_reports_diag() {
        let sema = check(
            r#"
        let Console := effect {
          let readLine () : String;
        };
        let value := "x";
        handle ask Console.readLine() answer value;
    "#,
        );

        assert!(
            has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn reachable_imported_exports_participate_in_lookup() {
        let import_env = TestImportEnv::default()
            .with_module("a", "a")
            .with_module("b", "b");

        let module_a = check_module_src(
            20,
            "a",
            r"
        export let base : Int := 1;
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
        export let copyBase : Int := A.base;
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
        A.base + B.copyBase;
    "#,
            Some(&import_env),
            Some(&env_for_main),
        );
        assert!(main.diags().is_empty(), "{:?}", main.diags());
    }

    #[test]
    fn imported_tuple_helpers_match_local_tuple_calls() {
        let (_module_a, main) = check_with_imported_surface(
            32,
            r"
        export let first (pair : (Int, Int)) : Int := match pair (
          | (left, _) => left
        );
    ",
            r#"
        let A := import "a";
        let first := A.first;
        first((1, 2));
    "#,
        );
        assert!(main.diags().is_empty(), "{:?}", main.diags());
    }

    #[test]
    fn explicit_type_apply_instantiates_local_generic_lets() {
        let sema = check(
            r#"
        let id[T] (x : T) : T := x;
        (id[Int](1), id[String]("ok"));
    "#,
        );
        let root = sema.module().root;
        let HirTyKind::Tuple { items } = sema
            .ty(sema.try_expr_ty(root).expect("root expr type missing"))
            .kind
        else {
            panic!(
                "expected tuple type, got {:?}",
                sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                    .kind
            );
        };
        let items = sema.module().store.ty_ids.get(items);
        assert!(items.len() > 1);
        assert!(matches!(sema.ty(items[0]).kind, HirTyKind::Int));
        assert!(matches!(sema.ty(items[1]).kind, HirTyKind::String));
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn type_apply_instantiates_generic_values_stored_in_records() {
        let sema = check(
            r"
        let id[T] (x : T) : T := x;
        let tools := { id := id };
        tools.id[Int](1);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Int
        ));
    }

    #[test]
    fn explicit_type_apply_instantiates_imported_generic_exports() {
        let import_env = TestImportEnv::default().with_module("std/base", "std/base");
        let base = check_module_src(
            34,
            "std/base",
            r"
        export let id[T] (x : T) : T := x;
    ",
            Some(&import_env),
            None,
        );
        let sema_env = TestSemaEnv::default().with_surface("std/base", base.surface().clone());
        let sema = check_module_src(
            35,
            "main",
            r#"
        let Base := import "std/base";
        (Base.id[Int](1), Base.id[String]("ok"));
    "#,
            Some(&import_env),
            Some(&sema_env),
        );
        let root = sema.module().root;
        let HirTyKind::Tuple { items } = sema
            .ty(sema.try_expr_ty(root).expect("root expr type missing"))
            .kind
        else {
            panic!(
                "expected tuple type, got {:?}",
                sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                    .kind
            );
        };
        let items = sema.module().store.ty_ids.get(items);
        assert!(items.len() > 1);
        assert!(matches!(sema.ty(items[0]).kind, HirTyKind::Int));
        assert!(matches!(sema.ty(items[1]).kind, HirTyKind::String));
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn type_equality_constraints_succeed_for_matching_type_apply() {
        let sema = check(
            r"
        let requireSame[T, U] (x : T) : T where T ~= U := x;
        requireSame[Int, Int](1);
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::UnsatisfiedConstraint),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn named_member_call_uses_record_callable_value() {
        let sema = check(
            r"
        let eq (left : Int, right : Int) : Bool := left = right;
        let Ops := { eq := eq };
        Ops.eq(1, 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn callable_values_in_records_infer_argument_types() {
        let sema = check(
            r"
        let eq (left : Int, right : Int) : Bool := left = right;
        let Ops := { eq := eq };
        Ops.eq(1, 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn callable_values_in_records_reject_inconsistent_arguments() {
        let sema = check(
            r#"
        let eq (left : Int, right : Int) : Bool := left = right;
        let Ops := { eq := eq };
        Ops.eq(1, "x");
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_equality_constraints_report_unsatisfied_mismatch() {
        let sema = check(
            r"
        let requireSame[T, U] (x : T) : T where T ~= U := x;
        requireSame[Int, String](1);
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::UnsatisfiedConstraint),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_equality_constraints_report_mismatch_on_calls() {
        let sema = check(
            r"
        let requireSame[T, U] (x : T) : T where T ~= U := x;
        requireSame[Int, String](1);
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::UnsatisfiedConstraint),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn exported_polymorphic_constrained_callable_reports_diag() {
        let sema = check(
            r"
        export let requireSame[T, U] (x : T) : T where T ~= U := x;
    ",
        );
        assert!(
            has_diag(
                &sema,
                SemaDiagKind::ExportedCallableRequiresConcreteConstraints
            ),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn fixed_array_dims_validate_literal_length() {
        let sema = check("let xs : [2]Int := [1, 2];");
        assert!(
            !has_diag(&sema, SemaDiagKind::ArrayLiteralLengthMismatch),
            "{:?}",
            sema.diags()
        );

        let sema = check("let xs : [2]Int := [1, 2, 3];");
        assert!(
            has_diag(&sema, SemaDiagKind::ArrayLiteralLengthMismatch),
            "{:?}",
            sema.diags()
        );

        let sema = check("let xs : [2]Int := [1, 2];");
        assert!(
            !has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn slice_type_syntax_accepts_dynamic_arrays() {
        let sema = check("let xs : []Any := [1, \"x\"];");
        assert!(!has_diag(&sema, SemaDiagKind::ArrayLiteralLengthMismatch));
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn multi_index_arrays_check_expected_arity() {
        let sema = check(
            r"
        export let touch (grid : mut [2][2]Int) : Int := (
          grid.[0, 1] := 7;
          grid.[0, 1]
        );
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidIndexArgCount),
            "{:?}",
            sema.diags()
        );

        let sema = check(
            r"
        export let touch (grid : mut [2][2]Int) : Int := grid.[0];
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidIndexArgCount),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn ranges_typecheck_and_membership_is_bool() {
        let sema = check(
            r#"
        let Core := import "musi:core";
        let Bool := Core.Bool;
        let Int := Core.Int;
        let Range := Core.Range;
        let Rangeable := Core.Rangeable;
        let RangeBounds := Core.RangeBounds;
        let xs : Range[Int] := 1 ..< 4;
        let closed : Range[Int] := 1 .. 4;
        let openClosed : Range[Int] := 1 <.. 4;
        let openOpen : Range[Int] := 1 <..< 4;
        let from : Range[Int] := 1 ..;
        let fromExclusive : Range[Int] := 1 <..;
        let thru : Range[Int] := .. 4;
        let upTo : Range[Int] := ..< 4;
        let ok : Bool := 2 in xs;
    "#,
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::BinaryOperatorHasNoExecutableLowering),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn local_recursive_callable_let_typechecks() {
        let sema = check(
            r"
        export let recurseDown (n : Int) : Int := (
          let rec loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
          loop(n)
        );
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn piped_calls_typecheck_without_binary_lowering_diag() {
        let sema = check(
            r"
        export let add (left : Int, right : Int) : Int := left + right;
        export let result : Int := 1 |> add(2);
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::BinaryOperatorHasNoExecutableLowering),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn request_effect_rows_capture_declared_effects() {
        let sema = check(
            r"
        let State := effect {
          let readLine () : String;
        };
        let readState () : String := ask State.readLine();
        readState();
    ",
        );
        let root = sema.module().root;
        let effects = sema
            .try_expr_effects(root)
            .expect("root expr effects missing");
        assert!(
            effects
                .items
                .iter()
                .any(|item| item.name.as_ref() == "State"),
            "{effects:?}"
        );
        assert!(effects.open.is_none(), "{effects:?}");
        assert!(
            !has_diag(&sema, SemaDiagKind::UnknownEffect),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn handle_with_bound_answer_value_parses_current_syntax() {
        let sema = check(
            r"
        let Console := effect {
          let readLine () : Int;
        };
        let h := answer Console;
        handle ask Console.readLine() answer h;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::UnknownEffect),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn any_expected_matches_concrete_types() {
        let sema = check(
            r"
        let idAny (x : Any) : Any := x;
        idAny(1);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn array_and_record_spreads_typecheck() {
        let sema = check(
            r"
        let xs := [1, 2];
        let ys := [0, ...xs, 3];

        let p := { x := 1, y := 2 };
        let q := { ...p, x := 3 };
        let r := { ...p, ...q, y := 9 };

        ys;
        q;
        r;
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn call_spreads_typecheck_for_tuples_and_any_seq() {
        let sema = check(
            r#"
        let f (a : Int, b : String) : Int := a;
        let t := (1, "x");
        f(...t);

        let g (a : Any, b : Any) : Any := a;
        let xs : []Any := [1, "x"];
        g(...xs);
    "#,
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn sum_constructors_and_patterns_typecheck() {
        let sema = check(
            r"
        let x : Int + String := .Left(1);
        match x (
          | .Left(n) => n
          | .Right(_) => 0
        );
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn named_variant_fields_support_named_construction_and_shorthand_patterns() {
        let sema = check(
            r"
        let Port := data {
          | Configured(port : Int, secure : Bool)
          | Default
        };
        let port : Port := .Configured(secure := 0 = 0, port := 8080);
        let value : Int := match port (
          | .Configured(port, secure := _) => port
          | .Default => 0
        );
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn mixed_variant_payload_style_reports_diag() {
        let sema = check(
            r"
        let Port := data {
          | Configured(Int, secure : Bool)
        };
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::MixedVariantPayloadStyle),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn foreign_call_inside_unsafe_block_passes_unsafe_check() {
        let module = check(
            r#"
        native "c" let clock () : Int;
        let value := unsafe { clock(); };
    "#,
        );
        assert!(!has_diag(
            &module,
            SemaDiagKind::UnsafeCallRequiresUnsafeBlock
        ));
    }

    #[test]
    fn unsafe_pin_accepts_pinnable_target() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe pin xs as pinned in 1;
    ",
        );
        assert!(!has_diag(&module, SemaDiagKind::UnsupportedPinTarget));
        assert!(!has_diag(&module, SemaDiagKind::PinnedValueEscapes));
    }

    #[test]
    fn unsafe_pin_rejects_scalar_target() {
        let module = check(
            r"
        let value := unsafe pin 1 as pinned in 0;
    ",
        );
        let diag =
            find_diag(&module, SemaDiagKind::UnsupportedPinTarget).expect("pin target diagnostic");
        assert_eq!(diag.message(), "unsupported pin target `Int`");
    }

    #[test]
    fn unsafe_pin_rejects_returned_pin_handle() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe pin xs as pinned in pinned;
    ",
        );
        let diag =
            find_diag(&module, SemaDiagKind::PinnedValueEscapes).expect("pin escape diagnostic");
        assert_eq!(diag.message(), "pinned value `pinned` escapes pin scope");
    }

    #[test]
    fn generic_callable_accepts_unary_data_constructor() {
        let sema = check(
            r"
        let Flag := data {
          | On
          | Off
        };
        let keep[T] (value : T) : T := value;
        keep[Flag](.On);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn generic_callable_accepts_fully_applied_data_constructor() {
        let sema = check(
            r#"
        let Pair := data {
          | Pair(String, Int)
        };
        let keep[T] (value : T) : T := value;
        keep[Pair](.Pair("x", 1));
    "#,
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn partial_let_is_accepted_as_totality_modifier() {
        let sema = check("partial let parseInt(text : String) : Int := 0;");
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidPartialModifier),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::PartialForeignConflict),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn indexed_variant_result_clause_is_recorded() {
        let sema = check(
            r"
        let Vec[T, n] := data {
          | Nil() -> Vec[T, 0]
          | Cons(head : T, tail : Vec[T, n]) -> Vec[T, n + 1]
        };
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let vec = sema.data_def("Vec").expect("Vec data definition");
        assert!(
            vec.variant("Nil")
                .and_then(SemaDataVariantDef::result)
                .is_some()
        );
        assert!(
            vec.variant("Cons")
                .and_then(SemaDataVariantDef::result)
                .is_some()
        );
    }

    #[test]
    fn type_equality_constraint_accepts_matching_type_application() {
        let sema = check(
            r"
        let same[A, B] (value : A) : A where A ~= B := value;
        let result := same[Int, Int](42);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn type_universe_names_accept_numeric_levels() {
        let sema = check("let id[T : Type0] (value : T) : T := value;");
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn comptime_diag_kind_roundtrips_code() {
        assert_eq!(
            SemaDiagKind::from_code(DiagCode::new(3102)),
            Some(SemaDiagKind::AmbiguousDotCallable)
        );
        assert_eq!(
            SemaDiagKind::from_code(DiagCode::new(3122)),
            Some(SemaDiagKind::RuntimeValueInComptimeContext)
        );
    }

    #[test]
    fn data_variant_discriminants_accept_const_int_expressions() {
        let sema = check(
            r"
        let base : Int := comptime 20;
        let Level := data {
          | Debug := 10
          | Warn := base + 10
        };
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let level = sema.data_def("Level").expect("Level data definition");
        assert_eq!(
            level.variant("Debug").map(SemaDataVariantDef::tag),
            Some(10)
        );
        assert_eq!(level.variant("Warn").map(SemaDataVariantDef::tag), Some(30));
    }

    #[test]
    fn comptime_prefix_accepts_const_int_expressions() {
        let sema = check(
            r"
        let x : Int := comptime (1 + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn comptime_quote_expands_expression_type() {
        let sema = check(
            r"
        let value : Int := comptime quote (40 + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn comptime_quote_splices_primitive_literals_into_syntax() {
        let sema = check(
            r"
        let base : Int := comptime 40;
        let generated : Syntax := comptime quote (#(base) + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn comptime_prefix_accepts_runtime_expressions_for_ctfe() {
        let sema = check(
            r"
        let runtime () : Int := 1;
        let x : Int := comptime runtime();
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn imported_comptime_param_metadata_accepts_runtime_arguments_for_ctfe() {
        let (_module_a, module_b) = check_with_imported_surface(
            210,
            r"
        export let scale (comptime n : Int, x : Int) : Int := x * n;
    ",
            r#"
        let A := import "a";
        let scale := A.scale;
        let runtime () : Int := 3;
        let y : Int := scale(runtime(), 2);
    "#,
        );
        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
    }

    #[test]
    fn comptime_params_accept_runtime_arguments_for_ctfe() {
        let sema = check(
            r"
        let scale (comptime n : Int, x : Int) : Int := x * n;
        let runtime () : Int := 3;
        let y : Int := scale(runtime(), 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }
}

mod failure {
    use super::*;

    #[test]
    fn known_attr_requires_foundation_module() {
        let sema = check(
            r#"
        @musi.known(name := "Type")
        export let Type := Type;
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AttrKnownRequiresFoundationModule),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn intrinsic_attr_requires_foreign_let_in_intrinsics_module() {
        let sema = check_module_src(
            44,
            "musi:intrinsics",
            r#"
        @musi.intrinsic(name := "ptr.load")
        let ptrLoad := 1;
    "#,
            None,
            None,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AttrIntrinsicRequiresForeignLet),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn frozen_attr_requires_exported_non_opaque_data() {
        let sema = check(
            r"
        @frozen
        let Token := data {
          | Token(Int)
        };
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AttrFrozenRequiresExportedNonOpaqueData),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn profile_conflict_on_callable() {
        let sema = check(
            r"
        @profile(level := .hot)
        @profile(level := .cold)
        let work () : Int := 1;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AttrHotColdConflict),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn exported_plain_value_with_wrong_type_reports_type_mismatch() {
        let sema = check(r#"export let hidden : Int := "no";"#);
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn duplicate_receiver_first_callables_report_ambiguity() {
        let sema = check(
            r"
        let dup (self : Int) : Int := self;
        let dup (self : Int) : Int := self + 1;
        let one : Int := 1;
        one.dup();
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AmbiguousDotCallable),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn handle_with_non_handler_answer_reports_diag() {
        let sema = check(
            r#"
        let Console := effect {
          let readLine () : String;
        };
        let value := "ok";
        handle ask Console.readLine() answer value;
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_test_target_rejects_mut() {
        let sema = check(
            r"
        export let check (x : Any) : Bool := x :? mut Int;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::MutForbiddenInTypeTestTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_cast_target_rejects_mut() {
        let sema = check(
            r"
        export let cast (x : Any) : Int := x :?> mut Int;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::MutForbiddenInTypeCastTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn missing_required_record_field_reports_diag() {
        let sema = check(
            r"
        let Box[T] := data { value : T; };
        let boxed : Box[Int] := {};
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::MissingRecordField),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn unknown_record_field_reports_diag() {
        let sema =
            check(r"let Box[T] := data { value : T; }; let boxed : Box[Int] := { other := 1 };");
        assert!(
            has_diag(&sema, SemaDiagKind::UnknownField),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn imported_surfaces_do_not_conflict_for_distinct_exports() {
        let import_env = TestImportEnv::default()
            .with_module("a", "a")
            .with_module("b", "b");

        let module_a = check_module_src(
            29,
            "a",
            r"
        export let valueA : Int := 1;
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
        export let valueB : Int := A.valueA + 1;
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
        A.valueA + B.valueB;
    "#,
            Some(&import_env),
            Some(&env_for_main),
        );
        assert!(main.diags().is_empty(), "{:?}", main.diags());
    }

    #[test]
    fn constrained_non_callable_let_reports_diag() {
        let sema = check(
            r"
        let x : Int where Int ~= Int := 1;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::ConstrainedNonCallableBinding),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn assignment_requires_mut_location() {
        let sema = check("let x : Int := 1; x := 2;");
        assert!(
            has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );

        let sema = check("let x := mut 1; x := 2;");
        assert!(
            !has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn write_through_requires_mut_type() {
        let sema = check(
            r"
        let xs := [1, 2];
        xs.[0] := 3;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );

        let sema = check(
            r"
        let xs := mut [1, 2];
        xs.[0] := 3;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );

        let sema = check(
            r"
        let r := { x := 1 };
        r.x := 2;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );

        let sema = check(
            r"
        let r := mut { x := 1 };
        r.x := 2;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::WriteTargetRequiresMut),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn rejects_invalid_field_access_empty_index_and_callable_pattern() {
        let sema = check("let value := 1.x;");
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidFieldTarget),
            "{:?}",
            sema.diags()
        );

        let sema = check(
            r"
        let grid : [2][2]Int := [[1, 2], [3, 4]];
        let value := grid.[];
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidIndexArgCount),
            "{:?}",
            sema.diags()
        );

        let sema = check("let (f) (x : Int) : Int := x;");
        assert!(
            has_diag(&sema, SemaDiagKind::CallableLetRequiresSimpleBindingPattern),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn request_without_effect_op_target_reports_diag() {
        let sema = check(
            r#"
        let State := effect {
          let readLine () : String;
        };
        let notEffect := { readLine := \() => "" };
        ask notEffect.readLine();
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidRequestTarget),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn invalid_link_attr_target_reports_diag() {
        let sema = check("@link(name := \"c\") let x := 1;");
        assert!(
            has_diag(&sema, SemaDiagKind::AttrLinkRequiresForeignLet),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn foreign_call_requires_unsafe_block() {
        let module = check(
            r#"
        native "c" let clock () : Int;
        let value := clock();
    "#,
        );
        assert!(has_diag(
            &module,
            SemaDiagKind::UnsafeCallRequiresUnsafeBlock
        ));
    }

    #[test]
    fn type_equality_constraint_rejects_mismatched_type_application() {
        let sema = check(
            r"
        let same[A, B] (value : A) : A where A ~= B := value;
        let result := same[Int, String](42);
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::UnsatisfiedConstraint),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn data_variant_discriminants_reject_plain_let_values() {
        let sema = check(
            r"
        let base : Int := 20;
        let Level := data {
          | Warn := base + 10
        };
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidDataVariantDiscriminant),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn comptime_prefix_accepts_non_int_literals() {
        let sema = check(
            r#"
        let text : String := comptime "hello";
        let unit : Unit := comptime ();
    "#,
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn data_variant_discriminants_reject_duplicate_values() {
        let sema = check(
            r"
        let Level := data {
          | Debug := 10
          | Trace := 10
        };
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::DuplicateDataVariantDiscriminant),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn data_variant_discriminants_reject_runtime_expressions() {
        let sema = check(
            r"
        let level () : Int := 10;
        let Level := data {
          | Debug := level()
        };
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidDataVariantDiscriminant),
            "{:?}",
            sema.diags()
        );
    }
}
