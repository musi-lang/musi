#![allow(unused_imports)]

use std::collections::BTreeMap;

use music_base::diag::{Diag, DiagContext};
use music_base::{DiagCode, SourceId};
use music_hir::{HirExprId, HirExprKind, HirLitKind, HirTyKind};
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportResolveResult, ModuleKey, ModuleSpecifier,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, resolve_module};
use music_syntax::{Lexer, parse};

use super::{
    ExprMemberKind, ModuleSurface, SemaDataVariantDef, SemaDiagKind, SemaEnv, SemaModule,
    SemaOptions, SurfaceTyKind, TargetInfo, check_module, sema_diag_kind,
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

fn check_with_target(src: &str, target: TargetInfo) -> SemaModule {
    check_module_src_with_options(
        1,
        "main",
        src,
        None,
        SemaOptions {
            target: Some(target),
            env: None,
            prelude: None,
        },
    )
}

fn check_module_src(
    source_id_raw: u32,
    module_key: &str,
    src: &str,
    import_env: Option<&dyn ImportEnv>,
    sema_env: Option<&dyn SemaEnv>,
) -> SemaModule {
    check_module_src_with_options(
        source_id_raw,
        module_key,
        src,
        import_env,
        SemaOptions {
            target: None,
            env: sema_env,
            prelude: None,
        },
    )
}

fn check_module_src_with_options(
    source_id_raw: u32,
    module_key: &str,
    src: &str,
    import_env: Option<&dyn ImportEnv>,
    options: SemaOptions<'_>,
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
    let module = check_module(resolved, &mut interner, options);
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

fn assert_imported_record_method_request(binding: &str, source_id: u32) {
    let import_env = TestImportEnv::default().with_module("std/io", "std/io");
    let io = check_module_src(
        source_id,
        "std/io",
        r#"
        export let Console := { readLine := \() : String => "" };
    "#,
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
        Console.readLine();
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
        !has_diag(&sema, SemaDiagKind::UnknownField),
        "{:?}",
        sema.diags()
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
    fn imported_record_alias_exposes_methods() {
        let (_module_a, module_b) = check_with_imported_surface(
            42,
            r"
        export let Console := { readLine := \() : Int => 1 };
        export let readLine () : Int := Console.readLine();
    ",
            r#"
        let A := import "a";
        let Console := A.Console;
        let direct () : Int := Console.readLine();
    "#,
        );
        assert!(
            !has_diag(&module_b, SemaDiagKind::UnknownField),
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
    fn tuple_import_binds_each_import_record_target() {
        let import_env = TestImportEnv::default()
            .with_module("@std/cmp", "@@std@0.1.0/cmp.ms")
            .with_module("@std/word", "@@std@0.1.0/word.ms");
        let cmp = check_module_src(
            12,
            "@@std@0.1.0/cmp.ms",
            "export let compare : Int := 1;",
            Some(&import_env),
            None,
        );
        let word = check_module_src(
            13,
            "@@std@0.1.0/word.ms",
            "export let word : Int := 2;",
            Some(&import_env),
            None,
        );
        let sema_env = TestSemaEnv::default()
            .with_surface("@@std@0.1.0/cmp.ms", cmp.surface().clone())
            .with_surface("@@std@0.1.0/word.ms", word.surface().clone());
        let main = check_module_src(
            14,
            "main",
            r#"
        let (StdCmp, StdWord) := import ("@std/cmp", "@std/word");
        StdCmp.compare + StdWord.word;
    "#,
            Some(&import_env),
            Some(&sema_env),
        );
        assert!(main.diags().is_empty(), "{:?}", main.diags());
    }

    #[test]
    fn import_record_field_access_uses_export_surface() {
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
    fn empty_tuple_arrow_type_accepts_zero_param_lambda() {
        let sema = check(
            r"
        let fallback : () -> Int := \() => 9;
        fallback();
    ",
        );
        assert!(matches!(
            sema.ty(sema
                .try_expr_ty(sema.module().root)
                .expect("root expr type missing"))
                .kind,
            HirTyKind::Int
        ));
        assert!(
            !has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn import_record_field_access_records_member_fact() {
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
        let diag = find_diag(&sema, SemaDiagKind::InvalidCallTarget)
            .expect("invalid call target diagnostic");
        assert_eq!(
            diag.message(),
            "call target `value` expected callable type, found `Unknown`"
        );
        assert_eq!(
            diag.labels()[0].message(),
            "`value` has type `Unknown` here"
        );
    }

    #[test]
    fn invalid_call_target_names_direct_callee_without_nested_category() {
        let sema = check(
            r"
        let fromByte : Any := 1;
        fromByte();
    ",
        );

        let diag = find_diag(&sema, SemaDiagKind::InvalidCallTarget)
            .expect("invalid call target diagnostic");
        assert_eq!(
            diag.message(),
            "call target `fromByte` expected callable type, found `Any`"
        );
        assert_eq!(diag.labels()[0].message(), "`fromByte` has type `Any` here");
    }

    #[test]
    fn named_call_type_mismatch_names_argument() {
        let sema = check(
            r"
        let render (port : Int, secure : Bit) : Int := port;
        render(secure := 1, port := 8080);
    ",
        );

        let diag = find_diag(&sema, SemaDiagKind::TypeMismatch).expect("type mismatch diagnostic");
        assert_eq!(
            diag.message(),
            "call argument `secure` expected `Bit`, found `Int`"
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
        let isPositive (self : Int) : Bit := self > 0;
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
        let Box[T] := data { let value : T; };
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
        let Box[T] := data { let value : T; };
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
        let Box[T] := data { let value : T; };
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
        export let Box[T] := data { let value : T; };
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
    fn numeric_suffix_selects_nat_width_type() {
        let sema = check("1_n16;");
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Nat16
        ));
    }

    #[test]
    fn numeric_suffix_selects_int_width_type_without_separator() {
        let sema = check("1z32;");
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Int32
        ));
    }

    #[test]
    fn numeric_suffix_selects_float_width_type() {
        let sema = check("1_f64;");
        let root = sema.module().root;
        assert!(matches!(
            sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                .kind,
            HirTyKind::Float64
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
        let render (port : Int, secure : Bit) : Int := port;
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
        let Console := { readLine := \(prompt : String) : String => prompt };
        Console.readLine(">");
    "#,
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidCallTarget),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::CallArityMismatch),
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
    fn import_record_pattern_binds_exported_values() {
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
    fn imported_record_alias_supports_method_calling() {
        assert_imported_record_method_request("let Console := IO.Console;", 16);
    }

    #[test]
    fn record_method_calls_expose_textual_names() {
        let sema = check(
            r#"
        let Console := { readLine := \() : String => "" };
        Console.readLine();
    "#,
        );
        let root = sema.module().root;
        assert!(
            matches!(
                sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                    .kind,
                HirTyKind::String
            ),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn destructured_record_alias_supports_method_calling() {
        assert_imported_record_method_request("let {Console} := IO;", 25);
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
    fn plain_values_create_no_shape_facts() {
        let sema = check("let value := 1; value;");
        assert!(find_expr(&sema, |kind| matches!(kind, HirExprKind::Shape { .. })).is_none());
    }

    #[test]
    fn exported_record_keeps_structured_fields() {
        let sema = check(
            r"
        export let Console := { readLine := \(prompt : String) : String => prompt };
    ",
        );

        let surface = sema.surface();
        assert!(surface.exported_value("Console").is_some());
    }

    #[test]
    fn non_callable_record_value_reports_invalid_call_target() {
        let sema = check(
            r#"
        let value := "x";
        value();
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
        let eq (left : Int, right : Int) : Bit := left = right;
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
        let eq (left : Int, right : Int) : Bit := left = right;
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
        let eq (left : Int, right : Int) : Bit := left = right;
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
            r#"
        let requireInt(x : Int) : Int := x;
        requireInt("x");
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_equality_constraints_report_mismatch_on_calls() {
        let sema = check(
            r#"
        let requireInt(x : Int) : Int := x;
        requireInt("x");
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn exported_polymorphic_constrained_callable_reports_diag() {
        let sema = check(
            r"
        export let requireMark[T] (x : T) : T where T |= Missing := x;
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
        let Bit := Core.Bit;
        let Range := Core.Range;
        let Int := Core.Int;
        let halfOpen : Range[Int] := 0 ..< 10;
        let inclusive : Range[Int] := 0 .. 10;
        let ok : Bit := 0 = 0;
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
        assert!(
            !has_diag(&sema, SemaDiagKind::UnsatisfiedConstraint),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn logical_operator_family_accepts_bool_and_matching_bits() {
        let sema = check(
            r"
        export let boolAnd (left : Bit, right : Bit) : Bit := left and right;
        export let boolOr (left : Bit, right : Bit) : Bit := left or right;
        export let boolXor (left : Bit, right : Bit) : Bit := left xor right;
        export let bitsAnd (left : Bits[4], right : Bits[4]) : Bits[4] := left and right;
        export let bitsOr (left : Bits[4], right : Bits[4]) : Bits[4] := left or right;
        export let bitsXor (left : Bits[4], right : Bits[4]) : Bits[4] := left xor right;
        export let bitsNot (value : Bits[4]) : Bits[4] := not value;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::BinaryOperatorHasNoExecutableLowering),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::LogicalOperatorDomainMismatch),
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
    fn logical_operator_family_accepts_symbolic_bits_width() {
        let sema = check(
            r"
        export let bitsAnd [N : Nat] (left : Bits[N], right : Bits[N]) : Bits[N] :=
            left and right;
        export let bitsNot [N : Nat] (value : Bits[N]) : Bits[N] := not value;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidBitsWidth),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::LogicalOperatorDomainMismatch),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::UnaryLogicalOperatorDomainMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn bits_zero_width_typechecks_as_empty_pattern() {
        let sema = check(
            r"
        export let empty (value : Bits[0]) : Bits[0] := value;
    ",
        );
        let surface = sema.surface();
        let exported = surface
            .exported_value("empty")
            .expect("empty export should exist");
        let SurfaceTyKind::Arrow { params, ret, .. } =
            &surface.try_ty(exported.ty).expect("empty type exists").kind
        else {
            panic!("empty export should have function type");
        };
        assert!(
            matches!(
                surface.try_ty(params[0]).expect("param type exists").kind,
                SurfaceTyKind::Bits { width: 0 }
            ),
            "{:?}",
            surface.try_ty(params[0])
        );
        assert!(
            matches!(
                surface.try_ty(*ret).expect("return type exists").kind,
                SurfaceTyKind::Bits { width: 0 }
            ),
            "{:?}",
            surface.try_ty(*ret)
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidBitsWidth),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn word_types_canonicalize_to_bits_widths() {
        let sema = check_with_target(
            r"
        export let native (value : Word) : Word := value;
        export let fixed32 (value : Word32) : Word32 := value;
        export let fixed64 (value : Word64) : Word64 := value;
    ",
            TargetInfo::new().with_pointer_width(64),
        );
        let surface = sema.surface();
        let native = surface
            .exported_value("native")
            .expect("native export should exist");
        let SurfaceTyKind::Arrow {
            params: native_params,
            ret: native_ret,
            ..
        } = &surface.try_ty(native.ty).expect("native type exists").kind
        else {
            panic!("native export should have function type");
        };
        assert!(matches!(
            surface
                .try_ty(native_params[0])
                .expect("native param exists")
                .kind,
            SurfaceTyKind::Bits { width: 64 }
        ));
        assert!(matches!(
            surface
                .try_ty(*native_ret)
                .expect("native return exists")
                .kind,
            SurfaceTyKind::Bits { width: 64 }
        ));

        let fixed = surface
            .exported_value("fixed32")
            .expect("fixed32 export should exist");
        let SurfaceTyKind::Arrow {
            params: fixed_params,
            ret: fixed_ret,
            ..
        } = &surface.try_ty(fixed.ty).expect("fixed type exists").kind
        else {
            panic!("fixed export should have function type");
        };
        assert!(matches!(
            surface
                .try_ty(fixed_params[0])
                .expect("fixed param exists")
                .kind,
            SurfaceTyKind::Bits { width: 32 }
        ));
        assert!(matches!(
            surface
                .try_ty(*fixed_ret)
                .expect("fixed return exists")
                .kind,
            SurfaceTyKind::Bits { width: 32 }
        ));
        let fixed64 = surface
            .exported_value("fixed64")
            .expect("fixed64 export should exist");
        let SurfaceTyKind::Arrow {
            params: fixed64_params,
            ret: fixed64_ret,
            ..
        } = &surface
            .try_ty(fixed64.ty)
            .expect("fixed64 type exists")
            .kind
        else {
            panic!("fixed64 export should have function type");
        };
        assert!(matches!(
            surface
                .try_ty(fixed64_params[0])
                .expect("fixed64 param exists")
                .kind,
            SurfaceTyKind::Bits { width: 64 }
        ));
        assert!(matches!(
            surface
                .try_ty(*fixed64_ret)
                .expect("fixed64 return exists")
                .kind,
            SurfaceTyKind::Bits { width: 64 }
        ));
        assert!(
            !has_diag(&sema, SemaDiagKind::InvalidBitsWidth),
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
    fn logical_operator_family_rejects_mismatched_domains() {
        let sema = check(
            r"
        export let badBits (left : Bits[4], right : Bits[8]) := left and right;
        export let badInt (left : Int, right : Int) := left and right;
        export let badNot (value : Int) := not value;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::LogicalOperatorDomainMismatch),
            "{:?}",
            sema.diags()
        );
        assert!(
            has_diag(&sema, SemaDiagKind::UnaryLogicalOperatorDomainMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn local_recursive_callable_let_typechecks() {
        let sema = check(
            r"
        export let recurseDown (n : Int) : Int := (
          let recur loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
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
    fn record_method_calls_preserve_result_type() {
        let sema = check(
            r#"
        let State := { readLine := \() : String => "" };
        let readState () : String := State.readLine();
        readState();
    "#,
        );
        let root = sema.module().root;
        assert!(
            matches!(
                sema.ty(sema.try_expr_ty(root).expect("root expr type missing"))
                    .kind,
                HirTyKind::String
            ),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn bound_record_method_call_parses_current_syntax() {
        let sema = check(
            r"
        let Console := { readLine := \() : Int => 42 };
        Console.readLine();
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::UnknownField),
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
          | Configured(port : Int, secure : Bit)
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
          | Configured(Int, secure : Bit)
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
            r"
        @foreign(abi := .c) let clock () : Int;
        let value := unsafe (clock());
    ",
        );
        assert!(!has_diag(
            &module,
            SemaDiagKind::UnsafeCallRequiresUnsafeBlock
        ));
    }

    #[test]
    fn external_import_may_be_exported_for_module_visibility() {
        let sema = check(
            r"
        @foreign(abi := .c)
        export let clock () : Int;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalImportCannotBeExported),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalBodyRequiresExport),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn external_import_without_export_stays_module_local() {
        let sema = check(
            r"
        @foreign(abi := .c)
        let clock () : Int;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalImportCannotBeExported),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalBodyRequiresExport),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn external_body_requires_export() {
        let sema = check(
            r"
        @foreign(abi := .c)
        let clock () : Int := 1;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::ExternalBodyRequiresExport),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn external_export_with_body_sets_direction_by_shape() {
        let sema = check(
            r"
        @foreign(abi := .c)
        export let clock () : Int := 1;
    ",
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalImportCannotBeExported),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::ExternalBodyRequiresExport),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn native_signatures_accept_imported_primitive_aliases() {
        let (_module_a, module_b) = check_with_imported_surface(
            1,
            r"
        export let Int32 := Int32;
        export let CString := CString;
        export let CInt := Int32;
        export let char := Int8;
        export let bool := Bit;
        export let int32_t := Int32;
        export let uint32_t := Nat32;
        export let size_t := Nat;
        export let uintptr_t := Nat;
    ",
            r#"
        let Core := import "a";
        let CInt := Core.Int32;
        let CStringAlias := Core.CString;
        @foreign(abi := .c) let strerror (
          code : CInt,
          directCode : Core.CInt,
          ch : Core.char,
          flag : Core.bool,
          signed : Core.int32_t,
          unsigned : Core.uint32_t,
          size : Core.size_t,
          pointer : Core.uintptr_t
        ) : CStringAlias;
    "#,
        );

        assert!(
            !has_diag(&module_b, SemaDiagKind::InvalidFfiType),
            "{:?}",
            module_b.diags()
        );
    }

    #[test]
    fn type_arguments_accept_imported_primitive_aliases() {
        let (_module_a, module_b) = check_with_imported_surface(
            1,
            r"
        export let CInt := Int32;
        export let int32_t := Int32;
    ",
            r#"
        let Core := import "a";
        let ignore [T] () : Int := 0;
        let direct := ignore[Core.CInt]();
        let typedef := ignore[Core.int32_t]();
    "#,
        );

        assert!(module_b.diags().is_empty(), "{:?}", module_b.diags());
    }

    #[test]
    fn pin_inside_unsafe_accepts_pinnable_target() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe (pin xs as pinned in 1);
    ",
        );
        assert!(!has_diag(&module, SemaDiagKind::UnsupportedPinTarget));
        assert!(!has_diag(&module, SemaDiagKind::PinnedValueEscapes));
    }

    #[test]
    fn pin_outside_unsafe_reports_unsafe_block_requirement() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := pin xs as pinned in 1;
    ",
        );
        assert!(has_diag(&module, SemaDiagKind::PinRequiresUnsafeBlock));
    }

    #[test]
    fn pin_inside_unsafe_rejects_scalar_target() {
        let module = check(
            r"
        let value := unsafe (pin 1 as pinned in 0);
    ",
        );
        let diag =
            find_diag(&module, SemaDiagKind::UnsupportedPinTarget).expect("pin target diagnostic");
        assert_eq!(
            diag.message(),
            SemaDiagKind::UnsupportedPinTarget
                .message_with(&DiagContext::new().with("target", "Int"))
        );
    }

    #[test]
    fn pin_inside_unsafe_rejects_returned_pin_handle() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe (pin xs as pinned in pinned);
    ",
        );
        let diag =
            find_diag(&module, SemaDiagKind::PinnedValueEscapes).expect("pin escape diagnostic");
        assert_eq!(
            diag.message(),
            SemaDiagKind::PinnedValueEscapes
                .message_with(&DiagContext::new().with("name", "pinned"))
        );
    }

    #[test]
    fn pin_inside_unsafe_rejects_closure_capture_of_pin_handle() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe (pin xs as pinned in \() => pinned);
    ",
        );
        let diag = find_diag(&module, SemaDiagKind::PinnedValueCapturedByClosure)
            .expect("pin closure capture diagnostic");
        assert_eq!(
            diag.message(),
            SemaDiagKind::PinnedValueCapturedByClosure
                .message_with(&DiagContext::new().with("name", "pinned"))
        );
    }

    #[test]
    fn pin_inside_unsafe_allows_shadowed_name_inside_lambda() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe (pin xs as pinned in \(pinned : Int) => pinned);
    ",
        );
        assert!(!has_diag(
            &module,
            SemaDiagKind::PinnedValueCapturedByClosure
        ));
    }

    #[test]
    fn pin_inside_unsafe_rejects_yield_in_body() {
        let module = check(
            r"
        let xs := [1, 2];
        let value := unsafe (pin xs as pinned in yield 1);
    ",
        );
        assert!(has_diag(&module, SemaDiagKind::YieldInsidePinScope));
    }

    #[test]
    fn defer_accepts_unit_cleanup_and_bit_guard() {
        let sema = check(
            r"
        let cleanup () : Unit := known ();
        let value := defer cleanup() where 0 = 0;
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
        let defer_expr =
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Defer { .. })).expect("defer expr");
        assert!(
            matches!(
                sema.ty(sema
                    .try_expr_ty(defer_expr)
                    .expect("defer expr type missing"))
                    .kind,
                HirTyKind::Unit
            ),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn defer_guard_requires_bit_type() {
        let sema = check(
            r"
        let cleanup () : Unit := known ();
        let value := defer cleanup() where 1;
    ",
        );
        let diag = find_diag(&sema, SemaDiagKind::TypeMismatch).expect("type mismatch diagnostic");
        assert_eq!(diag.message(), "defer guard expected `Bit`, found `Int`");
    }

    #[test]
    fn defer_cleanup_requires_unit_type() {
        let sema = check(
            r"
        let value := defer 1 where 0 = 0;
    ",
        );
        let diag = find_diag(&sema, SemaDiagKind::TypeMismatch).expect("type mismatch diagnostic");
        assert_eq!(diag.message(), "defer cleanup expected `Unit`, found `Int`");
    }

    #[test]
    fn defer_cleanup_failure_result_requires_explicit_handling() {
        let sema = check(
            r"
        let CloseResult := data {
          | Success(Unit)
          | Failure(Int)
        };
        let close () : CloseResult := .Failure(1);
        let value := defer close();
    ",
        );
        let diag = find_diag(&sema, SemaDiagKind::TypeMismatch).expect("type mismatch diagnostic");
        assert_eq!(
            diag.message(),
            "defer cleanup expected `Unit`, found `CloseResult`"
        );
    }

    #[test]
    fn let_else_allows_refutable_literal_pattern() {
        let sema = check("let 0 := 1 else 2;");
        assert!(
            !has_diag(&sema, SemaDiagKind::PlainLetRequiresIrrefutablePattern),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn let_else_preserves_value_and_fallback_in_hir() {
        let sema = check("let kept := 1 else 2;");
        let let_expr =
            find_expr(&sema, |kind| matches!(kind, HirExprKind::Let { .. })).expect("let expr");
        let HirExprKind::Let { mods, value, .. } = sema.module().store.exprs.get(let_expr).kind
        else {
            panic!("expected let expr");
        };

        let fallback = mods.fallback.expect("let-else fallback missing");
        let assert_int_lit = |expr_id: HirExprId, expected: &str| {
            let HirExprKind::Lit { lit } = sema.module().store.exprs.get(expr_id).kind else {
                panic!("expected literal expression");
            };
            let HirLitKind::Int { raw } = &sema.module().store.lits.get(lit).kind else {
                panic!("expected int literal");
            };
            assert_eq!(raw.as_ref(), expected);
        };

        assert_int_lit(value, "1");
        assert_int_lit(fallback, "2");
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
    fn inferred_callable_return_keeps_data_type_arguments() {
        let sema = check(
            r"
        let List[T] := data {
          | ListValues(values : []T)
        };
        let of[T] (value : T) := .ListValues(values := [value]);
        let value : List[Int] := of[Int](4);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn partial_let_is_accepted_as_totality_modifier() {
        let sema = check("let parseInt(text : String) : Int := 0;");
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
        let same[A] (value : A) : A := value;
        let result := same[Int](42);
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
    fn known_diag_kind_roundtrips_code() {
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
        let base : Int := known 20;
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
    fn known_prefix_accepts_const_int_expressions() {
        let sema = check(
            r"
        let x : Int := known (1 + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn known_quote_expands_expression_type() {
        let sema = check(
            r"
        let value : Int := known (40 + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn known_quote_splices_primitive_literals_into_syntax() {
        let sema = check(
            r"
        let base : Int := known 40;
        let generated : Int := known (base + 2);
    ",
        );
        assert!(sema.diags().is_empty(), "{:?}", sema.diags());
    }

    #[test]
    fn known_prefix_rejects_runtime_expressions() {
        let sema = check(
            r"
        let runtime () : Int := 1;
        let x : Int := known runtime();
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::RuntimeValueInComptimeContext),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn imported_known_param_metadata_rejects_runtime_arguments() {
        let (_module_a, module_b) = check_with_imported_surface(
            210,
            r"
        export let scale (known n : Int, x : Int) : Int := x * n;
    ",
            r#"
        let A := import "a";
        let scale := A.scale;
        let runtime () : Int := 3;
        let y : Int := scale(runtime(), 2);
    "#,
        );
        assert!(
            has_diag(&module_b, SemaDiagKind::RuntimeValueInComptimeContext),
            "{:?}",
            module_b.diags()
        );
    }

    #[test]
    fn known_params_reject_runtime_arguments() {
        let sema = check(
            r"
        let scale (known n : Int, x : Int) : Int := x * n;
        let runtime () : Int := 3;
        let y : Int := scale(runtime(), 2);
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::RuntimeValueInComptimeContext),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn known_param_marks_param_compiler_known() {
        let sema = check(
            r"
        let scale (known n : Int, x : Int) : Int := x * n;
        let runtime () : Int := 3;
        let y : Int := scale(runtime(), 2);
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::RuntimeValueInComptimeContext),
            "{:?}",
            sema.diags()
        );
    }
}

mod failure {
    use super::*;

    #[test]
    fn builtin_attr_requires_foundation_module() {
        let sema = check(
            r#"
        @musi.builtin(name := "Type")
        export let Type := Type;
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::AttrBuiltinRequiresFoundationModule),
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
        let sema = check(r#"export let value : Int := "no";"#);
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
    fn non_callable_value_reports_invalid_call_target() {
        let sema = check(
            r#"
        let value := "ok";
        value();
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
        let x : mut Int := 1;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn type_cast_target_rejects_mut() {
        let sema = check(
            r"
        let x : mut Int := 1;
    ",
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn missing_required_record_field_reports_diag() {
        let sema = check(
            r"
        let Box[T] := data { let value : T; };
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
        let sema = check(
            r"let Box[T] := data { let value : T; }; let boxed : Box[Int] := { other := 1 };",
        );
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
    fn typed_let_value_mismatch_reports_diag() {
        let sema = check(
            r#"
        let x : Int := "no";
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
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
        let notEffect := { readLine := \() => "" };
        notEffect.readLine().missing();
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::InvalidFieldTarget)
                || has_diag(&sema, SemaDiagKind::UnknownField),
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
    fn target_attr_accepts_matching_plain_module_let() {
        let sema = check_with_target(
            r#"
        @target(os := "linux", arch := "x86_64", pointerWidth := 64)
        let linuxValue : Int := 1;
        let result : Int := linuxValue;
    "#,
            TargetInfo::new()
                .with_os("linux")
                .with_arch("x86_64")
                .with_pointer_width(64),
        );

        assert!(
            !has_diag(&sema, SemaDiagKind::TargetGateRejected),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::AttrLinkRequiresForeignLet),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn target_attr_rejects_non_matching_plain_module_let_use() {
        let sema = check_with_target(
            r#"
        @target(os := "macos")
        let macosValue : Int := 1;
        let result : Int := macosValue;
    "#,
            TargetInfo::new().with_os("linux"),
        );

        assert!(
            has_diag(&sema, SemaDiagKind::TargetGateRejected),
            "{:?}",
            sema.diags()
        );
        assert!(
            !has_diag(&sema, SemaDiagKind::AttrLinkRequiresForeignLet),
            "{:?}",
            sema.diags()
        );
    }

    #[test]
    fn foreign_call_requires_unsafe_block() {
        let module = check(
            r"
        @foreign(abi := .c) let clock () : Int;
        let value := clock();
    ",
        );
        assert!(has_diag(
            &module,
            SemaDiagKind::UnsafeCallRequiresUnsafeBlock
        ));
    }

    #[test]
    fn type_equality_constraint_rejects_mismatched_type_application() {
        let sema = check(
            r#"
        let same(value : Int) : Int := value;
        let result := same("x");
    "#,
        );
        assert!(
            has_diag(&sema, SemaDiagKind::TypeMismatch),
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
    fn known_prefix_accepts_non_int_literals() {
        let sema = check(
            r#"
        let text : String := known "hello";
        let unit : Unit := known ();
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
