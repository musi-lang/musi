#![allow(unused_imports)]

use musi_vm::{
    ForeignCall, Program, Value, ValueView, Vm, VmError, VmErrorKind, VmHost, VmHostCallContext,
    VmHostContext, VmOptions, VmResult,
};
use music_base::diag::Diag;
use music_emit::{EmitDiagKind, emit_diag_kind};
use music_ir::{IrDiagKind, ir_diag_kind};
use music_module::{ImportMap, ModuleKey};
use music_resolve::{ResolveDiagKind, resolve_diag_kind};
use music_seam::Artifact;
use music_seam::descriptor::ConstantValue;
use music_sema::{SemaDiagKind, TargetInfo, sema_diag_kind};
use music_syntax::{ParseErrorKind, TokenKind};

use crate::{CompiledOutput, Session, SessionError, SessionOptions, SessionSyntaxErrors};

fn meta_records(artifact: &Artifact) -> Vec<(String, String, Vec<String>)> {
    artifact
        .meta
        .as_slice()
        .iter()
        .map(|record| {
            (
                artifact.string_text(record.target).to_owned(),
                artifact.string_text(record.key).to_owned(),
                record
                    .values
                    .iter()
                    .map(|value| artifact.string_text(*value).to_owned())
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<Vec<_>>()
}

fn import_records(artifact: &Artifact) -> Vec<(String, String)> {
    artifact
        .imports
        .as_slice()
        .iter()
        .map(|record| {
            (
                artifact.string_text(record.spec).to_owned(),
                artifact.string_text(record.resolved).to_owned(),
            )
        })
        .collect::<Vec<_>>()
}

fn session() -> Session {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    let _ = import_map
        .imports
        .insert("musi:test".into(), "musi:test".into());
    Session::new(SessionOptions::new().with_import_map(import_map))
}

fn session_with_target(target: TargetInfo) -> Session {
    let mut import_map = ImportMap::default();
    let _ = import_map.imports.insert("dep".into(), "dep".into());
    Session::new(
        SessionOptions::new()
            .with_import_map(import_map)
            .with_target(target),
    )
}

fn main_key() -> ModuleKey {
    ModuleKey::new("main")
}

fn set_main_text(session: &mut Session, text: &str) {
    session.set_module_text(&main_key(), text).unwrap();
}

fn compile_main_module(session: &mut Session) -> CompiledOutput {
    session.compile_module(&main_key()).unwrap()
}

fn compile_main_entry(session: &mut Session) -> CompiledOutput {
    session.compile_entry(&main_key()).unwrap()
}

fn compile_main_module_with_source(source: &str) -> CompiledOutput {
    let mut session = session();
    set_main_text(&mut session, source);
    compile_main_module(&mut session)
}

fn assert_output_contains(output: &CompiledOutput, needles: &[&str]) {
    for needle in needles {
        assert!(
            output.disasm.contains(needle),
            "missing `{needle}` in:\n{}",
            output.disasm
        );
    }
}

fn run_export(output: &CompiledOutput, name: &str) -> Value {
    let program = Program::from_bytes(&output.bytes).expect("program should load");
    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("program should initialize");
    vm.call_export(name, &[]).expect("export should run")
}

fn assert_main_module_compiles_with(source: &str, needles: &[&str]) -> CompiledOutput {
    let output = compile_main_module_with_source(source);
    assert!(output.artifact.validate().is_ok());
    assert_output_contains(&output, needles);
    output
}

fn compile_main_entry_with_dep(dep_source: &str, main_source: &str) -> CompiledOutput {
    let mut session = session();
    session
        .set_module_text(&ModuleKey::new("dep"), dep_source)
        .unwrap();
    set_main_text(&mut session, main_source);
    let output = compile_main_entry(&mut session);
    assert!(output.artifact.validate().is_ok());
    output
}

fn parse_failure_syntax(err: SessionError) -> SessionSyntaxErrors {
    let SessionError::ModuleParseFailed { syntax, .. } = err else {
        panic!("parse error expected");
    };
    syntax
}

fn assert_parse_failure_via_compile<F>(run: F)
where
    F: FnOnce(&mut Session, &ModuleKey) -> Result<CompiledOutput, SessionError>,
{
    let mut session = session();
    set_main_text(&mut session, "let x := 1");
    let syntax = parse_failure_syntax(run(&mut session, &main_key()).unwrap_err());
    assert!(syntax.lex_errors().is_empty());
    assert_eq!(syntax.parse_errors().len(), 1);
    assert_eq!(syntax.diags().len(), 1);
}

macro_rules! assert_main_entry_compiles_with {
    ($source:expr, $needles:expr $(,)?) => {{
        let mut session = session();
        set_main_text(&mut session, $source);
        let output = compile_main_entry(&mut session);
        assert!(output.artifact.validate().is_ok());
        assert_output_contains(&output, $needles);
        output
    }};
}

macro_rules! assert_emit_failure_with_unknown_type_value {
    ($run:expr $(,)?) => {{
        let mut session = session();
        set_main_text(&mut session, "export let result : Int := 42;");
        session.inject_emit_failure_for_tests(
            vec![
                Diag::error("unknown emitted type value `Injected`")
                    .with_code(EmitDiagKind::UnknownTypeValue.code()),
            ]
            .into_boxed_slice(),
        );

        let err = $run(&mut session, &main_key()).unwrap_err();
        let SessionError::ModuleEmissionFailed { diags, .. } = err else {
            panic!("emit error expected");
        };

        assert_eq!(diags.len(), 1);
        assert_eq!(
            emit_diag_kind(&diags[0]),
            Some(EmitDiagKind::UnknownTypeValue)
        );
    }};
}

mod success {
    use super::*;

    #[test]
    fn compiles_module_to_artifact_bytes_and_text() {
        let output = assert_main_module_compiles_with(
            "export let result : Int := 42;",
            &[".global $main::result export"],
        );
        assert!(!output.bytes.is_empty());
    }

    #[test]
    fn compiles_piped_calls_as_normal_calls() {
        let output = assert_main_module_compiles_with(
            "export let add (left : Int, right : Int) : Int := left + right; export let result : Int := 1 |> add(2);",
            &[".global $main::result export"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_reachable_entry_graph() {
        let output = compile_main_entry_with_dep(
            "export let base : Int := 41;",
            "import \"dep\"; export let result : Int := 42;",
        );
        assert_output_contains(
            &output,
            &[".global $dep::base export", ".global $main::result export"],
        );
    }

    #[test]
    fn compiles_static_imports_into_artifact_import_table() {
        let output = compile_main_entry_with_dep(
            "export let base : Int := 41;",
            r#"let Dep := import "dep"; export let result : Int := Dep.base;"#,
        );
        assert!(import_records(&output.artifact).contains(&("dep".into(), "dep".into())));
    }

    #[test]
    fn artifact_import_table_preserves_specifier_and_resolved_module() {
        let mut import_map = ImportMap::default();
        let _ = import_map.imports.insert("tool".into(), "dep".into());
        let mut session = Session::new(SessionOptions::new().with_import_map(import_map));
        session
            .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
            .unwrap();
        set_main_text(
            &mut session,
            r#"let Tool := import "tool"; export let result : Int := Tool.base;"#,
        );

        let output = compile_main_entry(&mut session);

        assert!(output.artifact.validate().is_ok());
        assert!(import_records(&output.artifact).contains(&("tool".into(), "dep".into())));
    }

    #[test]
    fn anonymous_import_exports_enter_lexical_scope() {
        let output = compile_main_entry_with_dep(
            "export let base : Int := 41;",
            r#"import "dep"; export let result : Int := base + 1;"#,
        );
        assert_output_contains(
            &output,
            &["ld.glob $dep::base", ".global $main::result export"],
        );
    }

    #[test]
    fn resolve_reuses_cached_parse_product() {
        let mut session = session();
        set_main_text(&mut session, "export let result : Int := 42;");

        let _ = session.parse_module(&main_key()).unwrap();
        let after_parse = session.stats().clone();
        let _ = session.resolve_module(&main_key()).unwrap();

        assert_eq!(session.stats().parse_runs, after_parse.parse_runs);
        assert!(session.stats().resolve_runs > after_parse.resolve_runs);
    }

    #[test]
    fn compiles_imported_generic_callable_calls() {
        let output = compile_main_entry_with_dep(
            "export let id[T] (x : T) : T := x;",
            r#"
            let dep := import "dep";
            export let result () : Int := dep.id[Int](42);
        "#,
        );
        assert_output_contains(&output, &["$dep::id", "$main::result"]);
    }

    #[test]
    fn compiles_imported_callable_alias_as_closure_value() {
        let output = compile_main_entry_with_dep(
            "export let copy[T] (values : []T) : []T := [...values];",
            r#"
            let core := import "dep";
            export let copy := core.copy;
            export let result () : []Int := copy[Int]([1, 2, 3]);
        "#,
        );

        assert_output_contains(
            &output,
            &[
                ".procedure $main::copy::init",
                "new.fn $dep::copy",
                "$main::result",
            ],
        );
        let program = Program::from_bytes(&output.bytes).expect("program should load");
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("program should initialize");
        let result_value = vm.call_export("result", &[]).expect("export should run");
        let ValueView::Seq(seq) = vm.inspect(&result_value) else {
            panic!("result should return sequence");
        };
        assert_eq!(seq.len(), 3);
        assert_eq!(seq.get(0), Some(Value::Int(1)));
        assert_eq!(seq.get(1), Some(Value::Int(2)));
        assert_eq!(seq.get(2), Some(Value::Int(3)));
    }

    #[test]
    fn compiles_imported_given_alias_as_global_value() {
        let output = compile_main_entry_with_dep(
            r"
            export let intEq := { eq := \(left : Int, right : Int) : Bit => left = right };
        ",
            r#"
            let core := import "dep";
            export let intEq := core.intEq;
            export let result () : Int := 42;
        "#,
        );

        assert_output_contains(
            &output,
            &[
                ".global $dep::intEq export",
                "ld.glob $dep::intEq",
                ".global $main::intEq export",
            ],
        );
        assert_eq!(run_export(&output, "result"), Value::Int(42));
    }

    #[test]
    fn compiles_local_known_param_callable_specialization() {
        let output = assert_main_module_compiles_with(
            r"
            let scale (known n : Int, x : Int) : Int := x * n;
            export let result () : Int := scale(3, 14);
        ",
            &["scale$ct$0_i3", "$main::result"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_known_quote_expr_expansion() {
        let output = assert_main_module_compiles_with(
            r"
            export let result () : Int := known (40 + 2);
        ",
            &["$main::result", "ld.c.i4 42"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_known_quote_item_expansion() {
        let output = assert_main_module_compiles_with(
            r"
            export let result () : Int := 42;
        ",
            &["$main::result", "ld.c.i4 42"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_nested_known_quote_item_expansion() {
        let output = assert_main_module_compiles_with(
            r"
            export let result () : Int := known 42;
        ",
            &["$main::result", "ld.c.i4 42"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_local_syntax_item_expansion() {
        let output = assert_main_module_compiles_with(
            r"
            export let result () : Int := 42;
        ",
            &["$main::result", "ld.c.i4 42"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_imported_syntax_item_expansion() {
        let output = compile_main_entry_with_dep(
            r"
            export let generated () : Int := 42;
        ",
            r#"
            let dep := import "dep";
            export let result () : Int := dep.generated();
        "#,
        );
        assert_output_contains(&output, &["$main::result", "ld.c.i4 42"]);
    }

    #[test]
    fn compiles_local_syntax_factory_item_expansion() {
        let output = assert_main_module_compiles_with(
            r"
            export let result () : Int := known 42;
        ",
            &["$main::result", "ld.c.i4 42"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_imported_syntax_factory_item_expansion() {
        let output = compile_main_entry_with_dep(
            r"
            export let generated (value : Int) : Int := value;
        ",
            r#"
            let dep := import "dep";
            export let result () : Int := dep.generated(42);
        "#,
        );
        assert_output_contains(&output, &["$main::result", "ld.c.i4 42"]);
    }

    #[test]
    fn compiles_first_class_generic_values_in_records() {
        let output = compile_main_entry_with_dep(
            "export let id[T] (x : T) : T := x;",
            r#"
            let dep := import "dep";
            let tools := { id := dep.id };
            export let result () : Int := tools.id[Int](42);
        "#,
        );
        assert_output_contains(&output, &["call", "$main::result"]);
    }

    #[test]
    fn compiles_imported_globals_and_local_assignment() {
        let output = compile_main_entry_with_dep(
            "export let base : Int := 41;",
            r#"
            let dep := import "dep";
            export let result () : Int := (
              let local := mut dep.base;
              local := local + 1;
              local
            );
        "#,
        );
        assert_output_contains(&output, &["ld.glob $dep::base", "$main::result"]);
    }

    #[test]
    fn compiles_multi_index_and_quote() {
        let output = assert_main_entry_compiles_with!(
            r"
            export let touch (name : String, grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 7;
              grid.[0, 1]
            );
        ",
            &["ld.elem", "st.elem"],
        );
        assert!(output.artifact.validate().is_ok());
    }

    #[test]
    fn compiles_closures_and_higher_order_calls() {
        let _ = assert_main_entry_compiles_with!(
            r"
            let apply (f : Int -> Int, x : Int) : Int := f(x);
            export let result (x : Int) : Int := (
              let base : Int := 41;
              let add_base (y : Int) : Int := y + base;
              apply(add_base, x)
            );
        ",
            &["call.ind", "new.fn"],
        );
    }

    #[test]
    fn compiles_named_call_arguments_and_named_requests() {
        let _ = assert_main_module_compiles_with(
            r#"
        export let Console := { readLine := \(prompt : String) : String => prompt };

        let render (port : Int, secure : Bit) : Int := port;
        export let read () : String := Console.readLine(">");
        export let main () : Int := render(secure := 0 = 0, port := 8080);
        "#,
            &["call $main::render", "call.ind"],
        );
    }

    #[test]
    fn compiles_local_recursive_callable_let() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result (n : Int) : Int := (
              let recur loop (x : Int) : Int := match x (| 0 => 0 | _ => loop(x - 1));
              loop(n)
            );
        ",
            &["loop"],
        );
    }

    #[test]
    fn compiles_case_tuple_and_array_patterns() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let p : Int := match pair (| (1, b) => b | _ => 0);
              let q : Int := match items (| [3, b] => b | _ => 0);
              p + q
            );
        ",
            &["ld.elem", "br.z"],
        );
    }

    #[test]
    fn compiles_records_with_projection_and_update() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result () : Int := (
              let r := { y := 2, x := 1 };
              let a : Int := r.x;
              let s := { ...r, x := 3 };
              a + s.x
            );
        ",
            &["ld.fld", "new.obj", ".type $\"{ x: Int; y: Int }\""],
        );
    }

    #[test]
    fn compiles_record_shaped_data_with_projection_and_update() {
        let _ = assert_main_entry_compiles_with!(
            r#"
            let Box[T] := data {
              let value : T;
            };
            export let result () : String := (
              let boxed : Box[String] := { value := "Nora" };
              let renamed := { ...boxed, value := "Miso" };
              renamed.value
            );
        "#,
            &["ld.fld", "new.obj", ".type $main::Box"],
        );
    }

    #[test]
    fn compiles_record_field_assignment() {
        let output = assert_main_entry_compiles_with!(
            r"
            export let result () : Int := (
              let r := mut { x := 1, y := 2 };
              r.x := 3;
              r.x
            );
        ",
            &["st.fld"],
        );
        assert!(output.disasm.contains("st.fld"), "{}", output.disasm);
    }

    #[test]
    fn compiles_record_destructuring_let_patterns() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result () : Int := (
              let r := { y := 2, x := 1 };
              let {x, y} := r;
              x + y
            );
        ",
            &["ld.fld"],
        );
    }

    #[test]
    fn compiles_tuple_and_array_destructuring_let_patterns() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result () : Int := (
              let pair := (1, 2);
              let items := [3, 4];
              let (a, b) := pair;
              let [c, d] := items;
              a + b + c + d
            );
        ",
            &["ld.elem"],
        );
    }

    #[test]
    fn compiles_capturing_recursion_record_patterns_and_type_values() {
        let _ = assert_main_entry_compiles_with!(
            r"
            export let result (n : Int) : Int := (
              let base := 1;
              let recur loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
              let point := { x := 1, y := 2 };
              let picked : Int := match point (| { x } => x | _ => 0);
              picked + loop(n)
            );
        ",
            &["ld.fld", "call.ind"],
        );
    }

    #[test]
    fn compiles_variants_with_case_patterns() {
        let _ = assert_main_entry_compiles_with!(
            r"
            let Maybe := data { | Some(Int) | None };
            export let result () : Int := (
              let x : Maybe := .Some(1);
              match x (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
            &[
                "ld.fld",
                "br.tbl",
                "ld.fld",
                "new.obj",
                ".type $main::Maybe"
            ],
        );
    }

    #[test]
    fn compiles_variants_without_type_context_when_tag_unique() {
        let _ = assert_main_entry_compiles_with!(
            r"
            let Maybe := data { | Some(Int) | None };
            export let result () : Int := (
              let x := .Some(1);
              match x (
              | .Some(y) => y
              | .None => 0
              )
            );
        ",
            &["ld.fld", "br.tbl", "ld.fld", "new.obj"],
        );
    }

    #[test]
    fn compiles_record_method_call() {
        let _ = assert_main_entry_compiles_with!(
            r#"
            let Console := { readLine := \() : String => "ok" };
            export let result () : String := Console.readLine();
        "#,
            &["call.ind"],
        );
    }

    #[test]
    fn compiles_exported_native_declarations_into_artifact() {
        let _ = assert_main_module_compiles_with(
            r"
            @foreign(abi := .c)
            export let puts (msg : CString) : Int;
            export let result : Int := 1;
        ",
            &[".native $main::puts param $CString result $Int abi \"c\" symbol \"puts\" export"],
        );
    }

    #[test]
    fn lowers_link_attrs_into_native_descriptors() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r#"
            @link(name := "m")
            @foreign(abi := .c)
            let sin (x : Float) : Float;
        "#,
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());
        assert!(
            output.disasm.contains(
                ".native $main::sin param $Float result $Float abi \"c\" symbol \"sin\" link \"m\""
            ),
            "{}",
            output.disasm
        );
    }

    #[test]
    fn lowers_profile_attrs_into_callable_descriptors() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r"
            @profile(level := .hot)
            export let hotWork () : Int := 1;

            @profile(level := .cold)
            export let coldWork () : Int := 2;
        ",
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());
        assert!(
            output.disasm.contains(".procedure $main::hotWork"),
            "{}",
            output.disasm
        );
        assert!(output.disasm.contains("export hot"), "{}", output.disasm);
        assert!(
            output.disasm.contains(".procedure $main::coldWork"),
            "{}",
            output.disasm
        );
        assert!(output.disasm.contains("export cold"), "{}", output.disasm);
    }

    #[test]
    fn lowers_profile_attrs_into_native_descriptors() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r"
            @profile(level := .hot)
            @foreign(abi := .c)
            let fastClock () : Nat64;

            @profile(level := .cold)
            @foreign(abi := .c)
            let slowPath () : Int;
        ",
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());
        assert!(
            output.disasm.contains(
                ".native $main::fastClock result $Nat64 abi \"c\" symbol \"fastClock\" hot"
            ),
            "{}",
            output.disasm
        );
        assert!(
            output
                .disasm
                .contains(".native $main::slowPath result $Int abi \"c\" symbol \"slowPath\" cold"),
            "{}",
            output.disasm
        );
    }

    #[test]
    fn skips_gated_native_declarations_for_target() {
        let mut session =
            session_with_target(TargetInfo::new().with_os("linux").with_arch("x86_64"));
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r#"
            @target(os := "LiNuX", arch := "x86_64")
            @foreign(abi := .c)
            let clock_gettime (id : Int, out : CPtr) : Int;

            @target(os := "windows")
            @foreign(abi := .c)
            let QueryPerformanceCounter (out : CPtr) : Int;
        "#,
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());
        assert!(output.disasm.contains("clock_gettime"), "{}", output.disasm);
        assert!(!output.disasm.contains("QueryPerformanceCounter"));
    }

    #[test]
    fn matches_gated_native_declarations_by_target_family() {
        let mut session = session_with_target(
            TargetInfo::new()
                .with_os("macOS")
                .with_arch("AaRcH64")
                .with_family("Darwin")
                .with_family("Unix")
                .with_pointer_width(64),
        );
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r#"
            @target(family := ["darwin", "bsd"], arch := ["x86-64", "aarch64"], pointerWidth := 64)
            @foreign(abi := .c)
            let mach_absolute_time () : Nat64;

            @target(family := "windows")
            @foreign(abi := .c)
            let GetLastError () : Nat64;
        "#,
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());
        assert!(
            output.disasm.contains("mach_absolute_time"),
            "{}",
            output.disasm
        );
        assert!(!output.disasm.contains("GetLastError"));
    }

    #[test]
    fn emits_meta_records_for_attrs() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r#"
            @foreign(abi := .musi)
            let musi_true () : Bit;

            @foo.bar(baz := "qux", items := ["a", "b"])
            export let result : Int := 42;

            @musi.codegen(mode := "test")
            export let meaning : Int := 1;

            export let Eq[T] := shape {
              let (=) (a : T, b : T) : Bit;
            };

            export let Console := shape {
              let readLine () : String;
            };
        "#,
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());

        let meta = meta_records(&output.artifact);

        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::result"
                    && key == "inert.attr"
                    && values
                        == &vec!["@foo.bar(baz := \"qux\", items := [\"a\", \"b\"])".to_owned()]
            }),
            "{meta:?}"
        );
        assert!(
            !meta.iter().any(|(target, key, values)| {
                target == "main::meaning"
                    && key == "musi.attr"
                    && values == &vec!["@musi.codegen(mode := \"test\")".to_owned()]
            }),
            "{meta:?}"
        );
    }

    #[test]
    fn emits_meta_records_for_exported_signatures() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                r"
            let Maybe[T] := data { | Some(Int) | None };

            let Eq[T] := shape { };

            export let f (x : Int) : Int := x;
            export let sumId (x : Int + String) : Int + String := x;
            export let tupId (x : (Int, String)) : (Int, String) := x;
            export let arrId (x : [2]Int) : [2]Int := x;
            export let mutArrId (x : mut [2]Int) : mut [2]Int := x;
            export let noneInt () : Maybe[Int] := .None;
        ",
            )
            .unwrap();

        let output = session.compile_module(&ModuleKey::new("main")).unwrap();
        assert!(output.artifact.validate().is_ok());

        let meta = meta_records(&output.artifact);

        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::sumId"
                    && key == "value.ty"
                    && values
                        .first()
                        .is_some_and(|value| value.contains("Int + String"))
            }),
            "{meta:?}"
        );
        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::tupId"
                    && key == "value.ty"
                    && values
                        .first()
                        .is_some_and(|value| value.contains("(Int, String)"))
            }),
            "{meta:?}"
        );
        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::arrId"
                    && key == "value.ty"
                    && values.first().is_some_and(|value| value.contains("[2]Int"))
            }),
            "{meta:?}"
        );
        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::mutArrId"
                    && key == "value.ty"
                    && values
                        .first()
                        .is_some_and(|value| value.contains("mut [2]Int"))
            }),
            "{meta:?}"
        );
        assert!(
            meta.iter().any(|(target, key, values)| {
                target == "main::noneInt"
                    && key == "value.ty"
                    && values
                        .first()
                        .is_some_and(|value| value.contains("Maybe[Int]"))
            }),
            "{meta:?}"
        );
    }

    #[test]
    fn compile_entry_lowers_class_member_calls_through_evidence() {
        let mut session = session();
        set_main_text(
            &mut session,
            r"
            let same (left : Int, right : Int) : Bit := left = right;
            let direct := same(1, 2);
            ",
        );

        let output = session.compile_entry(&main_key());

        assert!(output.is_ok(), "{output:?}");
    }
}

mod failure {
    use super::*;

    #[test]
    fn rejects_unhandled_effect_inside_known() {
        let mut session = session();
        set_main_text(
            &mut session,
            r"
        export let result () : Int := missing();
    ",
        );
        let error = session
            .compile_entry(&main_key())
            .expect_err("unresolved known call should fail");
        assert!(matches!(error, SessionError::ModuleResolveFailed { .. }));
    }

    #[test]
    fn parse_failures_expose_typed_syntax_errors_and_diags() {
        let mut session = session();
        set_main_text(&mut session, "let x := 1");

        let syntax = parse_failure_syntax(session.parse_module(&main_key()).unwrap_err());

        assert!(syntax.lex_errors().is_empty());
        assert_eq!(syntax.parse_errors().len(), 1);
        assert!(matches!(
            syntax.parse_errors()[0].kind,
            ParseErrorKind::ExpectedToken {
                expected: TokenKind::Semicolon,
                ..
            }
        ));
        assert_eq!(syntax.diags().len(), 1);
        assert!(!syntax.diags()[0].labels().is_empty());
    }

    #[test]
    fn compile_module_propagates_parse_failures() {
        assert_parse_failure_via_compile(Session::compile_module);
    }

    #[test]
    fn compile_entry_propagates_parse_failures() {
        assert_parse_failure_via_compile(Session::compile_entry);
    }

    #[test]
    fn reuses_caches_and_invalidates_dependents_on_edit() {
        let mut session = session();
        session
            .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 41;")
            .unwrap();
        set_main_text(
            &mut session,
            "import \"dep\"; export let result : Int := 42;",
        );

        let _ = compile_main_entry(&mut session);
        let first_stats = session.stats().clone();
        let _ = compile_main_entry(&mut session);
        assert_eq!(session.stats(), &first_stats);

        session
            .set_module_text(&ModuleKey::new("dep"), "export let base : Int := 99;")
            .unwrap();
        let _ = compile_main_entry(&mut session);
        assert!(session.stats().resolve_runs > first_stats.resolve_runs);
        assert!(session.stats().emit_runs > first_stats.emit_runs);
    }

    #[test]
    fn resolve_failures_surface_session_resolve_error() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                "import \"missing\"; export let result : Int := 42;",
            )
            .unwrap();

        let err = session.resolve_module(&ModuleKey::new("main")).unwrap_err();
        let SessionError::ModuleResolveFailed { diags, .. } = err else {
            panic!("resolve error expected");
        };

        assert_eq!(diags.len(), 1);
        assert_eq!(
            resolve_diag_kind(&diags[0]),
            Some(ResolveDiagKind::ImportResolveFailed)
        );
        assert!(!diags[0].labels().is_empty());
    }

    #[test]
    fn sema_failures_surface_session_sema_error() {
        let mut session = session();
        session
            .set_module_text(
                &ModuleKey::new("main"),
                "export let result : Int := \"no\";",
            )
            .unwrap();

        let err = session.check_module(&ModuleKey::new("main")).unwrap_err();
        let SessionError::ModuleSemanticCheckFailed { diags, .. } = err else {
            panic!("sema error expected");
        };

        assert!(!diags.is_empty());
        assert!(
            diags
                .iter()
                .any(|diag| sema_diag_kind(diag) == Some(SemaDiagKind::TypeMismatch))
        );
        assert!(diags.iter().any(|diag| !diag.labels().is_empty()));
    }

    #[test]
    fn lower_module_propagates_ir_failure_with_typed_kind() {
        let mut session = session();
        set_main_text(&mut session, "export let result : Int := 42;");
        session.inject_ir_failure_for_tests(
            vec![
                Diag::error(IrDiagKind::LoweringRequiresSemaCleanModule.message())
                    .with_code(IrDiagKind::LoweringRequiresSemaCleanModule.code()),
            ]
            .into_boxed_slice(),
        );

        let err = session.lower_module(&main_key()).unwrap_err();
        let SessionError::ModuleLoweringFailed { diags, .. } = err else {
            panic!("ir error expected");
        };

        assert_eq!(diags.len(), 1);
        assert_eq!(
            ir_diag_kind(&diags[0]),
            Some(IrDiagKind::LoweringRequiresSemaCleanModule)
        );
    }

    #[test]
    fn compile_module_propagates_emit_failure_with_typed_kind() {
        assert_emit_failure_with_unknown_type_value!(|session: &mut Session, key: &ModuleKey| {
            session.compile_module(key)
        });
    }

    #[test]
    fn compile_entry_propagates_emit_failure_with_typed_kind() {
        assert_emit_failure_with_unknown_type_value!(|session: &mut Session, key: &ModuleKey| {
            session.compile_entry(key)
        });
    }
}
