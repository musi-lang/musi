use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use musi_foundation::register_modules;
use music_module::ModuleKey;
use music_seam::Artifact;
use music_seam::descriptor::{DataDescriptor, DataVariantDescriptor, TypeDescriptor};
use music_seam::{StringId, TypeId};
use music_session::{Session, SessionOptions};
use music_term::{TypeTerm, TypeTermKind};

use super::{
    EffectCall, ForeignCall, Program, ProgramTypeAbiKind, RejectingLoader, Value, ValueView, Vm,
    VmError, VmErrorKind, VmHost, VmLoader, VmOptions, VmResult, render_value_view,
};

#[derive(Default)]
struct TestLoader {
    modules: HashMap<Box<str>, Program>,
}

impl VmLoader for TestLoader {
    fn load_program(&mut self, spec: &str) -> VmResult<Program> {
        self.modules
            .get(spec)
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::ModuleLoadRejected { spec: spec.into() }))
    }
}

#[derive(Default)]
struct TestHost;

impl VmHost for TestHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "test host rejected effect call".into(),
        }))
    }
}

#[derive(Default)]
struct SignatureHost {
    log: Rc<RefCell<SignatureLog>>,
}

type ForeignSignatureRecord = (Box<str>, Box<[Box<str>]>, Box<str>);
type EffectSignatureRecord = (Box<str>, Box<str>, Box<[Box<str>]>, Box<str>);

#[derive(Default)]
struct SignatureLog {
    foreign_calls: Vec<ForeignSignatureRecord>,
    effect_calls: Vec<EffectSignatureRecord>,
}

impl VmHost for SignatureHost {
    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        self.log.borrow_mut().foreign_calls.push((
            foreign.name().into(),
            foreign
                .param_tys()
                .iter()
                .map(|ty| foreign.type_name(*ty).into())
                .collect::<Vec<Box<str>>>()
                .into_boxed_slice(),
            foreign.result_ty_name().into(),
        ));
        Ok(Value::Int(7))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        self.log.borrow_mut().effect_calls.push((
            effect.effect_name().into(),
            effect.op_name().into(),
            effect
                .param_tys()
                .iter()
                .map(|ty| effect.type_name(*ty).into())
                .collect::<Vec<Box<str>>>()
                .into_boxed_slice(),
            effect.result_ty_name().into(),
        ));
        Ok(Value::Int(42))
    }
}

fn session() -> Session {
    Session::new(SessionOptions::default())
}

fn compile_program(modules: &[(&str, &str)], entry: &str) -> Program {
    let mut session = session();
    register_modules(&mut session).expect("foundation modules should install");
    for &(name, source) in modules {
        session
            .set_module_text(&ModuleKey::new(name), source)
            .expect("module text should install");
    }
    let output = session
        .compile_entry(&ModuleKey::new(entry))
        .expect("session compile should succeed");
    Program::from_bytes(&output.bytes).expect("program load should succeed")
}

#[test]
fn loads_program_and_lists_exports() {
    let program = compile_program(
        &[(
            "main",
            "export let answer () : Int := 42; export let base : Int := 1;",
        )],
        "main",
    );

    assert_eq!(program.export_count(), 2);
    assert!(
        program
            .exports()
            .iter()
            .any(|export| export.name.as_ref() == "answer")
    );
}

#[test]
fn initializes_and_calls_export() {
    let program = compile_program(&[("main", "export let answer () : Int := 40 + 2;")], "main");

    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let value = vm
        .call_export("answer", &[])
        .expect("export call should succeed");

    assert_eq!(value, Value::Int(42));
}

#[test]
fn executes_closure_and_recursive_callable() {
    let program = compile_program(
        &[(
            "main",
            r"
            let apply (f : Int -> Int, x : Int) : Int := f(x);
            export let answer (n : Int) : Int := (
              let base : Int := 1;
              let rec loop (x : Int) : Int := match x (| 0 => base | _ => loop(x - 1));
              let add_base (y : Int) : Int := y + 41;
              apply(add_base, loop(n))
            );
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let value = vm
        .call_export("answer", &[Value::Int(3)])
        .expect("recursive closure call should succeed");

    assert_eq!(value, Value::Int(42));
}

#[test]
fn executes_record_projection_and_update() {
    let program = compile_program(
        &[(
            "main",
            r"
            export let answer () : Int := (
              let point := { x := 1, y := 2 };
              let updated := { ...point, x := 40 };
              updated.x + point.y
            );
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let value = vm
        .call_export("answer", &[])
        .expect("record call should succeed");

    assert_eq!(value, Value::Int(42));
}

#[test]
fn executes_multi_index_get_and_set() {
    let program = compile_program(
        &[(
            "main",
            r"
            export let touch (grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 42;
              grid.[0, 1]
            );
        ",
        )],
        "main",
    );

    let outer_ty = program
        .artifact()
        .types
        .iter()
        .next()
        .map(|(id, _)| id)
        .expect("type id");
    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let inner = Value::sequence(outer_ty, vec![Value::Int(1), Value::Int(2)]);
    let grid = Value::sequence(outer_ty, vec![inner.clone(), inner]);
    let value = vm
        .call_export("touch", &[grid])
        .expect("multi-index call should succeed");

    assert_eq!(value, Value::Int(42));
}

#[test]
fn loads_dynamic_module_through_host() {
    let dep = compile_program(
        &[(
            "dep",
            "export let answer () : Int := 42; export let base : Int := 41;",
        )],
        "dep",
    );
    let main = compile_program(&[("main", "export let root () : Int := 0;")], "main");

    let mut loader = TestLoader::default();
    let _ = loader.modules.insert("dep".into(), dep);
    let mut vm = Vm::new(main, loader, TestHost, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let module = vm
        .load_module("dep")
        .expect("dynamic import should succeed");

    assert_eq!(
        vm.lookup_module_export(&module, "base")
            .expect("module global export should resolve"),
        Value::Int(41)
    );
    assert_eq!(
        vm.call_module_export(&module, "answer", &[])
            .expect("module callable export should succeed"),
        Value::Int(42)
    );
    assert_eq!(
        vm.load_module("dep")
            .expect("cached dynamic load should succeed"),
        module
    );
}

#[test]
fn rejects_opaque_exports_from_root_and_dynamic_modules() {
    let dep = compile_program(
        &[(
            "dep",
            "export opaque let Hidden := data { | Hidden : Int }; export let answer () : Int := 42;",
        )],
        "dep",
    );
    let main = compile_program(
        &[(
            "main",
            "export opaque let Secret := data { | Secret : Int }; export let root () : Int := 0;",
        )],
        "main",
    );

    let mut loader = TestLoader::default();
    let _ = loader.modules.insert("dep".into(), dep);
    let mut vm = Vm::new(main, loader, TestHost, VmOptions);
    vm.initialize().expect("vm init should succeed");

    let err = vm.lookup_export("Secret").unwrap_err();
    assert!(matches!(
        err.kind(),
        VmErrorKind::OpaqueExport { module, export }
            if module.as_ref() == "<root>" && export.as_ref() == "Secret"
    ));

    let module = vm
        .load_module("dep")
        .expect("dynamic import should succeed");
    let err = vm.lookup_module_export(&module, "Hidden").unwrap_err();
    assert!(matches!(
        err.kind(),
        VmErrorKind::OpaqueExport { module, export }
            if module.as_ref() == "dep" && export.as_ref() == "Hidden"
    ));
}

#[test]
fn detects_module_init_cycle() {
    let main = compile_program(&[("main", "export let root () : Int := 0;")], "main");
    let dep = compile_program(
        &[(
            "dep",
            r#"
            let self_name : String := "dep";
            export let answer : Int := (
              let loaded := import self_name;
              1
            );
        "#,
        )],
        "dep",
    );

    let mut loader = TestLoader::default();
    let _ = loader.modules.insert("dep".into(), dep);
    let mut vm = Vm::new(main, loader, TestHost, VmOptions);
    vm.initialize().unwrap();
    let err = vm.load_module("dep").unwrap_err();

    assert!(matches!(err.kind(), VmErrorKind::ModuleInitCycle { .. }));
}

#[test]
fn handles_effect_value_clause_and_resume() {
    let program = compile_program(
        &[(
            "main",
            r"
            let Console := effect { let readln () : Int; };
            export let answer () : Int :=
              handle request Console.readln() using Console {
                value => value + 1;
                readln(k) => resume 41;
              };
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let value = vm
        .call_export("answer", &[])
        .expect("handled effect should succeed");

    assert_eq!(value, Value::Int(42));
}

#[test]
fn reuses_handler_value_and_executes_range_membership_and_spread() {
    let program = compile_program(
        &[(
            "main",
            r#"
            let Core := import "musi:core";
            let Bool := Core.Bool;
            let Int := Core.Int;
            let Rangeable := Core.Rangeable;
            let Console := effect { let readln () : Int; };
            let ConsoleHandler := using Console {
              value => value + 1;
              readln(k) => resume 41;
            };
            export let handled () : Int := handle request Console.readln() using ConsoleHandler;
            export let contains () : Bool := (
              let span := 1 ..< 4;
              2 in span
            );
            export let ranged () : Int := (
              let span := 1 ..< 4;
              let xs := [0, ...span, 4];
              xs.[2]
            );
        "#,
        )],
        "main",
    );

    let mut vm = Vm::with_rejecting_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let handled = vm
        .call_export("handled", &[])
        .expect("handler value call should succeed");
    let contains = vm
        .call_export("contains", &[])
        .expect("range membership call should succeed");
    let ranged = vm
        .call_export("ranged", &[])
        .expect("range call should succeed");

    assert_eq!(handled, Value::Int(42));
    assert_eq!(
        render_value_view(vm.inspect(&contains)).as_deref(),
        Some(".True")
    );
    assert_eq!(ranged, Value::Int(2));
}

#[test]
fn exposes_typed_foreign_and_effect_signatures_to_host() {
    let program = compile_program(
        &[(
            "main",
            r#"
            foreign "c" (
              let puts (value : Int) : Int;
            );
            let Console := effect { let readln (prompt : String) : Int; };
            export let call_puts () : Int := puts(1);
            export let call_readln () : Int := request Console.readln(">");
        "#,
        )],
        "main",
    );

    let log = Rc::new(RefCell::new(SignatureLog::default()));
    let host = SignatureHost {
        log: Rc::clone(&log),
    };
    let mut vm = Vm::new(program, RejectingLoader, host, VmOptions);
    vm.initialize().expect("vm init should succeed");

    let foreign_value = vm
        .call_export("call_puts", &[])
        .expect("foreign call should succeed");
    let effect_value = vm
        .call_export("call_readln", &[])
        .expect("effect call should succeed");

    assert_eq!(foreign_value, Value::Int(7));
    assert_eq!(effect_value, Value::Int(42));
    let log = log.borrow();
    assert_eq!(log.foreign_calls.len(), 1);
    assert_eq!(log.foreign_calls[0].0.as_ref(), "main::puts");
    assert_eq!(log.foreign_calls[0].1.len(), 1);
    assert_eq!(log.foreign_calls[0].1[0].as_ref(), "Int");
    assert_eq!(log.foreign_calls[0].2.as_ref(), "Int");
    assert_eq!(log.effect_calls.len(), 1);
    assert_eq!(log.effect_calls[0].0.as_ref(), "main::Console");
    assert_eq!(log.effect_calls[0].1.as_ref(), "readln");
    assert_eq!(log.effect_calls[0].2.len(), 1);
    assert_eq!(log.effect_calls[0].2[0].as_ref(), "String");
    assert_eq!(log.effect_calls[0].3.as_ref(), "Int");
}

#[test]
fn inspects_cptr_values() {
    let program = compile_program(&[("main", "export let answer () : Int := 0;")], "main");
    let vm = Vm::with_rejecting_host(program, VmOptions);
    let value = Value::c_ptr(0xDEAD_BEEF);

    assert!(matches!(vm.inspect(&value), ValueView::CPtr(0xDEAD_BEEF)));
}

#[test]
fn renders_bool_values_as_variants() {
    assert_eq!(
        render_value_view(ValueView::Bool(true)).as_deref(),
        Some(".True")
    );
    assert_eq!(
        render_value_view(ValueView::Bool(false)).as_deref(),
        Some(".False")
    );
}

#[test]
fn exposes_data_layout_descriptors_for_named_types() {
    let program = compile_program(
        &[(
            "main",
            r"
            let Maybe := data { | Some : Int | None };
            export let answer () : Int := 0;
        ",
        )],
        "main",
    );

    let maybe_ty = program
        .artifact()
        .types
        .iter()
        .find_map(|(id, descriptor)| {
            (program.string_text(descriptor.name) == "main::Maybe").then_some(id)
        })
        .expect("type id for Maybe");
    let layout = program
        .type_data_layout(maybe_ty)
        .expect("data layout for Maybe");
    assert_eq!(layout.name.as_ref(), "main::Maybe");
    assert_eq!(layout.variant_count, 2);
    assert_eq!(layout.field_count, 1);
    assert!(!layout.is_single_variant_product());
    assert_eq!(layout.repr_kind, None);
    assert_eq!(
        program.type_abi_kind(maybe_ty),
        ProgramTypeAbiKind::Unsupported
    );
}

fn alloc_named_type(artifact: &mut Artifact, full_name: &str) -> TypeId {
    let name = artifact.intern_string(full_name);
    let term = artifact.intern_string(
        &TypeTerm::new(TypeTermKind::Named {
            module: None,
            name: full_name
                .rsplit_once("::")
                .map_or_else(|| full_name.to_owned(), |(_, local)| local.to_owned())
                .into(),
            args: Box::new([]),
        })
        .to_json(),
    );
    artifact.types.alloc(TypeDescriptor::new(name, term))
}

fn alloc_named_data(
    artifact: &mut Artifact,
    full_name: &str,
    variants: Box<[DataVariantDescriptor]>,
    repr_kind: Option<StringId>,
    layout_align: Option<u32>,
    layout_pack: Option<u32>,
) -> TypeId {
    let ty = alloc_named_type(artifact, full_name);
    let name = artifact.intern_string(full_name);
    let mut descriptor = DataDescriptor::new(name, variants);
    if let Some(repr_kind) = repr_kind {
        descriptor = descriptor.with_repr_kind(repr_kind);
    }
    if let Some(layout_align) = layout_align {
        descriptor = descriptor.with_layout_align(layout_align);
    }
    if let Some(layout_pack) = layout_pack {
        descriptor = descriptor.with_layout_pack(layout_pack);
    }
    let _ = artifact.data.alloc(descriptor);
    ty
}

#[test]
fn classifies_named_data_native_abi_kinds() {
    let mut artifact = Artifact::new();
    let repr_c = artifact.intern_string("c");
    let transparent = artifact.intern_string("transparent");
    let point_variant_name = artifact.intern_string("Point");
    let point_field_ty = alloc_named_type(&mut artifact, "main::Point");
    let handle_variant_name = artifact.intern_string("Handle");
    let handle_field_ty = alloc_named_type(&mut artifact, "main::Handle");
    let none_variant_name = artifact.intern_string("None");
    let some_variant_name = artifact.intern_string("Some");
    let maybe_self_ty = alloc_named_type(&mut artifact, "main::Maybe");
    let point_ty = alloc_named_data(
        &mut artifact,
        "main::Point",
        Box::new([DataVariantDescriptor::new(
            point_variant_name,
            Box::new([point_field_ty; 2]),
        )]),
        Some(repr_c),
        Some(8),
        Some(4),
    );
    let handle_ty = alloc_named_data(
        &mut artifact,
        "main::Handle",
        Box::new([DataVariantDescriptor::new(
            handle_variant_name,
            Box::new([handle_field_ty]),
        )]),
        Some(transparent),
        None,
        None,
    );
    let maybe_ty = alloc_named_data(
        &mut artifact,
        "main::Maybe",
        Box::new([
            DataVariantDescriptor::new(none_variant_name, Box::new([])),
            DataVariantDescriptor::new(some_variant_name, Box::new([maybe_self_ty])),
        ]),
        Some(repr_c),
        None,
        None,
    );

    let program = Program::from_artifact(artifact).expect("program load should succeed");

    assert_eq!(
        program.type_abi_kind(point_ty),
        ProgramTypeAbiKind::DataReprCProduct
    );
    assert_eq!(
        program.type_abi_kind(handle_ty),
        ProgramTypeAbiKind::DataTransparent
    );
    assert_eq!(
        program.type_abi_kind(maybe_ty),
        ProgramTypeAbiKind::Unsupported
    );
}
