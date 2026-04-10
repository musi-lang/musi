use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use super::{
    EffectCall, ForeignCall, NativeLoader, Program, Value, Vm, VmError, VmErrorKind, VmHost,
    VmLoader, VmOptions, VmResult,
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

    let mut vm = Vm::with_native_host(program, VmOptions);
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
              let rec loop (x : Int) : Int := case x of (| 0 => base | _ => loop(x - 1));
              let add_base (y : Int) : Int := y + 41;
              apply(add_base, loop(n))
            );
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_native_host(program, VmOptions);
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
              let updated := point.{ x := 40 };
              updated.x + point.y
            );
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_native_host(program, VmOptions);
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
            export let touch (grid : mut Array[Int, 2, 2]) : Int := (
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
    let mut vm = Vm::with_native_host(program, VmOptions);
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
fn handles_effect_value_clause_and_resume() {
    let program = compile_program(
        &[(
            "main",
            r"
            let Console := effect { let readln () : Int; };
            export let answer () : Int :=
              handle perform Console.readln() with Console of (
              | value => value + 1
              | readln(k) => resume 41
              );
        ",
        )],
        "main",
    );

    let mut vm = Vm::with_native_host(program, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let value = vm
        .call_export("answer", &[])
        .expect("handled effect should succeed");

    assert_eq!(value, Value::Int(42));
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
            export let call_readln () : Int := perform Console.readln(">");
        "#,
        )],
        "main",
    );

    let log = Rc::new(RefCell::new(SignatureLog::default()));
    let host = SignatureHost {
        log: Rc::clone(&log),
    };
    let mut vm = Vm::new(program, NativeLoader, host, VmOptions);
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
