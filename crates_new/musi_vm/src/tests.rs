use std::collections::HashMap;

use music_module::ModuleKey;
use music_session::{Session, SessionOptions};

use super::{
    EffectCall, ForeignCall, Program, Value, Vm, VmError, VmErrorKind, VmHost, VmOptions, VmResult,
};

#[derive(Default)]
struct TestHost {
    modules: HashMap<Box<str>, Program>,
    syntax_results: HashMap<Box<str>, Value>,
}

impl VmHost for TestHost {
    fn load_module(&mut self, spec: &str) -> VmResult<Program> {
        self.modules
            .get(spec)
            .cloned()
            .ok_or_else(|| VmError::new(VmErrorKind::ModuleLoadRejected { spec: spec.into() }))
    }

    fn call_foreign(&mut self, foreign: &ForeignCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name.clone(),
        }))
    }

    fn handle_effect(&mut self, effect: &EffectCall, _args: &[Value]) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name.clone(),
            op: Some(effect.op_name.clone()),
            reason: "test host does not handle effect".into(),
        }))
    }

    fn eval_syntax(&mut self, syntax: &str) -> VmResult<Value> {
        self.syntax_results.get(syntax).cloned().ok_or_else(|| {
            VmError::new(VmErrorKind::SyntaxEvalRejected {
                syntax: syntax.into(),
            })
        })
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

    let mut host = TestHost::default();
    let _ = host.modules.insert("dep".into(), dep);
    let mut vm = Vm::new(main, host, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let loaded = vm
        .load_module("dep")
        .expect("dynamic import should succeed");

    assert_eq!(
        vm.lookup_module_export(&loaded, "base")
            .expect("module global export should resolve"),
        Value::Int(41)
    );
    assert_eq!(
        vm.call_module_export(&loaded, "answer", &[])
            .expect("module callable export should succeed"),
        Value::Int(42)
    );
    assert_eq!(
        vm.load_module("dep")
            .expect("cached dynamic load should succeed"),
        loaded
    );
}

#[test]
fn delegates_syntax_eval_to_host() {
    let program = compile_program(
        &[("main", "export let quoted : Syntax := quote (#(1 + 2));")],
        "main",
    );
    let syntax = "quote (#(1 + 2))";
    let mut host = TestHost::default();
    let _ = host.syntax_results.insert(syntax.into(), Value::Int(42));
    let mut vm = Vm::new(program, host, VmOptions);
    vm.initialize().expect("vm init should succeed");
    let quoted = vm
        .lookup_export("quoted")
        .expect("quoted export should exist");
    let result = vm.eval_syntax(&quoted).expect("syntax eval should succeed");

    assert_eq!(result, Value::Int(42));
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
