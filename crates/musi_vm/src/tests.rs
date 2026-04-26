#![allow(unused_imports)]

use std::collections::HashMap;
use std::iter::empty;
use std::slice::from_ref;
use std::sync::{Arc, Mutex};
use std::thread::spawn;

use crate::gc::{HeapOptions, RuntimeHeap};
use crate::value::SequenceValue;
use crate::vm::{MvmMode, RuntimeFusedOp, RuntimeKernel};
use musi_foundation::register_modules;
use music_module::ModuleKey;
use music_seam::descriptor::{
    DataDescriptor, DataVariantDescriptor, ExportDescriptor, ExportTarget, ProcedureDescriptor,
    TypeDescriptor,
};
use music_seam::{Artifact, CodeEntry, Instruction, Opcode, Operand};
use music_seam::{ProcedureId, StringId, TypeId};
use music_session::{Session, SessionOptions};
use music_term::{TypeTerm, TypeTermKind};

use super::{
    BitsValue, EffectCall, ForeignCall, Program, ProgramTypeAbiKind, RejectingLoader, Value,
    ValueView, Vm, VmError, VmErrorKind, VmHost, VmHostCallContext, VmHostContext, VmLoader,
    VmOptions, VmResult, render_value_view,
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
    fn call_foreign(
        &mut self,
        _ctx: &mut VmHostContext<'_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::ForeignCallRejected {
            foreign: foreign.name().into(),
        }))
    }

    fn handle_effect(
        &mut self,
        _ctx: &mut VmHostContext<'_>,
        effect: &EffectCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: "test host rejected effect call".into(),
        }))
    }
}

#[derive(Default)]
struct SignatureHost {
    log: Arc<Mutex<SignatureLog>>,
}

type ForeignSignatureRecord = (Box<str>, Box<[Box<str>]>, Box<str>);
type EffectSignatureRecord = (Box<str>, Box<str>, Box<[Box<str>]>, Box<str>, bool);

#[derive(Default)]
struct SignatureLog {
    foreign_calls: Vec<ForeignSignatureRecord>,
    effect_calls: Vec<EffectSignatureRecord>,
}

impl VmHost for SignatureHost {
    fn call_foreign(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        let mut log = self.log.lock().map_err(|_| {
            VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "signature log lock poisoned".into(),
            })
        })?;
        log.foreign_calls.push((
            foreign.name().into(),
            foreign
                .param_tys()
                .iter()
                .map(|ty| foreign.type_name(*ty).into())
                .collect::<Vec<Box<str>>>()
                .into_boxed_slice(),
            foreign.result_ty_name().into(),
        ));
        drop(log);
        Ok(Value::Int(7))
    }

    fn handle_effect(
        &mut self,
        _ctx: VmHostCallContext<'_, '_>,
        effect: &EffectCall,
        _args: &[Value],
    ) -> VmResult<Value> {
        let mut log = self.log.lock().map_err(|_| {
            VmError::new(VmErrorKind::InvalidProgramShape {
                detail: "signature log lock poisoned".into(),
            })
        })?;
        log.effect_calls.push((
            effect.effect_name().into(),
            effect.op_name().into(),
            effect
                .param_tys()
                .iter()
                .map(|ty| effect.type_name(*ty).into())
                .collect::<Vec<Box<str>>>()
                .into_boxed_slice(),
            effect.result_ty_name().into(),
            effect.is_comptime_safe(),
        ));
        drop(log);
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

fn compile_runaway_program() -> Program {
    compile_program(
        &[(
            "main",
            r"
            let rec loop (x : Int) : Int := loop(x + 1);
            export let result () : Int := loop(0);
            ",
        )],
        "main",
    )
}

fn call_result_error(source: &str, options: VmOptions, reason: &str) -> VmError {
    let program = compile_program(&[("main", source)], "main");
    let mut vm = Vm::with_rejecting_host(program, options);
    vm.initialize().expect("vm init should succeed");
    vm.call_export("result", &[]).expect_err(reason)
}

fn alloc_exported_bits_proc(artifact: &mut Artifact, name: &str, opcode: Opcode, params: u16) {
    let label = artifact.intern_string("L0");
    let name = artifact.intern_string(name);
    let mut code = vec![
        CodeEntry::Label(music_seam::Label { id: 0 }),
        CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
    ];
    if params == 2 {
        code.push(CodeEntry::Instruction(Instruction::new(
            Opcode::LdLoc,
            Operand::Local(1),
        )));
    }
    code.push(CodeEntry::Instruction(Instruction::new(
        opcode,
        Operand::None,
    )));
    code.push(CodeEntry::Instruction(Instruction::new(
        Opcode::Ret,
        Operand::None,
    )));
    let proc = artifact.procedures.alloc(
        ProcedureDescriptor::new(name, params, params, code.into_boxed_slice())
            .with_export(true)
            .with_labels(Box::new([label])),
    );
    let _ = artifact.exports.alloc(ExportDescriptor::new(
        name,
        false,
        ExportTarget::Procedure(proc),
    ));
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

fn alloc_simple_type(artifact: &mut Artifact, name: &str, kind: TypeTermKind) -> TypeId {
    let name_id = artifact.intern_string(name);
    let term = artifact.intern_string(&TypeTerm::new(kind).to_json());
    artifact.types.alloc(TypeDescriptor::new(name_id, term))
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

mod success {
    use super::*;

    #[test]
    fn loads_program_and_lists_exports() {
        let program = compile_program(
            &[(
                "main",
                "export let result () : Int := 42; export let base : Int := 1;",
            )],
            "main",
        );

        assert_eq!(program.export_count(), 2);
        assert!(
            program
                .exports()
                .iter()
                .any(|export| export.name.as_ref() == "result")
        );
    }

    #[test]
    fn initializes_and_calls_export() {
        let program = compile_program(&[("main", "export let result () : Int := 40 + 2;")], "main");

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let value = vm
            .call_export("result", &[])
            .expect("export call should succeed");

        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn initializes_simple_globals_from_cached_image() {
        let program = compile_program(
            &[(
                "main",
                r"
            let base : Int := 41;
            let offset : Int := 1;
            export let result () : Int := base + offset;
        ",
            )],
            "main",
        );

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        assert_eq!(vm.executed_instructions(), 0);

        let value = vm
            .call_export("result", &[])
            .expect("export call should succeed");
        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn garbage_collection_preserves_returned_external_values() {
        let program = compile_program(
            &[("main", "export let result () : [2]Int := [1, 2];")],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let value = vm
            .call_export("result", &[])
            .expect("array return should succeed");
        let before = vm.heap_allocated_bytes();
        let stats = vm.collect_garbage();

        assert!(before > 0);
        assert_eq!(stats.after_bytes, vm.heap_allocated_bytes());
        let ValueView::Seq(seq) = vm.inspect(&value) else {
            panic!("returned value should remain a sequence");
        };
        assert_eq!(seq.len(), 2);
    }

    #[test]
    fn garbage_collection_keeps_top_level_calls_stable() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result () : Int := (
              let values : [2]Int := [1, 2];
              values.[0] + 41
            );
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let value = vm
            .call_export("result", &[])
            .expect("export call should succeed");
        let before = vm.heap_allocated_bytes();
        let stats = vm.collect_garbage();

        assert_eq!(value, Value::Int(42));
        assert!(before > 0);
        assert!(stats.after_bytes <= before);
        assert_eq!(stats.after_bytes, vm.heap_allocated_bytes());
    }

    #[test]
    fn garbage_collection_breaks_unreachable_cycles() {
        let ty = TypeId::from_raw(0);
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let cycle = heap
            .alloc_sequence(SequenceValue::new(ty, Vec::new().into()), &options)
            .expect("cycle should allocate");
        let Value::Seq(reference) = cycle else {
            panic!("cycle should be a sequence");
        };
        heap.sequence_mut(reference)
            .expect("cycle should be mutable")
            .push_value(Value::Seq(reference));

        let before = heap.allocated_bytes();
        let stats = heap.collect_from_roots(empty::<&Value>());

        assert!(before > 0);
        assert_eq!(stats.after_bytes, 0);
        assert_eq!(heap.allocated_bytes(), 0);
    }

    #[test]
    fn typed_i64_array_sequence_get_set_and_demotes() {
        let ty = TypeId::from_raw(0);
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let sequence = heap
            .alloc_sequence(
                SequenceValue::new(ty, (1..=16).map(Value::Int).collect()),
                &options,
            )
            .expect("sequence should allocate");
        let Value::Seq(sequence_ref) = sequence else {
            panic!("value should be sequence");
        };

        assert!(
            heap.sequence_i64_array_ref(sequence_ref)
                .expect("sequence should inspect")
                .is_some()
        );
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 2)
                .expect("typed array get should succeed"),
            Value::Int(3)
        );
        heap.sequence_set(sequence_ref, 1, Value::Int(9))
            .expect("typed array set should succeed");
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 1)
                .expect("typed array get should succeed"),
            Value::Int(9)
        );

        let child = heap
            .alloc_string("demoted", &options)
            .expect("child string should allocate");
        heap.sequence_set(sequence_ref, 2, child.clone())
            .expect("non-int write should demote");

        assert!(
            heap.sequence_i64_array_ref(sequence_ref)
                .expect("sequence should inspect")
                .is_none()
        );
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 0)
                .expect("demoted sequence get should succeed"),
            Value::Int(1)
        );
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 2)
                .expect("demoted sequence get should succeed"),
            child
        );
    }

    #[test]
    fn shared_i64_array_sequence_detaches_on_write() {
        let ty = TypeId::from_raw(0);
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let (_prototype, buffer) = heap
            .alloc_shared_i64_array_sequence(ty, [0, 1, 2, 3, 4, 5, 6, 7], &options)
            .expect("prototype should allocate");
        let first = heap
            .alloc_sequence_with_i64_array(ty, buffer, 8, &options)
            .expect("first sequence should allocate");
        let second = heap
            .alloc_sequence_with_i64_array(ty, buffer, 8, &options)
            .expect("second sequence should allocate");
        let Value::Seq(first_ref) = first else {
            panic!("first should be sequence");
        };
        let Value::Seq(second_ref) = second else {
            panic!("second should be sequence");
        };

        heap.sequence_set(first_ref, 0, Value::Int(99))
            .expect("shared sequence write should detach");

        assert!(
            heap.sequence_i64_array_ref(first_ref)
                .expect("first should inspect")
                .is_none()
        );
        assert_eq!(
            heap.sequence_i64_array_ref(second_ref)
                .expect("second should inspect"),
            Some(buffer)
        );
        assert_eq!(
            heap.sequence_get_cloned(first_ref, 0)
                .expect("first get should succeed"),
            Value::Int(99)
        );
        assert_eq!(
            heap.sequence_get_cloned(second_ref, 0)
                .expect("second get should succeed"),
            Value::Int(0)
        );
    }

    #[test]
    fn typed_i64_array_sequence_survives_minor_and_major_collection() {
        let ty = TypeId::from_raw(0);
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let sequence = heap
            .alloc_sequence(
                SequenceValue::new(ty, (10..26).map(Value::Int).collect()),
                &options,
            )
            .expect("sequence should allocate");
        let Value::Seq(sequence_ref) = sequence else {
            panic!("value should be sequence");
        };
        let buffer_ref = heap
            .sequence_i64_array_ref(sequence_ref)
            .expect("sequence should inspect")
            .expect("sequence should use typed buffer");

        let _ = heap.collect_minor_from_refs([sequence_ref]);
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 2)
                .expect("minor collection should preserve typed array"),
            Value::Int(12)
        );
        assert!(heap.validate_ref(buffer_ref).is_ok());

        let _ = heap.collect_major_from_refs([sequence_ref]);
        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 1)
                .expect("major collection should preserve typed array"),
            Value::Int(11)
        );
        assert!(heap.validate_ref(buffer_ref).is_ok());
    }

    #[test]
    fn typed_i64_array_demotion_marks_mature_sequence_card() {
        let ty = TypeId::from_raw(0);
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let sequence = heap
            .alloc_sequence(
                SequenceValue::new(ty, (1..=16).map(Value::Int).collect()),
                &options,
            )
            .expect("sequence should allocate");
        let Value::Seq(sequence_ref) = sequence else {
            panic!("value should be sequence");
        };
        for _ in 0..3 {
            let _ = heap.collect_minor_from_refs([sequence_ref]);
        }

        let child = heap
            .alloc_string("young", &options)
            .expect("child string should allocate");
        let Value::String(child_ref) = child.clone() else {
            panic!("child should be string");
        };
        heap.sequence_set(sequence_ref, 1, child.clone())
            .expect("demotion should store young child");
        let _ = heap.collect_minor_from_refs([sequence_ref]);

        assert_eq!(
            heap.sequence_get_cloned(sequence_ref, 1)
                .expect("remembered edge should preserve child"),
            child
        );
        assert_eq!(
            heap.string(child_ref)
                .expect("young child should survive minor collection"),
            "young"
        );
    }

    #[test]
    fn immix_collection_evacuates_fragmented_blocks() {
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let mut roots = Vec::new();
        for index in 0usize..64 {
            let value = heap
                .alloc_string(format!("value-{index}"), &options)
                .expect("string should allocate");
            if index.is_multiple_of(2) {
                roots.push(value);
            }
        }

        let stats = heap.collect_from_roots(roots.iter());

        assert_eq!(stats.after_objects, roots.len());
        assert!(stats.reclaimed_objects > 0);
        assert!(stats.evacuated_objects > 0);
        assert!(stats.free_blocks > 0);
        for root in &roots {
            let Value::String(reference) = root else {
                panic!("root should be string");
            };
            assert!(
                heap.string(*reference)
                    .expect("root should stay live")
                    .starts_with("value-")
            );
        }
    }

    #[test]
    fn large_objects_collect_outside_immix_blocks() {
        let mut heap = RuntimeHeap::default();
        let options = HeapOptions {
            max_object_bytes: None,
        };
        let large = "x".repeat(40 * 1024);
        let value = heap
            .alloc_string(large, &options)
            .expect("large string should allocate");
        let rooted = heap.collect_from_roots([&value]);
        assert_eq!(rooted.reclaimed_objects, 0);
        assert!(rooted.after_bytes > 32 * 1024);

        let unrooted = heap.collect_from_roots(empty::<&Value>());
        assert_eq!(unrooted.after_bytes, 0);
        assert_eq!(unrooted.reclaimed_objects, 1);
    }

    #[test]
    fn isolate_heap_rejects_cross_isolate_values() {
        let program = compile_program(
            &[("main", "export let result () : [2]Int := [1, 2];")],
            "main",
        );
        let mut left = Vm::with_rejecting_host(program.clone(), VmOptions);
        let mut right = Vm::with_rejecting_host(program, VmOptions);
        left.initialize().expect("left vm init should succeed");
        right.initialize().expect("right vm init should succeed");

        let value = left
            .call_export("result", &[])
            .expect("left value should allocate");
        assert_ne!(left.isolate_id(), right.isolate_id());

        let error = right
            .observe_heap_value(&value)
            .expect_err("right vm should reject left heap value");
        assert!(matches!(
            error.kind(),
            VmErrorKind::InvalidProgramShape { detail }
                if detail.as_ref() == "cross-isolate heap reference"
        ));
    }

    #[test]
    fn isolates_run_on_separate_threads() {
        let program = compile_program(&[("main", "export let result () : Int := 42;")], "main");
        let left_program = program.clone();
        let right_program = program;

        let left = spawn(move || {
            let mut vm = Vm::with_rejecting_host(left_program, VmOptions);
            vm.initialize().expect("left vm init should succeed");
            (vm.isolate_id(), vm.call_export("result", &[]))
        });
        let right = spawn(move || {
            let mut vm = Vm::with_rejecting_host(right_program, VmOptions);
            vm.initialize().expect("right vm init should succeed");
            (vm.isolate_id(), vm.call_export("result", &[]))
        });

        let (left_id, left_value) = left.join().expect("left thread should join");
        let (right_id, right_value) = right.join().expect("right thread should join");
        assert_ne!(left_id, right_id);
        assert_eq!(
            left_value.expect("left call should succeed"),
            Value::Int(42)
        );
        assert_eq!(
            right_value.expect("right call should succeed"),
            Value::Int(42)
        );
    }

    #[test]
    fn executes_closure_and_recursive_callable() {
        let program = compile_program(
            &[(
                "main",
                r"
            let apply (f : Int -> Int, x : Int) : Int := f(x);
            export let result (n : Int) : Int := (
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
            .call_export("result", &[Value::Int(3)])
            .expect("recursive closure call should succeed");

        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn fuses_sequence_index_mutation() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result (grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 42;
              grid.[1, 0] := grid.[0, 1] + 1;
              grid.[0, 1] + grid.[1, 0]
            );
        ",
            )],
            "main",
        );
        let result = program
            .loaded_procedure(ProcedureId::from_raw(0))
            .expect("result should load");
        assert!(matches!(
            result.runtime_kernel(),
            Some(RuntimeKernel::Seq2Mutation2x2 { .. } | RuntimeKernel::Seq2Mutation(_))
        ));
        assert!(result.runtime_instructions.iter().any(|instruction| {
            matches!(
                instruction.fused,
                Some(RuntimeFusedOp::LocalSeq2ConstSet { .. })
            )
        }));
        assert!(result.runtime_instructions.iter().any(|instruction| {
            matches!(
                instruction.fused,
                Some(RuntimeFusedOp::LocalSeq2GetAddSet { .. })
            )
        }));
        assert!(result.runtime_instructions.iter().any(|instruction| {
            matches!(
                instruction.fused,
                Some(RuntimeFusedOp::LocalSeq2GetAdd { .. })
            )
        }));

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let ty = TypeId::from_raw(0);
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let grid = vm
            .alloc_sequence(ty, [first, second])
            .expect("grid should allocate");
        let value = vm
            .call_export("result", &[grid])
            .expect("sequence mutation should run");
        assert_eq!(value, Value::Int(85));

        let shared = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("shared row should allocate");
        let aliased = vm
            .alloc_sequence(ty, [shared.clone(), shared])
            .expect("aliased grid should allocate");
        let value = vm
            .call_export("result", &[aliased])
            .expect("aliased sequence mutation should run");
        assert_eq!(value, Value::Int(85));
    }

    #[test]
    fn binds_const_i64_array8_return() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result () : [8]Int := [0, 1, 2, 3, 4, 5, 6, 7];
        ",
            )],
            "main",
        );
        let result = program
            .loaded_procedure(ProcedureId::from_raw(0))
            .expect("result should load");
        assert!(matches!(
            result.runtime_kernel(),
            Some(RuntimeKernel::ConstI64Array8Return {
                cells: [0, 1, 2, 3, 4, 5, 6, 7],
                ..
            })
        ));

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        let bound = vm
            .bind_export_seq8_i64("result")
            .expect("const sequence export should bind");
        let first = vm
            .call_seq8_i64(bound)
            .expect("first const sequence call should run");
        let second = vm
            .call_seq8_i64(bound)
            .expect("second const sequence call should run");

        let (Value::Seq(first_ref), Value::Seq(second_ref)) = (&first, &second) else {
            panic!("calls should return sequences");
        };
        assert_ne!(first_ref, second_ref);
        let ValueView::Seq(sequence) = vm.inspect(&first) else {
            panic!("first should inspect as sequence");
        };
        assert_eq!(sequence.len(), 8);
        assert_eq!(sequence.get(7), Some(Value::Int(7)));

        for _ in 0..5000 {
            let _ = vm
                .call_seq8_i64(bound)
                .expect("pooled const sequence call should run");
        }
        let _ = vm.collect_garbage();
        let third = vm
            .call_seq8_i64(bound)
            .expect("post-collection const sequence call should run");
        let ValueView::Seq(sequence) = vm.inspect(&third) else {
            panic!("third should inspect as sequence");
        };
        assert_eq!(sequence.get(0), Some(Value::Int(0)));
    }

    #[test]
    fn bound_export_call_reuses_const_i64_array8_return() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result () : [8]Int := [0, 1, 2, 3, 4, 5, 6, 7];
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        let bound = vm
            .bind_export_call("result")
            .expect("const sequence export should bind");
        let first = vm
            .call_bound_export(&bound, &[])
            .expect("first bound export call should run");
        let second = vm
            .call_bound_export(&bound, &[])
            .expect("second bound export call should run");

        let (Value::Seq(first_ref), Value::Seq(second_ref)) = (&first, &second) else {
            panic!("calls should return sequences");
        };
        assert_ne!(first_ref, second_ref);
        let ValueView::Seq(sequence) = vm.inspect(&second) else {
            panic!("second should inspect as sequence");
        };
        assert_eq!(sequence.len(), 8);
        assert_eq!(sequence.get(7), Some(Value::Int(7)));
    }

    #[test]
    fn fuses_data_match_option_path() {
        let program = compile_program(
            &[(
                "main",
                r"
            let MaybeInt := data {
              | Some(Int)
              | None
            };
            export let result (n : Int) : Int := (
              let selected : MaybeInt := .Some(n);
              match selected (
              | .Some(value) => value + 1
              | .None => 0
              )
            );
        ",
            )],
            "main",
        );
        let result = program
            .loaded_procedure(ProcedureId::from_raw(0))
            .expect("result should load");
        assert!(matches!(
            result.runtime_kernel(),
            Some(RuntimeKernel::DataConstructMatchAdd { .. })
        ));
        assert!(result.runtime_instructions.iter().any(|instruction| {
            matches!(
                instruction.fused,
                Some(RuntimeFusedOp::LocalNewObj1Init { .. })
            )
        }));
        assert!(result.runtime_instructions.iter().any(|instruction| {
            matches!(
                instruction.fused,
                Some(RuntimeFusedOp::LocalCopyAddSmi { .. })
            )
        }));

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let value = vm
            .call_export("result", &[Value::Int(41)])
            .expect("data match should run");
        assert_eq!(value, Value::Int(42));

        let bound = vm
            .bind_export_i64_i64("result")
            .expect("result should bind");
        let value = vm
            .call_i64_i64(bound, 41)
            .expect("typed data match should run");
        assert_eq!(value, 42);
    }

    #[test]
    fn fuses_recursive_sum_loop() {
        let program = compile_program(
            &[(
                "main",
                r"
            let rec sum (n : Int, acc : Int) : Int :=
              match n (
              | 0 => acc
              | _ => sum(n - 1, acc + n)
            );
            export let result (n : Int) : Int := sum(n, 0);
        ",
            )],
            "main",
        );
        let sum = program
            .loaded_procedure(ProcedureId::from_raw(0))
            .expect("sum should load");
        assert!(matches!(
            sum.runtime_kernel(),
            Some(RuntimeKernel::IntTailAccumulator { .. })
        ));
        let result = program
            .loaded_procedure(ProcedureId::from_raw(1))
            .expect("result should load");
        assert!(matches!(
            result.runtime_kernel(),
            Some(RuntimeKernel::DirectIntWrapperCall { .. })
        ));
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let before = vm.executed_instructions();
        let value = vm
            .call_export("result", &[Value::Int(200)])
            .expect("sum should run");
        let executed = vm.executed_instructions() - before;
        assert_eq!(value, Value::Int(20_100));
        assert!(
            executed <= 220,
            "recursive sum should stay fused: {executed}"
        );

        let bound = vm
            .bind_export_i64_i64("result")
            .expect("result should bind");
        let value = vm.call_i64_i64(bound, 200).expect("typed sum should run");
        assert_eq!(value, 20_100);
        let error = vm
            .call_i64_i64(bound, i64::MAX)
            .expect_err("overflow-risk typed sum should fail");
        assert!(matches!(error.kind(), VmErrorKind::ArithmeticFailed { .. }));
    }

    #[test]
    fn debug_interpreter_mode_uses_general_dispatch() {
        let program = compile_program(
            &[(
                "main",
                r"
            let rec sum (n : Int, acc : Int) : Int :=
              match n (
              | 0 => acc
              | _ => sum(n - 1, acc + n)
            );
            export let result (n : Int) : Int := sum(n, 0);
        ",
            )],
            "main",
        );
        let options = VmOptions.with_mode(MvmMode::DebugInterpreter);
        let mut vm = Vm::with_rejecting_host(program, options);
        vm.initialize().expect("vm init should succeed");

        let before = vm.executed_instructions();
        let value = vm
            .call_export("result", &[Value::Int(200)])
            .expect("sum should run");
        let executed = vm.executed_instructions() - before;

        assert_eq!(value, Value::Int(20_100));
        assert!(
            executed > 220,
            "debug interpreter mode should skip runtime kernel/fused dispatch: {executed}"
        );
    }

    #[test]
    fn interpreter_mode_uses_runtime_kernels() {
        let program = compile_program(
            &[(
                "main",
                r"
            let rec sum (n : Int, acc : Int) : Int :=
              match n (
              | 0 => acc
              | _ => sum(n - 1, acc + n)
            );
            export let result (n : Int) : Int := sum(n, 0);
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions.with_mode(MvmMode::Interpreter));
        vm.initialize().expect("vm init should succeed");

        let before = vm.executed_instructions();
        let value = vm
            .call_export("result", &[Value::Int(200)])
            .expect("sum should run");
        let executed = vm.executed_instructions() - before;

        assert_eq!(value, Value::Int(20_100));
        assert!(
            executed <= 4,
            "interpreter mode should allow runtime kernel dispatch: {executed}"
        );
    }

    #[test]
    fn parses_mvm_options_from_env_and_args() {
        let args = [
            "-Xmvm:Tier=Debug".to_owned(),
            "-Xmvm:+UseKernels".to_owned(),
            "-Xmvm:HeapLimit=4096".to_owned(),
        ];
        let options =
            VmOptions::parse_mvm_options(Some("-Xmvm:Tier=Interp -Xmvm:-UseKernels"), &args)
                .expect("MVM options should parse");

        assert_eq!(options.mode, MvmMode::DebugInterpreter);
        assert_eq!(options.heap_limit_bytes, Some(4096));
        assert!(options.features.has_runtime_kernels());
        assert!(options.features.has_fused_dispatch());
    }

    #[test]
    fn rejects_unknown_mvm_options() {
        let error = VmOptions::parse_mvm_options(None, &["-Xmvm:+unknown".to_owned()])
            .expect_err("unknown MVM option should fail");

        assert!(error.message().contains("unknown MVM option"));
    }

    #[test]
    fn rejects_old_mvm_option_spellings() {
        for option in [
            "-Xmvm:mode=interpreter",
            "-Xmvm:+kernels",
            "-Xmvm:-quickening",
            "-Xmvm:heap=4096",
            "-Xmvm:stack-frames=64",
            "-Xmvm:instruction-budget=100",
            "-Xmvm:gc=stress",
        ] {
            let _error = VmOptions::parse_mvm_options(None, &[option.to_owned()])
                .expect_err("old MVM option spelling should fail");
        }
    }

    #[test]
    fn bound_sequence_call_matches_dynamic_call() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result (grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 42;
              grid.[1, 0] := grid.[0, 1] + 1;
              grid.[0, 1] + grid.[1, 0]
            );
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let bound = vm
            .bind_export_seq2x2_i64("result")
            .expect("result should bind");
        let ty = TypeId::from_raw(0);
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let grid = vm
            .alloc_sequence(ty, [first, second])
            .expect("grid should allocate");
        let dynamic = vm
            .call_export("result", from_ref(&grid))
            .expect("dynamic sequence call should run");
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let typed_grid = vm
            .alloc_sequence(ty, [first, second])
            .expect("typed grid should allocate");
        let Value::Seq(typed_grid_ref) = typed_grid else {
            panic!("typed grid should be sequence")
        };
        let typed = vm
            .call_seq2x2_i64(bound, typed_grid_ref)
            .expect("typed sequence call should run");
        assert_eq!(dynamic, Value::Int(85));
        assert_eq!(typed, 85);
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let bound_grid = vm
            .alloc_sequence(ty, [first.clone(), second.clone()])
            .expect("bound grid should allocate");
        let Value::Seq(bound_grid_ref) = bound_grid else {
            panic!("bound grid should be sequence")
        };
        let typed = {
            let bound_grid_arg = vm
                .bind_seq2x2_i64_arg(bound_grid_ref)
                .expect("grid arg should bind");
            bound_grid_arg.call_i64(bound)
        };
        assert_eq!(typed, 85);
        let ValueView::Seq(first) = vm.inspect(&first) else {
            panic!("first row should inspect as sequence");
        };
        assert_eq!(first.get(1), Some(Value::Int(42)));
        let ValueView::Seq(second) = vm.inspect(&second) else {
            panic!("second row should inspect as sequence");
        };
        assert_eq!(second.get(0), Some(Value::Int(43)));
    }

    #[test]
    fn bound_sequence_call_rejects_non_2x2_layout() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result (grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 42;
              grid.[1, 0] := grid.[0, 1] + 1;
              grid.[0, 1] + grid.[1, 0]
            );
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let ty = TypeId::from_raw(0);
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let third = vm
            .alloc_sequence(ty, [Value::Int(5), Value::Int(6)])
            .expect("third row should allocate");
        let grid = vm
            .alloc_sequence(ty, [first, second, third])
            .expect("grid should allocate");
        let Value::Seq(grid_ref) = grid else {
            panic!("grid should be sequence")
        };
        let bound = vm
            .bind_export_seq2x2_i64("result")
            .expect("result should bind");
        let error = vm
            .call_seq2x2_i64(bound, grid_ref)
            .expect_err("non-2x2 layout should fail typed call");
        assert!(matches!(
            error.kind(),
            VmErrorKind::InvalidProgramShape { .. }
        ));
    }

    #[test]
    fn bound_sequence_call_rejects_stale_grid() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result (grid : mut [2][2]Int) : Int := (
              grid.[0, 1] := 42;
              grid.[1, 0] := grid.[0, 1] + 1;
              grid.[0, 1] + grid.[1, 0]
            );
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let ty = TypeId::from_raw(0);
        let first = vm
            .alloc_sequence(ty, [Value::Int(1), Value::Int(2)])
            .expect("first row should allocate");
        let second = vm
            .alloc_sequence(ty, [Value::Int(3), Value::Int(4)])
            .expect("second row should allocate");
        let grid = vm
            .alloc_sequence(ty, [first, second])
            .expect("grid should allocate");
        let Value::Seq(grid_ref) = grid else {
            panic!("grid should be sequence")
        };
        let bound = vm
            .bind_export_seq2x2_i64("result")
            .expect("result should bind");
        _ = vm.collect_garbage();
        let error = vm
            .call_seq2x2_i64(bound, grid_ref)
            .expect_err("stale grid ref should fail after gc");
        assert!(matches!(
            error.kind(),
            VmErrorKind::InvalidProgramShape { .. }
        ));
    }

    #[test]
    fn executes_record_projection_and_update() {
        let program = compile_program(
            &[(
                "main",
                r"
            export let result () : Int := (
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
            .call_export("result", &[])
            .expect("record call should succeed");

        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn executes_string_order_comparisons() {
        let program = compile_program(
            &[(
                "main",
                r#"
            export let less () : Bool := "a" < "b";
            export let greater () : Bool := "é" > "z";
            export let equal () : Bool := "same" <= "same";
        "#,
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");

        let less = vm.call_export("less", &[]).expect("less");
        assert_eq!(
            render_value_view(vm.inspect(&less)).as_deref(),
            Some(".True")
        );
        let greater = vm.call_export("greater", &[]).expect("greater");
        assert_eq!(
            render_value_view(vm.inspect(&greater)).as_deref(),
            Some(".True")
        );
        let equal = vm.call_export("equal", &[]).expect("equal");
        assert_eq!(
            render_value_view(vm.inspect(&equal)).as_deref(),
            Some(".True")
        );
    }

    #[test]
    fn logical_bool_and_or_short_circuit_and_xor_is_eager() {
        let program = compile_program(
            &[(
                "main",
                r"
            let explode () : Bool := (1 / 0) = 0;
            export let andShort () : Bool := (0 = 1) and explode();
            export let orShort () : Bool := (0 = 0) or explode();
            export let xorEager () : Bool := (0 = 0) xor explode();
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");

        let and_value = vm.call_export("andShort", &[]).expect("and short");
        assert_eq!(
            render_value_view(vm.inspect(&and_value)).as_deref(),
            Some(".False")
        );

        let or_value = vm.call_export("orShort", &[]).expect("or short");
        assert_eq!(
            render_value_view(vm.inspect(&or_value)).as_deref(),
            Some(".True")
        );

        let err = vm.call_export("xorEager", &[]).expect_err("xor eager");
        assert!(matches!(err.kind(), VmErrorKind::ArithmeticFailed { .. }));
    }

    #[test]
    fn logical_bits_ops_are_eager_pointwise() {
        let mut artifact = Artifact::new();
        alloc_exported_bits_proc(&mut artifact, "bitsAnd", Opcode::And, 2);
        alloc_exported_bits_proc(&mut artifact, "bitsOr", Opcode::Or, 2);
        alloc_exported_bits_proc(&mut artifact, "bitsXor", Opcode::Xor, 2);
        alloc_exported_bits_proc(&mut artifact, "bitsNot", Opcode::Not, 1);
        let program = Program::from_artifact(artifact).expect("program load should succeed");
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let a = Value::Bits(BitsValue::from_u64(4, 10));
        let b = Value::Bits(BitsValue::from_u64(4, 12));

        assert_eq!(
            vm.call_export("bitsAnd", &[a.clone(), b.clone()])
                .expect("bits and"),
            Value::Bits(BitsValue::from_u64(4, 8))
        );
        assert_eq!(
            vm.call_export("bitsOr", &[a.clone(), b.clone()])
                .expect("bits or"),
            Value::Bits(BitsValue::from_u64(4, 14))
        );
        assert_eq!(
            vm.call_export("bitsXor", &[a.clone(), b])
                .expect("bits xor"),
            Value::Bits(BitsValue::from_u64(4, 6))
        );
        assert_eq!(
            vm.call_export("bitsNot", &[a]).expect("bits not"),
            Value::Bits(BitsValue::from_u64(4, 5))
        );
    }

    #[test]
    fn brfalse_rejects_non_bool_condition() {
        let mut artifact = Artifact::new();
        let l0 = artifact.intern_string("L0");
        let l1 = artifact.intern_string("L1");
        let name = artifact.intern_string("branch");
        let proc = artifact.procedures.alloc(
            ProcedureDescriptor::new(
                name,
                1,
                1,
                Box::new([
                    CodeEntry::Label(music_seam::Label { id: 0 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdLoc, Operand::Local(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::BrFalse, Operand::Label(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(1))),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                    CodeEntry::Label(music_seam::Label { id: 1 }),
                    CodeEntry::Instruction(Instruction::new(Opcode::LdCI4, Operand::I16(0))),
                    CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
                ]),
            )
            .with_export(true)
            .with_labels(Box::new([l0, l1])),
        );
        let _ = artifact.exports.alloc(ExportDescriptor::new(
            name,
            false,
            ExportTarget::Procedure(proc),
        ));
        let program = Program::from_artifact(artifact).expect("program load should succeed");
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");

        let err = vm
            .call_export("branch", &[Value::Int(0)])
            .expect_err("br.false should reject Int");
        assert!(matches!(
            err.kind(),
            VmErrorKind::InvalidValueKind {
                expected: crate::VmValueKind::Bool,
                found: crate::VmValueKind::Int
            }
        ));
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
        let inner = vm
            .alloc_sequence(outer_ty, [Value::Int(1), Value::Int(2)])
            .expect("inner sequence should allocate");
        let grid = vm
            .alloc_sequence(outer_ty, [inner.clone(), inner])
            .expect("grid sequence should allocate");
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
                "export let result () : Int := 42; export let base : Int := 41;",
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
            .expect("non-literal import should succeed");

        assert_eq!(
            vm.lookup_module_export(&module, "base")
                .expect("module global export should resolve"),
            Value::Int(41)
        );
        assert_eq!(
            vm.call_module_export(&module, "result", &[])
                .expect("module callable export should succeed"),
            Value::Int(42)
        );
        let cached = vm
            .load_module("dep")
            .expect("cached dynamic load should succeed");
        let (ValueView::Module(module), ValueView::Module(cached)) =
            (vm.inspect(&module), vm.inspect(&cached))
        else {
            panic!("dynamic module handles should inspect as modules");
        };
        assert_eq!(cached.slot(), module.slot());
        assert_eq!(cached.spec(), module.spec());
    }

    #[test]
    fn handles_effect_value_clause_and_resume() {
        let program = compile_program(
            &[(
                "main",
                r"
            let Console := effect { let readLine () : Int; };
            let consoleAnswer := answer Console {
              value => value + 1;
              readLine(k) => resume 41;
            };
            export let result () : Int :=
              handle ask Console.readLine() answer consoleAnswer;
        ",
            )],
            "main",
        );
        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let value = vm
            .call_export("result", &[])
            .expect("handled effect should succeed");

        assert_eq!(value, Value::Int(42));
    }

    #[test]
    fn fuses_inline_effect_resume() {
        let program = compile_program(
            &[(
                "main",
                r"
            let Console := effect { let readLine () : Int; };
            let consoleAnswer := answer Console {
              value => value + 1;
              readLine(k) => resume 41;
            };
            export let result () : Int :=
              handle ask Console.readLine() answer consoleAnswer;
        ",
            )],
            "main",
        );

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let result = vm
            .lookup_export("result")
            .expect("result export should resolve");
        let Value::Procedure(procedure) = result else {
            panic!("result export should be procedure");
        };
        assert_eq!(
            vm.module(procedure.module_slot)
                .expect("result module should exist")
                .program
                .loaded_procedure(procedure.procedure)
                .expect("result procedure should exist")
                .runtime_kernel(),
            Some(RuntimeKernel::InlineEffectResume {
                resume_value: 41,
                value_add: 1
            })
        );
        let init = vm
            .bind_export_init0("result")
            .expect("result export should bind");
        assert_eq!(
            vm.call_init0_i64(init)
                .expect("bound effect resume should run"),
            42
        );
        let result = vm
            .lookup_export("result")
            .expect("result export should resolve");
        let before = vm.executed_instructions();
        let value = vm
            .call_value(result, &[])
            .expect("handled effect should run");
        let executed = vm.executed_instructions() - before;

        assert_eq!(value, Value::Int(42));
        assert_eq!(executed, 1);
    }

    #[test]
    fn reuses_handler_value_and_executes_range_membership_and_spread() {
        let program = compile_program(
            &[(
                "main",
                r#"
            import "musi:core";
            let Core := import "musi:core";
            let Bool := Core.Bool;
            let Int := Core.Int;
            let Rangeable := Core.Rangeable;
            let RangeBounds := Core.RangeBounds;
            let Console := effect { let readLine () : Int; };
            let ConsoleAnswer := answer Console {
              value => value + 1;
              readLine(k) => resume 41;
            };
            export let handled () : Int := handle ask Console.readLine() answer ConsoleAnswer;
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
    fn streams_range_membership_without_materialized_sequence() {
        let program = compile_program(
            &[(
                "main",
                r#"
            import "musi:core";
            let Core := import "musi:core";
            let Bool := Core.Bool;
            let Int := Core.Int;
            let Rangeable := Core.Rangeable;
            export let result () : Bool := 0 in (0 ..< 1000);
        "#,
            )],
            "main",
        );

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        let before = vm.heap_allocated_bytes();
        let value = vm
            .call_export("result", &[])
            .expect("range membership call should succeed");
        let after = vm.heap_allocated_bytes();

        assert_eq!(
            render_value_view(vm.inspect(&value)).as_deref(),
            Some(".True")
        );
        assert!(after.saturating_sub(before) < 32 * 1024);
    }

    #[test]
    fn range_endpoint_inclusivity_matches_mathematical_forms() {
        let program = compile_program(
            &[(
                "main",
                r#"
            import "musi:core";
            let Core := import "musi:core";
            let Bool := Core.Bool;
            let Int := Core.Int;
            let Rangeable := Core.Rangeable;
            let RangeBounds := Core.RangeBounds;
            export let closedLower () : Bool := 1 in (1 .. 3);
            export let closedUpper () : Bool := 3 in (1 .. 3);
            export let halfOpenUpper () : Bool := 3 in (1 ..< 3);
            export let openClosedLower () : Bool := 1 in (1 <.. 3);
            export let openClosedUpper () : Bool := 3 in (1 <.. 3);
            export let openOpenLower () : Bool := 1 in (1 <..< 3);
            export let openOpenUpper () : Bool := 3 in (1 <..< 3);
            export let materializedOpenClosed () : Int := (
              let span := 1 <.. 4;
              let xs := [...span];
              xs.[0]
            );
        "#,
            )],
            "main",
        );

        let mut vm = Vm::with_rejecting_host(program, VmOptions);
        vm.initialize().expect("vm init should succeed");
        for name in ["closedLower", "closedUpper", "openClosedUpper"] {
            let value = vm
                .call_export(name, &[])
                .expect("range call should succeed");
            assert_eq!(
                render_value_view(vm.inspect(&value)).as_deref(),
                Some(".True")
            );
        }
        for name in [
            "halfOpenUpper",
            "openClosedLower",
            "openOpenLower",
            "openOpenUpper",
        ] {
            let value = vm
                .call_export(name, &[])
                .expect("range call should succeed");
            assert_eq!(
                render_value_view(vm.inspect(&value)).as_deref(),
                Some(".False")
            );
        }
        let value = vm
            .call_export("materializedOpenClosed", &[])
            .expect("range materialization should succeed");
        assert_eq!(value, Value::Int(2));
    }

    #[test]
    fn exposes_typed_foreign_and_effect_signatures_to_host() {
        let program = compile_program(
            &[(
                "main",
                r#"
            native "c" (
              let puts (value : Int) : Int;
            );
            let Console := effect { @knownSafe let readLine (prompt : String) : Int; };
            export let call_puts () : Int := unsafe { puts(1); };
            export let call_readLine () : Int := ask Console.readLine(">");
        "#,
            )],
            "main",
        );

        let log = Arc::new(Mutex::new(SignatureLog::default()));
        let host = SignatureHost {
            log: Arc::clone(&log),
        };
        let mut vm = Vm::new(program, RejectingLoader, host, VmOptions);
        vm.initialize().expect("vm init should succeed");

        let foreign_value = vm
            .call_export("call_puts", &[])
            .expect("foreign call should succeed");
        let effect_value = vm
            .call_export("call_readLine", &[])
            .expect("effect call should succeed");

        assert_eq!(foreign_value, Value::Int(7));
        assert_eq!(effect_value, Value::Int(42));
        let log = log.lock().expect("signature log should lock");
        assert_eq!(log.foreign_calls.len(), 1);
        assert_eq!(log.foreign_calls[0].0.as_ref(), "main::puts");
        assert_eq!(log.foreign_calls[0].1.len(), 1);
        assert_eq!(log.foreign_calls[0].1[0].as_ref(), "Int");
        assert_eq!(log.foreign_calls[0].2.as_ref(), "Int");
        assert_eq!(log.effect_calls.len(), 1);
        assert_eq!(log.effect_calls[0].0.as_ref(), "main::Console");
        assert_eq!(log.effect_calls[0].1.as_ref(), "readLine");
        assert_eq!(log.effect_calls[0].2.len(), 1);
        assert_eq!(log.effect_calls[0].2[0].as_ref(), "String");
        assert_eq!(log.effect_calls[0].3.as_ref(), "Int");
        assert!(log.effect_calls[0].4);
        drop(log);
    }

    #[test]
    fn inspects_cptr_values() {
        let program = compile_program(&[("main", "export let result () : Int := 0;")], "main");
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
            let Maybe := data { | Some(Int) | None };
            export let result () : Int := 0;
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

    #[test]
    fn classifies_fixed_width_native_abi_kinds() {
        let mut artifact = Artifact::new();
        let int32 = alloc_simple_type(&mut artifact, "Int32", TypeTermKind::Int32);
        let nat64 = alloc_simple_type(&mut artifact, "Nat64", TypeTermKind::Nat64);
        let float32 = alloc_simple_type(&mut artifact, "Float32", TypeTermKind::Float32);
        let float64 = alloc_simple_type(&mut artifact, "Float64", TypeTermKind::Float64);
        let program = Program::from_artifact(artifact).expect("program load should succeed");

        assert_eq!(
            program.type_abi_kind(int32),
            ProgramTypeAbiKind::Int {
                signed: true,
                bits: 32
            }
        );
        assert_eq!(
            program.type_abi_kind(nat64),
            ProgramTypeAbiKind::Int {
                signed: false,
                bits: 64
            }
        );
        assert_eq!(
            program.type_abi_kind(float32),
            ProgramTypeAbiKind::Float { bits: 32 }
        );
        assert_eq!(
            program.type_abi_kind(float64),
            ProgramTypeAbiKind::Float { bits: 64 }
        );
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
                0,
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
                0,
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
                DataVariantDescriptor::new(none_variant_name, 0, Box::new([])),
                DataVariantDescriptor::new(some_variant_name, 1, Box::new([maybe_self_ty])),
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
}

mod failure {
    use super::*;

    #[test]
    fn instruction_budget_stops_runaway_execution() {
        let program = compile_runaway_program();

        let mut vm = Vm::with_rejecting_host(program, VmOptions.with_instruction_budget(64));
        vm.initialize().expect("vm init should succeed");
        let error = vm
            .call_export("result", &[])
            .expect_err("runaway execution should exhaust instruction budget");

        assert!(matches!(
            error.kind(),
            VmErrorKind::InstructionBudgetExhausted { budget: 64 }
        ));
    }

    #[test]
    fn stack_frame_limit_stops_runaway_recursion() {
        let program = compile_runaway_program();

        let mut vm = Vm::with_rejecting_host(program, VmOptions.with_stack_frame_limit(8));
        vm.initialize().expect("vm init should succeed");
        let error = vm
            .call_export("result", &[])
            .expect_err("runaway recursion should exceed stack frame limit");

        assert!(matches!(
            error.kind(),
            VmErrorKind::StackFrameLimitExceeded {
                frames: 9,
                limit: 8
            }
        ));
    }

    #[test]
    fn heap_object_limit_rejects_large_runtime_object() {
        let error = call_result_error(
            r#"export let result () : String := "abcdef";"#,
            VmOptions.with_max_object_bytes(8),
            "large string should exceed object limit",
        );

        assert!(matches!(
            error.kind(),
            VmErrorKind::HeapObjectTooLarge { limit: 8, .. }
        ));
    }

    #[test]
    fn heap_limit_rejects_live_graph_after_collection() {
        let error = call_result_error(
            "export let result () : [2]Int := [1, 2];",
            VmOptions.with_heap_limit_bytes(16),
            "live array should exceed heap limit",
        );

        assert!(
            matches!(error.kind(), VmErrorKind::HeapLimitExceeded { .. }),
            "{:?}",
            error.kind()
        );
    }

    #[test]
    fn rejects_opaque_exports_from_root_and_dynamic_modules() {
        let dep = compile_program(
            &[(
                "dep",
                "export opaque let Hidden := data { | Hidden(Int) }; export let result () : Int := 42;",
            )],
            "dep",
        );
        let main = compile_program(
            &[(
                "main",
                "export opaque let Secret := data { | Secret(Int) }; export let root () : Int := 0;",
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
            .expect("non-literal import should succeed");
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
        let mut dep_artifact = Artifact::new();
        let init_name = dep_artifact.intern_string("dep::__module_init");
        let spec = dep_artifact.intern_string("dep");
        let _ = dep_artifact.procedures.alloc(ProcedureDescriptor::new(
            init_name,
            0,
            0,
            Box::new([
                CodeEntry::Instruction(Instruction::new(Opcode::LdStr, Operand::String(spec))),
                CodeEntry::Instruction(Instruction::new(Opcode::MdlLoad, Operand::None)),
                CodeEntry::Instruction(Instruction::new(Opcode::Ret, Operand::None)),
            ]),
        ));
        let dep = Program::from_artifact(dep_artifact).expect("program load should succeed");

        let mut loader = TestLoader::default();
        let _ = loader.modules.insert("dep".into(), dep);
        let mut vm = Vm::new(main, loader, TestHost, VmOptions);
        vm.initialize().unwrap();
        let err = vm.load_module("dep").unwrap_err();

        assert!(matches!(err.kind(), VmErrorKind::ModuleInitCycle { .. }));
    }
}
