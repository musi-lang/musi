//! VM integration tests.
//!
//! All tests build minimal `.msbc` binaries from raw bytes — no compiler
//! crate dependency.
#![allow(clippy::panic)]

use std::iter;

use musi_bc::{Opcode, crc32_slice};

use crate::error::VmError;
use crate::loader::load;
use crate::value::Value;
use crate::verifier::verify;
use crate::vm::{StepResult, Vm};

// ── Binary builder helpers ────────────────────────────────────────────────────

/// Constant pool entry for the test builder.
enum ConstEntry {
    I32(i32),
    Str(Vec<u8>),
}

/// Function definition for the test builder.
struct FnDef {
    fn_id: u32,
    local_count: u16,
    param_count: u16,
    code: Vec<u8>,
    handlers: Vec<(u8, u32)>,
}

/// Convenience: function def without handlers.
fn fn_def(fn_id: u32, local_count: u16, param_count: u16, code: Vec<u8>) -> FnDef {
    FnDef {
        fn_id,
        local_count,
        param_count,
        code,
        handlers: vec![],
    }
}

/// Build a minimal valid `.msbc` binary.
fn make_msbc(consts: &[ConstEntry], fns: &[FnDef]) -> Vec<u8> {
    let entry_fn_id: u32 = fns.first().map_or(0, |f| f.fn_id);

    // ── Const pool ────────────────────────────────────────────────────────────
    let mut const_section: Vec<u8> = vec![];
    let const_count = u32::try_from(consts.len()).expect("fits u32");
    const_section.extend_from_slice(&const_count.to_le_bytes());
    for c in consts {
        match c {
            ConstEntry::I32(v) => {
                const_section.push(0x01); // TAG_I32
                const_section.extend_from_slice(&v.to_le_bytes());
            }
            ConstEntry::Str(bytes) => {
                const_section.push(0x05); // TAG_STR
                let len = u32::try_from(bytes.len()).expect("fits u32");
                const_section.extend_from_slice(&len.to_le_bytes());
                const_section.extend_from_slice(bytes);
            }
        }
    }

    // ── Type pool (empty) ─────────────────────────────────────────────────────
    let type_section: Vec<u8> = 0u32.to_le_bytes().to_vec();

    // ── Effect pool (empty) ───────────────────────────────────────────────────
    let effect_section: Vec<u8> = 0u32.to_le_bytes().to_vec();

    // ── Function pool ─────────────────────────────────────────────────────────
    let mut fn_section: Vec<u8> = vec![];
    let fn_count = u32::try_from(fns.len()).expect("fits u32");
    fn_section.extend_from_slice(&fn_count.to_le_bytes());
    for f in fns {
        fn_section.extend_from_slice(&f.fn_id.to_le_bytes());
        fn_section.extend_from_slice(&0u32.to_le_bytes()); // type_id
        fn_section.extend_from_slice(&f.local_count.to_le_bytes());
        fn_section.extend_from_slice(&f.param_count.to_le_bytes());
        let max_stack: u16 = 16;
        fn_section.extend_from_slice(&max_stack.to_le_bytes());
        fn_section.extend_from_slice(&0u16.to_le_bytes()); // effect_mask
        let code_len = u32::try_from(f.code.len()).expect("fits u32");
        fn_section.extend_from_slice(&code_len.to_le_bytes());
        fn_section.extend_from_slice(&f.code);
        let handler_count = u16::try_from(f.handlers.len()).expect("fits u16");
        fn_section.extend_from_slice(&handler_count.to_le_bytes());
        for &(eid, hfn) in &f.handlers {
            fn_section.push(eid);
            fn_section.extend_from_slice(&hfn.to_le_bytes());
        }
    }

    // ── Foreign pool (empty for tests) ──────────────────────────────────────
    let foreign_section: Vec<u8> = 0u32.to_le_bytes().to_vec(); // count = 0

    // ── Header ────────────────────────────────────────────────────────────────
    let header_size: u32 = 40;
    let const_off = header_size;
    let type_off = const_off + u32::try_from(const_section.len()).expect("fits u32");
    let effect_off = type_off + u32::try_from(type_section.len()).expect("fits u32");
    let foreign_off = effect_off + u32::try_from(effect_section.len()).expect("fits u32");
    let fn_off = foreign_off + u32::try_from(foreign_section.len()).expect("fits u32");

    let mut header: Vec<u8> = Vec::with_capacity(40);
    header.extend_from_slice(b"MUSI");
    header.extend_from_slice(&1u16.to_le_bytes());
    header.extend_from_slice(&0u16.to_le_bytes());
    header.extend_from_slice(&4u32.to_le_bytes()); // flags (IS_SCRIPT)
    header.extend_from_slice(&entry_fn_id.to_le_bytes());
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&effect_off.to_le_bytes());
    header.extend_from_slice(&foreign_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());

    debug_assert_eq!(header.len(), 36);
    let checksum = crc32_slice(&header);
    header.extend_from_slice(&checksum.to_le_bytes());
    debug_assert_eq!(header.len(), 40);

    let mut out = header;
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&effect_section);
    out.extend_from_slice(&foreign_section);
    out.extend_from_slice(&fn_section);
    out
}

/// Load, verify, and run the entry function of a `.msbc` binary.
fn run_vm(bytes: &[u8]) -> (Vm, Result<Value, VmError>) {
    let module = load(bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.run();
    (vm, result)
}

/// Load, verify, and call a specific function with arguments.
fn run_vm_call(bytes: &[u8], fn_id: u32, args: &[Value]) -> (Vm, Result<Value, VmError>) {
    let module = load(bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.call_fn(fn_id, args);
    (vm, result)
}

// ── Loader tests ──────────────────────────────────────────────────────────────

#[test]
fn test_load_valid_header_succeeds() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_U.0])]);
    let result = load(&bytes);
    assert!(result.is_ok(), "expected Ok, got {result:?}");
}

#[test]
fn test_load_bad_magic_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_U.0])]);
    bytes[0] = b'X';
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadMagic)),
        "expected BadMagic, got {result:?}"
    );
}

#[test]
fn test_load_bad_checksum_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_U.0])]);
    bytes[8] ^= 0xFF;
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadChecksum)),
        "expected BadChecksum, got {result:?}"
    );
}

// ── Verifier tests ────────────────────────────────────────────────────────────

#[test]
fn test_verifier_rejects_oob_const() {
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, 0, vec![Opcode::LD_CST.0, 5, Opcode::RET.0])],
    );
    let module = load(&bytes).expect("loads ok");
    let result = verify(&module);
    assert!(result.is_err(), "expected Verify error, got Ok");
}

#[test]
fn test_verifier_rejects_stack_overflow() {
    let mut code = vec![Opcode::LD_CST.0, 0u8];
    code.extend(iter::repeat_n(Opcode::DUP.0, 20));
    code.push(Opcode::RET.0);
    let bytes = make_msbc(&[ConstEntry::I32(42)], &[fn_def(0, 0, 0, code)]);
    let module = load(&bytes).expect("loads ok");
    let result = verify(&module);
    assert!(result.is_err(), "expected Verify error for stack overflow");
}

// ── Execution tests (original) ───────────────────────────────────────────────

#[test]
fn test_run_constant_return_i32() {
    let bytes = make_msbc(
        &[ConstEntry::I32(99)],
        &[fn_def(0, 0, 0, vec![Opcode::LD_CST.0, 0, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_run_add_two_ints() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            2,
            2,
            vec![
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_LOC.0,
                1,
                Opcode::I_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(3), Value::from_int(4)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

#[test]
fn test_run_conditional_jump() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5), ConstEntry::I32(3)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CMP_LT.0,
                0,
                Opcode::JMP_F.0,
                2,
                0,
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(
        result.expect("runs").as_int().expect("is int"),
        3,
        "jmp.f taken: should return 3"
    );
}

#[test]
fn test_run_tail_call_countdown() {
    let code = vec![
        Opcode::LD_LOC.0,
        0,
        Opcode::LD_CST.0,
        0,
        Opcode::CMP_EQ.0,
        Opcode::JMP_F.0,
        3,
        0,
        Opcode::LD_CST.0,
        0,
        Opcode::RET.0,
        Opcode::LD_LOC.0,
        0,
        Opcode::LD_CST.0,
        1,
        Opcode::I_SUB.0,
        Opcode::INV_TAL.0,
        0,
        0,
        0,
        0,
    ];
    let bytes = make_msbc(
        &[ConstEntry::I32(0), ConstEntry::I32(1)],
        &[fn_def(0, 1, 1, code)],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(10)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0);
}

#[test]
fn test_run_make_product_and_load_field() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(20)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::MK_PRD.0,
                2,
                Opcode::LD_FLD.0,
                1,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 20);
}

#[test]
fn test_run_make_variant_and_check_tag() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::MK_VAR.0,
                7,
                Opcode::CMP_TAG.0,
                7,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

// ── Value NaN-boxing tests ────────────────────────────────────────────────────

#[test]
fn test_value_nan_boxing_roundtrip() {
    type Case = (Value, fn(Value) -> bool);
    let cases: &[Case] = &[
        (Value::from_int(-1), |v| v.as_int().is_ok()),
        (Value::from_int(i64::MAX >> 16), |v| v.as_int().is_ok()),
        (Value::from_uint(0xDEAD_BEEF), |v| v.as_uint().is_ok()),
        (Value::from_float(1.5), |v| v.as_float().is_ok()),
        (Value::from_bool(true), |v| v.as_bool().is_ok()),
        (Value::from_bool(false), |v| v.as_bool().is_ok()),
        (Value::from_rune('A'), |v| {
            v.as_int().is_err() && !v.is_float()
        }),
        (Value::from_fn_id(42), |v| v.as_fn_id().is_ok()),
        (Value::UNIT, |v| v.is_unit()),
    ];
    for (val, check) in cases {
        assert!(check(*val), "roundtrip failed for {val:?}");
    }
}

#[test]
fn test_value_float_is_not_tagged_int() {
    let f = Value::from_float(1.0);
    assert!(f.is_float());
    assert!(f.as_int().is_err());

    let i = Value::from_int(42);
    assert!(!i.is_float());
    assert!(i.as_float().is_err());
}

#[test]
fn test_value_int_sign_extension() {
    let v = Value::from_int(-1);
    assert_eq!(v.as_int().expect("is int"), -1);
    let v2 = Value::from_int(-42);
    assert_eq!(v2.as_int().expect("is int"), -42);
}

// ── String values ───────────────────────────────────────────────────────

#[test]
fn test_string_const_returns_heap_ref() {
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hello".to_vec())],
        &[fn_def(0, 0, 0, vec![Opcode::LD_CST.0, 0, Opcode::RET.0])],
    );
    let (vm, result) = run_vm(&bytes);
    let result = result.expect("runs");
    let ptr = result.as_ref().expect("should be a ref");
    let obj = vm.heap().get(ptr).expect("heap lookup");
    assert_eq!(
        obj.string.as_deref(),
        Some("hello"),
        "heap object should contain the string"
    );
}

#[test]
fn test_string_const_two_distinct_loads_produce_separate_objects() {
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hi".to_vec())],
        &[fn_def(
            0,
            2,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::ST_LOC.0,
                0, // first load
                Opcode::LD_CST.0,
                0,
                Opcode::ST_LOC.0,
                1, // second load
                Opcode::LD_LOC.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_ref().is_ok(), "should be a ref");
}

// ── Global variables ────────────────────────────────────────────────────

#[test]
fn test_globals_store_and_load() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 42
                Opcode::ST_GLB.0,
                5,
                0,
                0,
                0, // store to global[5]
                Opcode::LD_GLB.0,
                5,
                0,
                0,
                0, // load global[5]
                Opcode::RET.0,
            ],
        )],
    );
    let (vm, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
    assert_eq!(
        vm.globals().len(),
        6,
        "globals should have grown to index 5+1"
    );
}

#[test]
fn test_globals_uninitialized_returns_unit() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_GLB.0, 0, 0, 0, 0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(
        result.expect("runs").is_unit(),
        "uninitialized global should be unit"
    );
}

// ── Division by zero ────────────────────────────────────────────────────

#[test]
fn test_division_by_zero_returns_error() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(0)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::I_DIV.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(matches!(**source, VmError::DivideByZero));
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

// ── Float arithmetic ────────────────────────────────────────────────────

#[test]
fn test_float_add_and_multiply() {
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                0,
                Opcode::F_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 5.0).abs() < f64::EPSILON);
}

// ── Bitwise operations ──────────────────────────────────────────────────

#[test]
fn test_bitwise_and_and_shift() {
    let bytes = make_msbc(
        &[
            ConstEntry::I32(0xFF),
            ConstEntry::I32(0x0F),
            ConstEntry::I32(4),
        ],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::B_AND.0,
                Opcode::LD_CST.0,
                2,
                Opcode::B_SHL.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 240);
}

// ── Type conversions ────────────────────────────────────────────────────

#[test]
fn test_int_to_float_to_int_roundtrip() {
    let bytes = make_msbc(
        &[ConstEntry::I32(7)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                0,
                Opcode::CNV_FTI.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

// ── Array operations ────────────────────────────────────────────────────

#[test]
fn test_array_create_store_load() {
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(99), ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 3 (length)
                Opcode::MK_ARR.0,
                0,
                0,
                0,
                0, // mk.arr type_id=0 → ref on stack
                Opcode::ST_LOC.0,
                0, // save ref
                Opcode::LD_LOC.0,
                0, // push ref
                Opcode::LD_CST.0,
                2, // push index 1
                Opcode::LD_CST.0,
                1, // push value 99
                Opcode::ST_IDX.0,
                0, // arr[1] = 99 (2-byte instr)
                Opcode::LD_LOC.0,
                0, // push ref
                Opcode::LD_CST.0,
                2, // push index 1
                Opcode::LD_IDX.0,
                0, // load arr[1] (2-byte instr)
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_array_length() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 5 (length)
                Opcode::MK_ARR.0,
                0,
                0,
                0,
                0, // mk.arr → ref
                Opcode::LD_LEN.0,
                0, // push length (2-byte instr)
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_uint().expect("is uint"), 5);
}

// ── Stack underflow ─────────────────────────────────────────────────────

#[test]
fn test_stack_underflow_returns_error() {
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, 0, vec![Opcode::I_ADD.0, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.is_err(), "i.add on empty stack should error");
}

// ── HLT instruction ────────────────────────────────────────────────────

#[test]
fn test_hlt_returns_halted_error() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::HLT.0])]);
    let (_, result) = run_vm(&bytes);
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(matches!(**source, VmError::Halted));
        }
        _ => panic!("expected Runtime wrapping Halted, got {err:?}"),
    }
}

// ── Instruction limit ──────────────────────────────────────────────────

#[test]
fn test_instruction_limit_exceeded() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::JMP_W.0,
                0xFB,
                0xFF,
                0xFF,
                0xFF, // i32 = -5 (little-endian)
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.set_instruction_limit(Some(100));
    let result = vm.run();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        matches!(err, VmError::InstructionLimitExceeded { limit: 100 }),
        "expected InstructionLimitExceeded, got {err:?}"
    );
    assert_eq!(vm.instruction_count(), 100);
}

// ── Error context ───────────────────────────────────────────────────────

#[test]
fn test_error_context_contains_fn_id_and_ip() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(0)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::I_DIV.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { fn_id, ip, .. } => {
            assert_eq!(*fn_id, 0, "fn_id should be 0");
            assert_eq!(*ip, 4, "ip should point to i.div at offset 4");
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

// ── Public stepping API ─────────────────────────────────────────────────

#[test]
fn test_step_api_single_stepping() {
    let bytes = make_msbc(
        &[ConstEntry::I32(77)],
        &[fn_def(0, 0, 0, vec![Opcode::LD_CST.0, 0, Opcode::RET.0])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.setup_call(0, &[]).expect("setup ok");

    assert!(vm.is_running());

    match vm.step().expect("step 1") {
        StepResult::Continue => {}
        StepResult::Returned(_) => panic!("expected Continue after ld.cst"),
    }

    match vm.step().expect("step 2") {
        StepResult::Continue => panic!("expected Returned after ret"),
        StepResult::Returned(v) => {
            assert_eq!(v.as_int().expect("is int"), 77);
        }
    }

    assert!(!vm.is_running());
}

#[test]
fn test_introspection_frames_and_heap() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::MK_PRD.0, 1, Opcode::RET.0],
        )],
    );
    let (vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert!(vm.frames().is_empty());
    assert!(vm.heap().live_count() >= 1);
}

// ── Garbage collection ──────────────────────────────────────────────────

#[test]
fn test_gc_collects_unreachable_objects() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 1
                Opcode::MK_PRD.0,
                1,             // mk.prd 1 → ref (heap object)
                Opcode::POP.0, // discard the ref — object is now unreachable
                Opcode::LD_CST.0,
                0, // push 1 (keep something reachable)
                Opcode::MK_PRD.0,
                1, // mk.prd 1 → ref (heap object, reachable)
                Opcode::RET.0,
            ],
        )],
    );
    let (mut vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert_eq!(vm.heap().live_count(), 2);
    let freed = vm.collect_garbage();
    assert!(
        freed >= 1,
        "GC should free at least 1 object, freed {freed}"
    );
}

#[test]
fn test_gc_preserves_reachable_globals() {
    let bytes = make_msbc(
        &[ConstEntry::I32(99)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 99
                Opcode::MK_PRD.0,
                1, // mk.prd 1 → ref
                Opcode::ST_GLB.0,
                0,
                0,
                0,
                0, // store to global[0]
                Opcode::RET_U.0,
            ],
        )],
    );
    let (mut vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert_eq!(vm.heap().live_count(), 1);
    let freed = vm.collect_garbage();
    assert_eq!(freed, 0, "GC should not free globally-reachable objects");
    assert_eq!(vm.heap().live_count(), 1);
}

// ── Dynamic invocation ──────────────────────────────────────────────────

#[test]
fn test_direct_call_with_inv() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(5)],
        &[
            fn_def(
                0,
                0,
                0,
                vec![
                    Opcode::LD_CST.0,
                    0,
                    Opcode::INV.0,
                    1,
                    0,
                    0,
                    0,
                    Opcode::RET.0,
                ],
            ),
            fn_def(
                1,
                1,
                1,
                vec![
                    Opcode::LD_LOC.0,
                    0,
                    Opcode::LD_CST.0,
                    1,
                    Opcode::I_ADD.0,
                    Opcode::RET.0,
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 15);
}

// ── Wide jump ───────────────────────────────────────────────────────────

#[test]
fn test_wide_jump() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::JMP_W.0,
                2,
                0,
                0,
                0,             // jmp.w +2, target = 7
                Opcode::HLT.0, // offset 5, skipped
                Opcode::NOP.0, // NOP at offset 6, skipped
                Opcode::LD_CST.0,
                0,             // offset 7, push 42
                Opcode::RET.0, // offset 9
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

// ── Integer negation ────────────────────────────────────────────────────

#[test]
fn test_int_negation() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::I_NEG.0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), -42);
}

// ── Float multiplication ────────────────────────────────────────────────

#[test]
fn test_float_multiply() {
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(4)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                0,
                Opcode::F_MUL.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 12.0).abs() < f64::EPSILON);
}

// ── CMP_EQ ──────────────────────────────────────────────────────────────

#[test]
fn test_cmp_eq_equal_values() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                0,
                Opcode::CMP_EQ.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(
        result.expect("runs").as_bool().expect("is bool"),
        "5 == 5 should be true"
    );
}

// ── Value try_as_ref ────────────────────────────────────────────────────

#[test]
fn test_value_try_as_ref() {
    assert!(Value::from_ref(42).try_as_ref().is_some());
    assert_eq!(Value::from_ref(42).try_as_ref(), Some(42));
    assert!(Value::from_int(42).try_as_ref().is_none());
    assert!(Value::UNIT.try_as_ref().is_none());
}

// ── Tier 1: CNV_TRM ──────────────────────────────────────────────────────────

#[test]
fn test_cnv_trm_passthrough_preserves_bits() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::CNV_TRM.0, 0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

// ── Tier 1: FRE ──────────────────────────────────────────────────────────────

#[test]
fn test_fre_frees_heap_object() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 1
                Opcode::MK_PRD.0,
                1,             // mk.prd 1 → ref
                Opcode::DUP.0, // dup ref
                Opcode::ST_LOC.0,
                0, // save copy to local[0]
                Opcode::FRE.0,
                0, // free the ref on stack (2-byte instr)
                Opcode::LD_LOC.0,
                0, // load the saved ref
                Opcode::RET.0,
            ],
        )],
    );
    let (vm, result) = run_vm(&bytes);
    let result = result.expect("runs");
    let ptr = result.as_ref().expect("is ref");
    assert!(
        matches!(vm.heap().get(ptr), Err(VmError::FreedObject { .. })),
        "object should be freed"
    );
}

#[test]
fn test_fre_double_free_returns_error() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            0,
            vec![
                Opcode::LD_CST.0,
                0, // push 1
                Opcode::MK_PRD.0,
                1,             // mk.prd 1 → ref
                Opcode::DUP.0, // dup ref
                Opcode::ST_LOC.0,
                0,             // save copy
                Opcode::DUP.0, // dup again for second free
                Opcode::FRE.0,
                0, // first free
                Opcode::FRE.0,
                0, // second free — should error
                Opcode::LD_LOC.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(
                matches!(**source, VmError::FreedObject { .. }),
                "expected FreedObject, got {source:?}"
            );
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

// ── Tier 1: ALC_MAN ──────────────────────────────────────────────────────────

#[test]
fn test_alc_man_allocates_object() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::ALC_MAN.0,
                0,
                0,
                0,
                0, // alc.man type_id=0
                Opcode::RET.0,
            ],
        )],
    );
    let (vm, result) = run_vm(&bytes);
    let result = result.expect("runs");
    assert!(result.as_ref().is_ok(), "should be a ref");
    assert!(
        vm.heap().live_count() >= 1,
        "heap should have at least 1 object"
    );
}

// ── Tier 2: EFF_DO cross-frame ───────────────────────────────────────────────

/// Effect pool builder for tests.
struct EffectDef {
    id: u32,
    name_const_idx: u32,
    ops: Vec<EffectOpDef>,
}

struct EffectOpDef {
    id: u32,
    name_const_idx: u32,
}

/// Build a `.msbc` binary with an effect pool.
fn make_msbc_with_effects(consts: &[ConstEntry], effects: &[EffectDef], fns: &[FnDef]) -> Vec<u8> {
    let entry_fn_id: u32 = fns.first().map_or(0, |f| f.fn_id);

    // ── Const pool ────────────────────────────────────────────────────────────
    let mut const_section: Vec<u8> = vec![];
    let const_count = u32::try_from(consts.len()).expect("fits u32");
    const_section.extend_from_slice(&const_count.to_le_bytes());
    for c in consts {
        match c {
            ConstEntry::I32(v) => {
                const_section.push(0x01);
                const_section.extend_from_slice(&v.to_le_bytes());
            }
            ConstEntry::Str(bytes) => {
                const_section.push(0x05);
                let len = u32::try_from(bytes.len()).expect("fits u32");
                const_section.extend_from_slice(&len.to_le_bytes());
                const_section.extend_from_slice(bytes);
            }
        }
    }

    // ── Type pool (empty) ─────────────────────────────────────────────────────
    let type_section: Vec<u8> = 0u32.to_le_bytes().to_vec();

    // ── Effect pool ───────────────────────────────────────────────────────────
    let mut effect_section: Vec<u8> = vec![];
    let effect_count = u32::try_from(effects.len()).expect("fits u32");
    effect_section.extend_from_slice(&effect_count.to_le_bytes());
    for eff in effects {
        effect_section.extend_from_slice(&eff.id.to_le_bytes());
        effect_section.extend_from_slice(&eff.name_const_idx.to_le_bytes());
        let op_count = u16::try_from(eff.ops.len()).expect("fits u16");
        effect_section.extend_from_slice(&op_count.to_le_bytes());
        for op in &eff.ops {
            effect_section.extend_from_slice(&op.id.to_le_bytes());
            effect_section.extend_from_slice(&op.name_const_idx.to_le_bytes());
            effect_section.extend_from_slice(&0u16.to_le_bytes()); // param_count = 0
            effect_section.extend_from_slice(&0u32.to_le_bytes()); // ret_type_id = 0
        }
    }

    // ── Function pool ─────────────────────────────────────────────────────────
    let mut fn_section: Vec<u8> = vec![];
    let fn_count = u32::try_from(fns.len()).expect("fits u32");
    fn_section.extend_from_slice(&fn_count.to_le_bytes());
    for f in fns {
        fn_section.extend_from_slice(&f.fn_id.to_le_bytes());
        fn_section.extend_from_slice(&0u32.to_le_bytes());
        fn_section.extend_from_slice(&f.local_count.to_le_bytes());
        fn_section.extend_from_slice(&f.param_count.to_le_bytes());
        let max_stack: u16 = 16;
        fn_section.extend_from_slice(&max_stack.to_le_bytes());
        fn_section.extend_from_slice(&0u16.to_le_bytes());
        let code_len = u32::try_from(f.code.len()).expect("fits u32");
        fn_section.extend_from_slice(&code_len.to_le_bytes());
        fn_section.extend_from_slice(&f.code);
        let handler_count = u16::try_from(f.handlers.len()).expect("fits u16");
        fn_section.extend_from_slice(&handler_count.to_le_bytes());
        for &(eid, hfn) in &f.handlers {
            fn_section.push(eid);
            fn_section.extend_from_slice(&hfn.to_le_bytes());
        }
    }

    // ── Foreign pool (empty for tests) ──────────────────────────────────────
    let foreign_section: Vec<u8> = 0u32.to_le_bytes().to_vec(); // count = 0

    // ── Header ────────────────────────────────────────────────────────────────
    let header_size: u32 = 40;
    let const_off = header_size;
    let type_off = const_off + u32::try_from(const_section.len()).expect("fits u32");
    let effect_off = type_off + u32::try_from(type_section.len()).expect("fits u32");
    let foreign_off = effect_off + u32::try_from(effect_section.len()).expect("fits u32");
    let fn_off = foreign_off + u32::try_from(foreign_section.len()).expect("fits u32");

    let mut header: Vec<u8> = Vec::with_capacity(40);
    header.extend_from_slice(b"MUSI");
    header.extend_from_slice(&1u16.to_le_bytes());
    header.extend_from_slice(&0u16.to_le_bytes());
    header.extend_from_slice(&4u32.to_le_bytes());
    header.extend_from_slice(&entry_fn_id.to_le_bytes());
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&effect_off.to_le_bytes());
    header.extend_from_slice(&foreign_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());

    debug_assert_eq!(header.len(), 36);
    let checksum = crc32_slice(&header);
    header.extend_from_slice(&checksum.to_le_bytes());
    debug_assert_eq!(header.len(), 40);

    let mut out = header;
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&effect_section);
    out.extend_from_slice(&foreign_section);
    out.extend_from_slice(&fn_section);
    out
}

#[test]
fn test_eff_do_cross_frame_finds_handler() {
    let effect_id: u8 = 1;
    let bytes = make_msbc_with_effects(
        &[ConstEntry::I32(42)],
        &[EffectDef {
            id: 1,
            name_const_idx: 0,
            ops: vec![EffectOpDef {
                id: 1,
                name_const_idx: 0,
            }],
        }],
        &[
            FnDef {
                fn_id: 0,
                local_count: 0,
                param_count: 0,
                code: vec![
                    Opcode::EFF_PSH.0,
                    effect_id, // push handler for effect 1
                    Opcode::INV.0,
                    1,
                    0,
                    0,
                    0, // call fn 1
                    Opcode::RET.0,
                ],
                handlers: vec![(effect_id, 2)],
            },
            fn_def(
                1,
                0,
                0,
                vec![
                    Opcode::EFF_DO.0,
                    1,
                    0,
                    0,
                    0, // do effect op_id=1
                    Opcode::RET.0,
                ],
            ),
            fn_def(
                2,
                0,
                0,
                vec![
                    Opcode::LD_CST.0,
                    0, // push 42
                    Opcode::RET.0,
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_eff_res_resumes_continuation() {
    let effect_id: u8 = 1;
    let bytes = make_msbc_with_effects(
        &[ConstEntry::I32(99)],
        &[EffectDef {
            id: 1,
            name_const_idx: 0,
            ops: vec![EffectOpDef {
                id: 1,
                name_const_idx: 0,
            }],
        }],
        &[
            FnDef {
                fn_id: 0,
                local_count: 0,
                param_count: 0,
                code: vec![
                    Opcode::EFF_PSH.0,
                    effect_id,
                    Opcode::INV.0,
                    1,
                    0,
                    0,
                    0,
                    Opcode::RET.0,
                ],
                handlers: vec![(effect_id, 2)],
            },
            fn_def(1, 0, 0, vec![Opcode::EFF_DO.0, 1, 0, 0, 0, Opcode::RET.0]),
            fn_def(
                2,
                0,
                0,
                vec![
                    Opcode::LD_CST.0,
                    0, // push 99
                    Opcode::EFF_RES.0,
                    0,
                    0,
                    0,
                    0,               // resume with 99
                    Opcode::RET_U.0, // should not reach
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_eff_abt_after_eff_do() {
    let effect_id: u8 = 1;
    let bytes = make_msbc_with_effects(
        &[],
        &[EffectDef {
            id: 1,
            name_const_idx: 0,
            ops: vec![EffectOpDef {
                id: 1,
                name_const_idx: 0,
            }],
        }],
        &[
            FnDef {
                fn_id: 0,
                local_count: 0,
                param_count: 0,
                code: vec![
                    Opcode::EFF_PSH.0,
                    effect_id,
                    Opcode::INV.0,
                    1,
                    0,
                    0,
                    0,
                    Opcode::RET_U.0,
                ],
                handlers: vec![(effect_id, 2)],
            },
            fn_def(1, 0, 0, vec![Opcode::EFF_DO.0, 1, 0, 0, 0, Opcode::RET_U.0]),
            fn_def(
                2,
                0,
                0,
                vec![
                    Opcode::EFF_ABT.0, // abort
                    Opcode::RET_U.0,   // unreachable, but needed for verifier boundary
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(
                matches!(**source, VmError::EffectAborted),
                "expected EffectAborted, got {source:?}"
            );
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

#[test]
fn test_eff_res_c_is_noop() {
    let bytes = make_msbc(
        &[ConstEntry::I32(77)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::EFF_RES_C.0, 0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 77);
}

// ── Tier 3: Value task/chan tags ──────────────────────────────────────────────

#[test]
fn test_value_task_roundtrip() {
    let v = Value::from_task(42);
    assert_eq!(v.as_task_id().expect("is task"), 42);
    assert!(v.as_int().is_err());
    assert!(v.as_chan_id().is_err());
}

#[test]
fn test_value_chan_roundtrip() {
    let v = Value::from_chan(7);
    assert_eq!(v.as_chan_id().expect("is chan"), 7);
    assert!(v.as_int().is_err());
    assert!(v.as_task_id().is_err());
}

// ── Concurrency tests ────────────────────────────────────────────────────────

/// Build bytecode from a sequence of byte slices.
fn code(parts: &[&[u8]]) -> Vec<u8> {
    let mut out = Vec::new();
    for p in parts {
        out.extend_from_slice(p);
    }
    out
}

/// Encode a u32-operand opcode as a 5-byte slice.
fn op32(op: Opcode, operand: u32) -> [u8; 5] {
    let b = operand.to_le_bytes();
    [op.0, b[0], b[1], b[2], b[3]]
}

#[test]
fn test_spawn_returns_task_handle() {
    let child_code = vec![Opcode::LD_CST.0, 0, Opcode::RET.0];
    let entry_code = code(&[&op32(Opcode::TSK_SPN, 1), &[Opcode::RET.0]]);
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(0, 0, 0, entry_code), fn_def(1, 0, 0, child_code)],
    );
    let (_, result) = run_vm(&bytes);
    let v = result.expect("runs");
    assert_eq!(v.as_task_id().expect("is task"), 1);
}

#[test]
fn test_spawn_await_returns_child_value() {
    let child_code = vec![Opcode::LD_CST.0, 0, Opcode::RET.0];
    let entry_code = code(&[
        &op32(Opcode::TSK_SPN, 1),
        &[Opcode::TSK_AWT.0, 0],
        &[Opcode::RET.0],
    ]);
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(0, 0, 0, entry_code), fn_def(1, 0, 0, child_code)],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_channel_send_recv_fifo() {
    // fn 1 (sender): chan in local 0, sends 10, 20, 30
    let chs = op32(Opcode::TSK_CHS, 0);
    let sender_code = code(&[
        &[Opcode::LD_LOC.0, 0, Opcode::LD_CST.0, 0],
        &chs,
        &[Opcode::POP.0],
        &[Opcode::LD_LOC.0, 0, Opcode::LD_CST.0, 1],
        &chs,
        &[Opcode::POP.0],
        &[Opcode::LD_LOC.0, 0, Opcode::LD_CST.0, 2],
        &chs,
        &[Opcode::POP.0, Opcode::RET_U.0],
    ]);

    let cmk = op32(Opcode::TSK_CMK, 0);
    let spn = op32(Opcode::TSK_SPN, 1);
    let chr = op32(Opcode::TSK_CHR, 0);
    let entry_code = code(&[
        &cmk,
        &[Opcode::ST_LOC.0, 0],
        &[Opcode::LD_LOC.0, 0],
        &spn,
        &[Opcode::TSK_AWT.0, 0, Opcode::POP.0],
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::I_ADD.0],
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::I_ADD.0, Opcode::RET.0],
    ]);

    let bytes = make_msbc(
        &[
            ConstEntry::I32(10),
            ConstEntry::I32(20),
            ConstEntry::I32(30),
        ],
        &[
            fn_def(0, 1, 0, entry_code),
            FnDef {
                fn_id: 1,
                local_count: 1,
                param_count: 1,
                code: sender_code,
                handlers: vec![],
            },
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 60);
}

#[test]
fn test_channel_recv_empty_suspends_and_resumes() {
    let chr = op32(Opcode::TSK_CHR, 0);
    let recv_code = code(&[&[Opcode::LD_LOC.0, 0], &chr, &[Opcode::RET.0]]);

    let cmk = op32(Opcode::TSK_CMK, 0);
    let spn = op32(Opcode::TSK_SPN, 1);
    let chs = op32(Opcode::TSK_CHS, 0);
    let entry_code = code(&[
        &cmk,
        &[Opcode::ST_LOC.0, 0],
        &[Opcode::LD_LOC.0, 0],
        &spn,
        &[Opcode::ST_LOC.0, 1],
        &[Opcode::LD_LOC.0, 0, Opcode::LD_CST.0, 0],
        &chs,
        &[Opcode::POP.0],
        &[Opcode::LD_LOC.0, 1, Opcode::TSK_AWT.0, 0, Opcode::RET.0],
    ]);

    let bytes = make_msbc(
        &[ConstEntry::I32(99)],
        &[
            fn_def(0, 2, 0, entry_code),
            FnDef {
                fn_id: 1,
                local_count: 1,
                param_count: 1,
                code: recv_code,
                handlers: vec![],
            },
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_multiple_tasks_all_complete() {
    let fn1 = vec![Opcode::LD_CST.0, 0, Opcode::RET.0];
    let fn2 = vec![Opcode::LD_CST.0, 1, Opcode::RET.0];
    let fn3 = vec![Opcode::LD_CST.0, 2, Opcode::RET.0];

    let entry_code = code(&[
        &op32(Opcode::TSK_SPN, 1),
        &[Opcode::ST_LOC.0, 0],
        &op32(Opcode::TSK_SPN, 2),
        &[Opcode::ST_LOC.0, 1],
        &op32(Opcode::TSK_SPN, 3),
        &[Opcode::ST_LOC.0, 2],
        &[Opcode::LD_LOC.0, 0, Opcode::TSK_AWT.0, 0],
        &[Opcode::LD_LOC.0, 1, Opcode::TSK_AWT.0, 0],
        &[Opcode::I_ADD.0],
        &[Opcode::LD_LOC.0, 2, Opcode::TSK_AWT.0, 0],
        &[Opcode::I_ADD.0, Opcode::RET.0],
    ]);

    let bytes = make_msbc(
        &[
            ConstEntry::I32(10),
            ConstEntry::I32(20),
            ConstEntry::I32(30),
        ],
        &[
            fn_def(0, 3, 0, entry_code),
            fn_def(1, 0, 0, fn1),
            fn_def(2, 0, 0, fn2),
            fn_def(3, 0, 0, fn3),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 60);
}

#[test]
fn test_deadlock_two_tasks_mutual_await() {
    let chr = op32(Opcode::TSK_CHR, 0);
    let child_code = code(&[&[Opcode::LD_LOC.0, 0], &chr, &[Opcode::RET.0]]);
    let entry_code = code(&[
        &op32(Opcode::TSK_CMK, 0),
        &op32(Opcode::TSK_SPN, 1),
        &[Opcode::TSK_AWT.0, 0, Opcode::RET.0],
    ]);

    let bytes = make_msbc(
        &[],
        &[
            fn_def(0, 0, 0, entry_code),
            FnDef {
                fn_id: 1,
                local_count: 1,
                param_count: 1,
                code: child_code,
                handlers: vec![],
            },
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert!(
        matches!(&result, Err(VmError::Runtime { source, .. }) if matches!(**source, VmError::Deadlock)),
        "expected Deadlock, got {result:?}"
    );
}

#[test]
fn test_gc_traces_suspended_task_stacks() {
    // Spawn + await completes normally. Trigger GC mid-execution to verify
    // suspended task stacks are traced without panicking.
    let child_code = vec![Opcode::LD_CST.0, 0, Opcode::RET.0];
    let entry_code = code(&[
        &op32(Opcode::TSK_SPN, 1),
        &[Opcode::TSK_AWT.0, 0, Opcode::RET.0],
    ]);

    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(0, 0, 0, entry_code), fn_def(1, 0, 0, child_code)],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);

    // Also verify GC on a fresh VM with scheduler active
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    vm.setup_call(0, &[]).expect("setup ok");
    let freed = vm.collect_garbage();
    assert_eq!(freed, 0);
}

#[test]
fn test_sync_program_no_scheduler_overhead() {
    let bytes = make_msbc(
        &[ConstEntry::I32(7)],
        &[fn_def(0, 0, 0, vec![Opcode::LD_CST.0, 0, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}
