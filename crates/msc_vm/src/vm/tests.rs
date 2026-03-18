//! VM integration tests.
//!
//! All tests build minimal `.msbc` binaries from raw bytes - no compiler
//! crate dependency.
#![allow(clippy::panic)]

use std::iter;

use msc_bc::{self, Opcode, crc32_slice};

use crate::error::VmError;
use crate::loader::load;
use crate::value::Value;
use crate::verifier::verify;
use crate::vm::{StepResult, Vm};

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
    max_stack: Option<u16>,
}

/// Convenience: function def without handlers.
fn fn_def(fn_id: u32, local_count: u16, param_count: u16, code: Vec<u8>) -> FnDef {
    FnDef {
        fn_id,
        local_count,
        param_count,
        code,
        handlers: vec![],
        max_stack: None,
    }
}

/// Convenience: function def with an explicit `max_stack` limit.
fn fn_def_with_max_stack(
    fn_id: u32,
    local_count: u16,
    param_count: u16,
    max_stack: u16,
    code: Vec<u8>,
) -> FnDef {
    FnDef {
        fn_id,
        local_count,
        param_count,
        code,
        handlers: vec![],
        max_stack: Some(max_stack),
    }
}

/// Build a minimal valid `.msbc` binary.
fn make_msbc(consts: &[ConstEntry], fns: &[FnDef]) -> Vec<u8> {
    let entry_fn_id: u32 = fns.first().map_or(0, |f| f.fn_id);

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

    let type_section: Vec<u8> = 0u32.to_le_bytes().to_vec();
    let effect_section: Vec<u8> = 0u32.to_le_bytes().to_vec();

    let mut fn_section: Vec<u8> = vec![];
    let fn_count = u32::try_from(fns.len()).expect("fits u32");
    fn_section.extend_from_slice(&fn_count.to_le_bytes());
    for f in fns {
        fn_section.extend_from_slice(&f.fn_id.to_le_bytes());
        fn_section.extend_from_slice(&0u32.to_le_bytes()); // type_id
        fn_section.extend_from_slice(&f.local_count.to_le_bytes());
        fn_section.extend_from_slice(&f.param_count.to_le_bytes());
        let max_stack: u16 = f.max_stack.unwrap_or(16);
        fn_section.extend_from_slice(&max_stack.to_le_bytes());
        fn_section.extend_from_slice(&0u16.to_le_bytes()); // effect_mask
        fn_section.extend_from_slice(&0u16.to_le_bytes()); // upvalue_count
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

    let foreign_section: Vec<u8> = 0u32.to_le_bytes().to_vec(); // count = 0

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

#[test]
fn test_load_valid_header_succeeds() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_UT.0])]);
    let result = load(&bytes);
    assert!(result.is_ok(), "expected Ok, got {result:?}");
}

#[test]
fn test_load_bad_magic_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_UT.0])]);
    bytes[0] = b'X';
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadMagic)),
        "expected BadMagic, got {result:?}"
    );
}

#[test]
fn test_load_bad_checksum_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![Opcode::RET_UT.0])]);
    bytes[8] ^= 0xFF;
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadChecksum)),
        "expected BadChecksum, got {result:?}"
    );
}

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
                Opcode::INT_ADD.0,
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
                Opcode::JNF.0,
                2,
                0,
                0,
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
        Opcode::JNF.0,
        3,
        0,
        0,
        0,
        Opcode::LD_CST.0,
        0,
        Opcode::RET.0,
        Opcode::LD_LOC.0,
        0,
        Opcode::LD_CST.0,
        1,
        Opcode::INT_SUB.0,
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
                0x01, // packed LE lo: (7 << 8 | 1) = 0x0701
                0x07, // packed LE hi
                Opcode::CMP_TAG.0,
                7,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_value_nan_boxing_roundtrip() {
    type Case = (Value, fn(Value) -> bool);
    let cases: &[Case] = &[
        (Value::from_int(-1), |v| v.as_int().is_ok()),
        (Value::from_int(i64::MAX >> 16), |v| v.as_int().is_ok()),
        (Value::from_nat(0xDEAD_BEEF), |v| v.as_nat().is_ok()),
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
fn test_nan_handling() {
    let v = Value::from_float(f64::NAN);
    assert!(v.is_float());
    assert!(v.as_float().unwrap().is_nan());

    // All NaN variants are Float.
    let neg_nan = Value::from_float(f64::from_bits(0xFFF8_0000_0000_0000));
    assert!(neg_nan.as_float().unwrap().is_nan());

    let snan = Value::from_float(f64::from_bits(0x7FF0_0000_0000_0002));
    assert!(snan.as_float().unwrap().is_nan());

    // Non-NaN floats are untouched.
    let normal = Value::from_float(1.5);
    assert!(normal.is_float());
    assert!(!normal.as_float().unwrap().is_nan());

    // Infinity is NOT NaN.
    let inf = Value::from_float(f64::INFINITY);
    assert!(inf.is_float());
    assert!(!inf.as_float().unwrap().is_nan());
}

#[test]
fn test_value_int_sign_extension() {
    let v = Value::from_int(-1);
    assert_eq!(v.as_int().expect("is int"), -1);
    let v2 = Value::from_int(-42);
    assert_eq!(v2.as_int().expect("is int"), -42);
}

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
                Opcode::INT_DIV.0,
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
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::FLT_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 5.0).abs() < f64::EPSILON);
}

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
                Opcode::BIT_AND.0,
                Opcode::LD_CST.0,
                2,
                Opcode::BIT_SHL.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 240);
}

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
                Opcode::CNV_FTI.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

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
                0, // mk.arr type_id=0 -> ref on stack
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
                0, // mk.arr -> ref
                Opcode::LD_LEN.0,
                0, // push length (2-byte instr)
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 5);
}

#[test]
fn test_stack_underflow_returns_error() {
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, 0, vec![Opcode::INT_ADD.0, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.is_err(), "i.add on empty stack should error");
}

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

#[test]
fn test_instruction_limit_exceeded() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::JMP.0,
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
                Opcode::INT_DIV.0,
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
                1,             // mk.prd 1 -> ref (heap object)
                Opcode::POP.0, // discard the ref - object is now unreachable
                Opcode::LD_CST.0,
                0, // push 1 (keep something reachable)
                Opcode::MK_PRD.0,
                1, // mk.prd 1 -> ref (heap object, reachable)
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
                1, // mk.prd 1 -> ref
                Opcode::ST_GLB.0,
                0,
                0,
                0,
                0, // store to global[0]
                Opcode::RET_UT.0,
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
                    0,
                    1,
                    0,
                    0, // pack_id_arity(1, 0) = 0x100 LE
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
                    Opcode::INT_ADD.0,
                    Opcode::RET.0,
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 15);
}

#[test]
fn test_wide_jump() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::JMP.0,
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

#[test]
fn test_int_negation() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::INT_NEG.0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), -42);
}

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
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::FLT_MUL.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 12.0).abs() < f64::EPSILON);
}

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

#[test]
fn test_value_try_as_ref() {
    assert!(Value::from_ref(42).try_as_ref().is_some());
    assert_eq!(Value::from_ref(42).try_as_ref(), Some(42));
    assert!(Value::from_int(42).try_as_ref().is_none());
    assert!(Value::UNIT.try_as_ref().is_none());
}

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

    let type_section: Vec<u8> = 0u32.to_le_bytes().to_vec();

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

    let mut fn_section: Vec<u8> = vec![];
    let fn_count = u32::try_from(fns.len()).expect("fits u32");
    fn_section.extend_from_slice(&fn_count.to_le_bytes());
    for f in fns {
        fn_section.extend_from_slice(&f.fn_id.to_le_bytes());
        fn_section.extend_from_slice(&0u32.to_le_bytes());
        fn_section.extend_from_slice(&f.local_count.to_le_bytes());
        fn_section.extend_from_slice(&f.param_count.to_le_bytes());
        let max_stack: u16 = f.max_stack.unwrap_or(16);
        fn_section.extend_from_slice(&max_stack.to_le_bytes());
        fn_section.extend_from_slice(&0u16.to_le_bytes()); // effect_mask
        fn_section.extend_from_slice(&0u16.to_le_bytes()); // upvalue_count
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

    let foreign_section: Vec<u8> = 0u32.to_le_bytes().to_vec(); // count = 0

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
                    Opcode::CNT_MRK.0,
                    effect_id, // push handler for effect 1
                    Opcode::INV.0,
                    0,
                    1,
                    0,
                    0, // pack_id_arity(1, 0) = 0x100 LE; call fn 1
                    Opcode::RET.0,
                ],
                handlers: vec![(effect_id, 2)],
                max_stack: None,
            },
            fn_def(
                1,
                0,
                0,
                vec![
                    Opcode::CNT_SAV.0,
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
                    Opcode::CNT_MRK.0,
                    effect_id,
                    Opcode::INV.0,
                    0,
                    1,
                    0,
                    0, // pack_id_arity(1, 0) = 0x100 LE
                    Opcode::RET.0,
                ],
                handlers: vec![(effect_id, 2)],
                max_stack: None,
            },
            fn_def(1, 0, 0, vec![Opcode::CNT_SAV.0, 1, 0, 0, 0, Opcode::RET.0]),
            fn_def(
                2,
                0,
                0,
                vec![
                    Opcode::LD_CST.0,
                    0, // push 99
                    Opcode::CNT_RSM.0,
                    0,
                    0,
                    0,
                    0,                // resume with 99
                    Opcode::RET_UT.0, // should not reach
                ],
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

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

/// Build bytecode from a sequence of byte slices.
fn code(parts: &[&[u8]]) -> Vec<u8> {
    let mut out = vec![];
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
        &[Opcode::TSK_AWT.0],
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
        &[Opcode::POP.0, Opcode::RET_UT.0],
    ]);

    let cmk = op32(Opcode::TSK_CMK, 0);
    let spn = op32(Opcode::TSK_SPN, 1);
    let chr = op32(Opcode::TSK_CHR, 0);
    let entry_code = code(&[
        &cmk,
        &[Opcode::ST_LOC.0, 0],
        &[Opcode::LD_LOC.0, 0],
        &spn,
        &[Opcode::TSK_AWT.0, Opcode::POP.0],
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::INT_ADD.0],
        &[Opcode::LD_LOC.0, 0],
        &chr,
        &[Opcode::INT_ADD.0, Opcode::RET.0],
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
                max_stack: None,
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
        &[Opcode::LD_LOC.0, 1, Opcode::TSK_AWT.0, Opcode::RET.0],
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
                max_stack: None,
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
        &[Opcode::LD_LOC.0, 0, Opcode::TSK_AWT.0],
        &[Opcode::LD_LOC.0, 1, Opcode::TSK_AWT.0],
        &[Opcode::INT_ADD.0],
        &[Opcode::LD_LOC.0, 2, Opcode::TSK_AWT.0],
        &[Opcode::INT_ADD.0, Opcode::RET.0],
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
        &[Opcode::TSK_AWT.0, Opcode::RET.0],
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
                max_stack: None,
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
        &[Opcode::TSK_AWT.0, Opcode::RET.0],
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

#[test]
fn test_cmp_le_true_when_equal() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5), ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CMP_LE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_lt_true() {
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(5)],
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
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_gt_true() {
    let bytes = make_msbc(
        &[ConstEntry::I32(7), ConstEntry::I32(3)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CMP_GT.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_f_eq_true() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5), ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::CMP_EQ.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_make_variant_multi_field_via_mk_prd_field_0() {
    // MK_PRD wraps two values into a product. MK_VAR stores the product ref
    // as the variant's payload (fields[0]). LD_PAY 0 extracts the product ref,
    // then LD_FLD 0 reads field 0 of the product.
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
                Opcode::MK_VAR.0,
                0x01, // packed LE lo: (1 << 8 | 1) = 0x0101
                0x01, // packed LE hi
                Opcode::LD_PAY.0,
                0, // extract payload (product ref)
                Opcode::LD_FLD.0,
                0, // field 0 of the product
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 10);
}

#[test]
fn test_make_variant_multi_field_via_mk_prd_field_1() {
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
                Opcode::MK_VAR.0,
                0x01, // packed LE lo: (1 << 8 | 1) = 0x0101
                0x01, // packed LE hi
                Opcode::LD_PAY.0,
                0, // extract payload (product ref)
                Opcode::LD_FLD.0,
                1, // field 1 of the product
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 20);
}

#[test]
fn test_verifier_accepts_max_stack_exact_match() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(2)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            2,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::INT_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(
        result.is_ok(),
        "max_stack=2 should pass when peak is 2, got {result:?}"
    );
}

#[test]
fn test_verifier_rejects_max_stack_exceeded_by_one() {
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(2)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            1,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::INT_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(result.is_err(), "max_stack=1 should fail when peak is 2");
}

#[test]
fn test_verifier_resets_depth_after_unconditional_jump() {
    // offset 0: LD_CST 0  (2)  depth -> 1
    // offset 2: JMP_W +0  (5)  jumps to offset 7; depth resets to 0
    // offset 7: RET_U     (1)  depth 0
    // max_stack=1 matches peak reachable depth of 1.
    let code = vec![
        Opcode::LD_CST.0,
        0,
        Opcode::JMP.0,
        0,
        0,
        0,
        0,
        Opcode::RET_UT.0,
    ];
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def_with_max_stack(0, 0, 0, 1, code)],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(
        result.is_ok(),
        "depth must reset after JMP_W, got {result:?}"
    );
}

#[test]
fn test_verifier_depth_resets_to_zero_after_terminator() {
    let code = vec![
        Opcode::LD_CST.0,
        0,
        Opcode::RET.0,
        Opcode::LD_CST.0,
        0,
        Opcode::RET.0,
    ];
    let bytes = make_msbc(
        &[ConstEntry::I32(1)],
        &[fn_def_with_max_stack(0, 0, 0, 1, code)],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(result.is_ok(), "depth must reset after RET, got {result:?}");
}

#[test]
fn test_ld_loc_w_loads_high_slot() {
    let code = vec![
        Opcode::LD_CST.0,
        0,
        msc_bc::Opcode::WID,
        Opcode::ST_LOC.0,
        0x00,
        0x01,
        msc_bc::Opcode::WID,
        Opcode::LD_LOC.0,
        0x00,
        0x01,
        Opcode::RET.0,
    ];
    let bytes = make_msbc(&[ConstEntry::I32(99)], &[fn_def(0, 300, 0, code)]);
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_st_loc_w_stores_high_slot() {
    let code = vec![
        Opcode::LD_CST.0,
        0,
        msc_bc::Opcode::WID,
        Opcode::ST_LOC.0,
        0x10,
        0x01,
        msc_bc::Opcode::WID,
        Opcode::LD_LOC.0,
        0x10,
        0x01,
        Opcode::RET.0,
    ];
    let bytes = make_msbc(&[ConstEntry::I32(77)], &[fn_def(0, 300, 0, code)]);
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 77);
}

#[test]
fn test_cmp_tag_w_matches_large_tag() {
    let bytes = make_msbc(
        &[ConstEntry::I32(0)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::MK_VAR.0,
                0x01, // packed LE lo: (7 << 8 | 1) = 0x0701
                0x07, // packed LE hi
                msc_bc::Opcode::WID,
                Opcode::CMP_TAG.0,
                7,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_f_sub() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(3)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::FLT_SUB.0,
                Opcode::CNV_FTI.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

#[test]
fn test_f_neg() {
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
                Opcode::FLT_NEG.0,
                Opcode::CNV_FTI.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), -7);
}

#[test]
fn test_b_or() {
    let bytes = make_msbc(
        &[ConstEntry::I32(0b1010), ConstEntry::I32(0b0110)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::BIT_OR.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0b1110);
}

#[test]
fn test_b_xor() {
    let bytes = make_msbc(
        &[ConstEntry::I32(0b1010), ConstEntry::I32(0b0110)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::BIT_XOR.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0b1100);
}

#[test]
fn test_b_not() {
    let bytes = make_msbc(
        &[ConstEntry::I32(0)],
        &[fn_def(
            0,
            0,
            0,
            vec![Opcode::LD_CST.0, 0, Opcode::BIT_NOT.0, Opcode::RET.0],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), -1);
}

#[test]
fn test_b_shr() {
    let bytes = make_msbc(
        &[ConstEntry::I32(16), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::BIT_SHR.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 4);
}

#[test]
fn test_ld_tag_returns_tag_value() {
    let bytes = make_msbc(
        &[ConstEntry::I32(0)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::MK_VAR.0,
                0x01, // packed LE lo: (3 << 8 | 1) = 0x0301
                0x03, // packed LE hi
                Opcode::LD_TAG.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 3);
}

#[test]
fn test_ld_pay_extracts_variant_payload() {
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
                0x01, // packed LE lo: (0 << 8 | 1) = 0x0001
                0x00, // packed LE hi
                Opcode::LD_PAY.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

// ── New arithmetic / comparison / control tests ─────────────────────────────

#[test]
fn test_int_rem() {
    // 7 % 3 = 1 - INT_REM is zone 0 (0x14), no operand
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
                Opcode::INT_REM.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(7), Value::from_int(3)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 1);
}

#[test]
fn test_nat_add() {
    // 3 + 4 = 7 - NAT_ADD is zone 0 (0x18), no operand
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
                Opcode::NAT_ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(3), Value::from_nat(4)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 7);
}

#[test]
fn test_nat_sub() {
    // 10 - 3 = 7
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
                Opcode::NAT_SUB.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(10), Value::from_nat(3)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 7);
}

#[test]
fn test_nat_mul() {
    // 3 * 4 = 12
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
                Opcode::NAT_MUL.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(3), Value::from_nat(4)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 12);
}

#[test]
fn test_nat_div() {
    // 10 / 3 = 3
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
                Opcode::NAT_DIV.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(10), Value::from_nat(3)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 3);
}

#[test]
fn test_nat_rem() {
    // 10 % 3 = 1
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
                Opcode::NAT_REM.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(10), Value::from_nat(3)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 1);
}

#[test]
fn test_bit_sru() {
    // Unsigned shift right: 16 >> 2 = 4 - BIT_SRU requires nat operands
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
                Opcode::BIT_SRU.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(16), Value::from_nat(2)]);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 4);
}

#[test]
fn test_cmp_ne() {
    // 5 != 3 = true
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
                Opcode::CMP_NE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_ge() {
    // 5 >= 5 = true
    let bytes = make_msbc(
        &[ConstEntry::I32(5), ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::CMP_GE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_ltu() {
    // unsigned 3 < 5 = true
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
                Opcode::CMP_LTU.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(3), Value::from_nat(5)]);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_leu() {
    // unsigned 5 <= 5 = true
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
                Opcode::CMP_LEU.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(5), Value::from_nat(5)]);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_gtu() {
    // unsigned 5 > 3 = true
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
                Opcode::CMP_GTU.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(5), Value::from_nat(3)]);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_geu() {
    // unsigned 5 >= 5 = true
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
                Opcode::CMP_GEU.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_nat(5), Value::from_nat(5)]);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_flt() {
    // 1.0 < 2.0 = true - load int constants, convert to float, compare
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::CMP_FLT.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_fle() {
    // 2.0 <= 2.0 = true
    let bytes = make_msbc(
        &[ConstEntry::I32(2), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::CMP_FLE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_fgt() {
    // 3.0 > 2.0 = true
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
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::CMP_FGT.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_fge() {
    // 2.0 >= 2.0 = true
    let bytes = make_msbc(
        &[ConstEntry::I32(2), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::CMP_FGE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_flt_div() {
    // 10.0 / 2.0 = 5.0 - convert back to int via CNV_FTI
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::FLT_DIV.0,
                Opcode::CNV_FTI.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 5);
}

#[test]
fn test_flt_rem() {
    // 7.0 % 3.0 = 1.0 - convert back to int via CNV_FTI
    let bytes = make_msbc(
        &[ConstEntry::I32(7), ConstEntry::I32(3)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::CNV_ITF.0,
                Opcode::LD_CST.0,
                1,
                Opcode::CNV_ITF.0,
                Opcode::FLT_REM.0,
                Opcode::CNV_FTI.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 1);
}

#[test]
fn test_swp() {
    // Push 10, push 20, SWP - stack becomes [20(bottom), 10(top)]. RET returns 10.
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
                Opcode::SWP.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 10);
}

#[test]
fn test_ld_ut() {
    // LD_UT pushes unit; RET returns it.
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, 0, vec![Opcode::LD_UT.0, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").is_unit());
}

#[test]
fn test_jif_long() {
    // JIF (long, zone 3 = 5 bytes) forward past a HLT, then load 42 and return.
    // Layout:
    //   offset 0: ld.cst 0       (2 bytes) - push 1 (truthy)
    //   offset 2: jif +1,0,0,0   (5 bytes) - ip after = 7, target = 7+1 = 8
    //   offset 7: hlt             (1 byte)  - skipped
    //   offset 8: ld.cst 1       (2 bytes) - push 42
    //   offset 10: ret            (1 byte)
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::JIF.0,
                1,
                0,
                0,
                0,
                Opcode::HLT.0,
                Opcode::LD_CST.0,
                1,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_jmp_sh_backward() {
    // Countdown loop: start with local 0 = 3, decrement until 0, return 0.
    // consts: 0 = I32(0), 1 = I32(1)
    //
    // Layout (LOOP starts at offset 0):
    //   offset  0: ld.loc 0        (2 bytes)
    //   offset  2: ld.cst 0        (2 bytes) - push 0
    //   offset  4: cmp.eq          (1 byte)  - counter == 0?
    //   offset  5: jif +9,0,0,0    (5 bytes) - ip after = 10, target = 10+9 = 19 (EXIT)
    //   offset 10: ld.loc 0        (2 bytes)
    //   offset 12: ld.cst 1        (2 bytes) - push 1
    //   offset 14: int.sub         (1 byte)
    //   offset 15: st.loc 0        (2 bytes)
    //   offset 17: jmp.sh -19      (2 bytes) - ip after = 19, target = 19 + (-19) = 0
    // EXIT:
    //   offset 19: ld.loc 0        (2 bytes)
    //   offset 21: ret             (1 byte)
    let bytes = make_msbc(
        &[ConstEntry::I32(0), ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            1,
            vec![
                // LOOP:
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_CST.0,
                0,
                Opcode::CMP_EQ.0,
                Opcode::JIF.0,
                9,
                0,
                0,
                0,
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::INT_SUB.0,
                Opcode::ST_LOC.0,
                0,
                Opcode::JMP_SH.0,
                (-19i8).cast_unsigned(),
                // EXIT:
                Opcode::LD_LOC.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(3)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0);
}

#[test]
fn test_jif_sh_forward() {
    // JIF_SH forward: push truthy value, skip over HLT, load 42 and return.
    // consts: 0 = I32(1), 1 = I32(42)
    //
    // Layout:
    //   offset 0: ld.cst 0    (2 bytes) - push 1 (truthy)
    //   offset 2: jif.sh +1   (2 bytes) - ip after = 4, target = 4+1 = 5
    //   offset 4: hlt         (1 byte)  - skipped
    //   offset 5: ld.cst 1    (2 bytes) - push 42
    //   offset 7: ret         (1 byte)
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::JIF_SH.0,
                1,
                Opcode::HLT.0,
                Opcode::LD_CST.0,
                1,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_jnf_sh_backward() {
    // JNF_SH backward loop: decrement counter, loop while non-zero, return 0.
    // consts: 0 = I32(0), 1 = I32(1)
    //
    // Layout (LOOP starts at offset 0):
    //   offset  0: ld.loc 0    (2 bytes)
    //   offset  2: ld.cst 1    (2 bytes) - push 1
    //   offset  4: int.sub     (1 byte)  - counter - 1
    //   offset  5: st.loc 0    (2 bytes)
    //   offset  7: ld.loc 0    (2 bytes)
    //   offset  9: ld.cst 0    (2 bytes) - push 0
    //   offset 11: cmp.eq      (1 byte)  - counter == 0?
    //   offset 12: jnf.sh -14  (2 bytes) - ip after = 14, not-zero → target = 14+(-14) = 0
    // EXIT:
    //   offset 14: ld.loc 0    (2 bytes)
    //   offset 16: ret         (1 byte)
    let bytes = make_msbc(
        &[ConstEntry::I32(0), ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            1,
            vec![
                // LOOP:
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::INT_SUB.0,
                Opcode::ST_LOC.0,
                0,
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_CST.0,
                0,
                Opcode::CMP_EQ.0,
                Opcode::JNF_SH.0,
                (-14i8).cast_unsigned(),
                // EXIT:
                Opcode::LD_LOC.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(3)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0);
}

#[test]
fn test_inv_dyn() {
    // fn 0: create closure over fn 1 (0 upvals), call it dynamically with 0 args.
    // fn 1: push 42 and return.
    //
    // MK_CLO packed operand = (fn_id_u24 << 8) | upval_count_u8
    //   fn_id=1, upval_count=0 → 0x00000100 LE = [0x00, 0x01, 0x00, 0x00]
    // INV_DYN zone 1 (0x4A), operand = 0 args.
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[
            fn_def(
                0,
                0,
                0,
                vec![
                    Opcode::MK_CLO.0,
                    0x00,
                    0x01,
                    0x00,
                    0x00,
                    Opcode::INV_DYN.0,
                    0,
                    Opcode::RET.0,
                ],
            ),
            fn_def(1, 0, 0, vec![Opcode::LD_CST.0, 0, Opcode::RET.0]),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_st_fld() {
    // Create a product {10, 20}, store 99 into field 1, load field 1 → 99.
    // MK_PRD zone 1 (0x48), operand = field count (2).
    // ST_FLD zone 1 (0x44): pops val then obj_ref.
    // LD_FLD zone 1 (0x43): pops obj_ref, pushes field value.
    //
    // consts: 0=10, 1=20, 2=99
    let bytes = make_msbc(
        &[
            ConstEntry::I32(10),
            ConstEntry::I32(20),
            ConstEntry::I32(99),
        ],
        &[fn_def(
            0,
            1,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::LD_CST.0,
                1,
                Opcode::MK_PRD.0,
                2,
                Opcode::ST_LOC.0,
                0,
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_CST.0,
                2,
                Opcode::ST_FLD.0,
                1,
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_FLD.0,
                1,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_mk_clo_ld_upv() {
    // fn 0: load param (= 42), create closure over fn 1 capturing it, call it.
    // fn 1: ld.upv 0, ret → returns the captured value.
    //
    // MK_CLO packed: fn_id=1, upval_count=1 → (1 << 8) | 1 = 0x101
    //   LE bytes: [0x01, 0x01, 0x00, 0x00]
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[
            fn_def(
                0,
                1,
                1,
                vec![
                    Opcode::LD_LOC.0,
                    0,
                    Opcode::MK_CLO.0,
                    0x01,
                    0x01,
                    0x00,
                    0x00,
                    Opcode::INV_DYN.0,
                    0,
                    Opcode::RET.0,
                ],
            ),
            fn_def(1, 0, 0, vec![Opcode::LD_UPV.0, 0, Opcode::RET.0]),
        ],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(42)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_alc_ref() {
    // ALC_REF pops initial value, allocates a ref cell, pushes the ref.
    // LD_FLD 0 reads the single stored field back.
    // ALC_REF is zone 3 (0xCB), operand = type_id (use 0).
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                Opcode::LD_CST.0,
                0,
                Opcode::ALC_REF.0,
                0,
                0,
                0,
                0,
                Opcode::LD_FLD.0,
                0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}
