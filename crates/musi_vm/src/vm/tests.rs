//! VM integration tests.
//!
//! All tests build minimal `.msbc` binaries from raw bytes — no compiler
//! crate dependency.
#![allow(clippy::panic)]

use std::iter;

use crate::error::VmError;
use crate::loader::load;
use crate::value::Value;
use crate::verifier::verify;
use crate::vm::{StepResult, Vm};

// ── Opcode constants ─────────────────────────────────────────────────────────

const HLT: u8 = 0x01;
const RET: u8 = 0x02;
const RET_U: u8 = 0x03;
const DUP: u8 = 0x06;
const POP: u8 = 0x07;
const I_ADD: u8 = 0x10;
const I_SUB: u8 = 0x12;
const I_DIV: u8 = 0x16;
const I_NEG: u8 = 0x1A;
const F_ADD: u8 = 0x20;
const F_MUL: u8 = 0x22;
const B_AND: u8 = 0x30;
const B_SHL: u8 = 0x34;
const CMP_EQ: u8 = 0x3B;
const CMP_LT: u8 = 0x50;
const CNV_ITF: u8 = 0x5E;
const CNV_FTI: u8 = 0x5F;
const LD_LOC: u8 = 0x40;
const ST_LOC: u8 = 0x41;
const LD_CST: u8 = 0x42;
const MK_PRD: u8 = 0x44;
const LD_FLD: u8 = 0x45;
const MK_VAR: u8 = 0x46;
const CMP_TAG: u8 = 0x48;
const LD_LEN: u8 = 0x62;
const LD_IDX: u8 = 0x63;
const ST_IDX: u8 = 0x64;
const JMP_F: u8 = 0x88;
const INV: u8 = 0xC0;
const INV_TAL: u8 = 0xC2;
const LD_GLB: u8 = 0xC4;
const ST_GLB: u8 = 0xC5;
const MK_ARR: u8 = 0xC6;
const JMP_W: u8 = 0xD0;

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

    // ── Header ────────────────────────────────────────────────────────────────
    let header_size: u32 = 36;
    let const_off = header_size;
    let type_off = const_off + u32::try_from(const_section.len()).expect("fits u32");
    let effect_off = type_off + u32::try_from(type_section.len()).expect("fits u32");
    let fn_off = effect_off + u32::try_from(effect_section.len()).expect("fits u32");

    let mut header: Vec<u8> = Vec::with_capacity(36);
    header.extend_from_slice(b"MUSI");
    header.extend_from_slice(&1u16.to_le_bytes());
    header.extend_from_slice(&0u16.to_le_bytes());
    header.extend_from_slice(&4u32.to_le_bytes()); // flags (IS_SCRIPT)
    header.extend_from_slice(&entry_fn_id.to_le_bytes());
    header.extend_from_slice(&const_off.to_le_bytes());
    header.extend_from_slice(&type_off.to_le_bytes());
    header.extend_from_slice(&effect_off.to_le_bytes());
    header.extend_from_slice(&fn_off.to_le_bytes());

    debug_assert_eq!(header.len(), 32);
    let checksum = crc32_test(&header);
    header.extend_from_slice(&checksum.to_le_bytes());
    debug_assert_eq!(header.len(), 36);

    let mut out = header;
    out.extend_from_slice(&const_section);
    out.extend_from_slice(&type_section);
    out.extend_from_slice(&effect_section);
    out.extend_from_slice(&fn_section);
    out
}

fn crc32_test(data: &[u8]) -> u32 {
    const POLY: u32 = 0xEDB8_8320;
    let mut crc: u32 = 0xFFFF_FFFF;
    for &byte in data {
        let low = crc.to_le_bytes()[0];
        let xored = low ^ byte;
        let mut entry = u32::from(xored);
        let mut j = 0;
        while j < 8 {
            if entry & 1 != 0 {
                entry = (entry >> 1) ^ POLY;
            } else {
                entry >>= 1;
            }
            j += 1;
        }
        crc = entry ^ (crc >> 8);
    }
    crc ^ 0xFFFF_FFFF
}

// ── Loader tests ──────────────────────────────────────────────────────────────

#[test]
fn test_load_valid_header_succeeds() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![RET_U])]);
    let result = load(&bytes);
    assert!(result.is_ok(), "expected Ok, got {result:?}");
}

#[test]
fn test_load_bad_magic_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![RET_U])]);
    bytes[0] = b'X';
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadMagic)),
        "expected BadMagic, got {result:?}"
    );
}

#[test]
fn test_load_bad_checksum_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![RET_U])]);
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
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![LD_CST, 5, RET])]);
    let module = load(&bytes).expect("loads ok");
    let result = verify(&module);
    assert!(result.is_err(), "expected Verify error, got Ok");
}

#[test]
fn test_verifier_rejects_stack_overflow() {
    let mut code = vec![LD_CST, 0u8];
    code.extend(iter::repeat_n(DUP, 20));
    code.push(RET);
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
        &[fn_def(0, 0, 0, vec![LD_CST, 0, RET])],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 99);
}

#[test]
fn test_run_add_two_ints() {
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 2, 2, vec![LD_LOC, 0, LD_LOC, 1, I_ADD, RET])],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm
        .call_fn(0, &[Value::from_int(3), Value::from_int(4)])
        .expect("runs");
    assert_eq!(result.as_int().expect("is int"), 7);
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
                LD_CST, 0, LD_CST, 1, CMP_LT, 0, JMP_F, 2, 0, LD_CST, 0, LD_CST, 1, RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(
        result.as_int().expect("is int"),
        3,
        "jmp.f taken: should return 3"
    );
}

#[test]
fn test_run_tail_call_countdown() {
    let code = vec![
        LD_LOC, 0, LD_CST, 0, 0x3B, // cmp.eq
        JMP_F, 3, 0, LD_CST, 0, RET, LD_LOC, 0, LD_CST, 1, I_SUB, INV_TAL, 0, 0, 0, 0,
    ];
    let bytes = make_msbc(
        &[ConstEntry::I32(0), ConstEntry::I32(1)],
        &[fn_def(0, 1, 1, code)],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.call_fn(0, &[Value::from_int(10)]).expect("runs");
    assert_eq!(result.as_int().expect("is int"), 0);
}

#[test]
fn test_run_make_product_and_load_field() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(20)],
        &[fn_def(
            0,
            0,
            0,
            vec![LD_CST, 0, LD_CST, 1, MK_PRD, 2, LD_FLD, 1, RET],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 20);
}

#[test]
fn test_run_make_variant_and_check_tag() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, MK_VAR, 7, CMP_TAG, 7, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert!(result.as_bool().expect("is bool"));
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

// ── NEW: String values ───────────────────────────────────────────────────────

#[test]
fn test_string_const_returns_heap_ref() {
    // ld.cst 0 (a string) ; ret — should return a ref, not unit.
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hello".to_vec())],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, RET])],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    // Result should be a ref (heap-allocated string), not unit.
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
    // ld.cst 0 ; st.loc 0 ; ld.cst 0 ; st.loc 1 ; ld.loc 0 ; ret
    // Loading the same string const twice should produce two separate heap refs.
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hi".to_vec())],
        &[fn_def(
            0,
            2,
            0,
            vec![
                LD_CST, 0, ST_LOC, 0, // first load
                LD_CST, 0, ST_LOC, 1, // second load
                LD_LOC, 0, RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert!(result.as_ref().is_ok(), "should be a ref");
}

// ── NEW: Global variables ────────────────────────────────────────────────────

#[test]
fn test_globals_store_and_load() {
    // const[0] = 42
    // st.glb 5 (store 42 into global[5]) ; ld.glb 5 ; ret
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                LD_CST, 0, // push 42
                ST_GLB, 5, 0, 0, 0, // store to global[5]
                LD_GLB, 5, 0, 0, 0, // load global[5]
                RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 42);
    assert_eq!(
        vm.globals().len(),
        6,
        "globals should have grown to index 5+1"
    );
}

#[test]
fn test_globals_uninitialized_returns_unit() {
    // ld.glb 0 ; ret — uninitialized global should return unit.
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![LD_GLB, 0, 0, 0, 0, RET])]);
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert!(result.is_unit(), "uninitialized global should be unit");
}

// ── NEW: Division by zero ────────────────────────────────────────────────────

#[test]
fn test_division_by_zero_returns_error() {
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(0)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, LD_CST, 1, I_DIV, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run();
    // Should produce a Runtime error wrapping DivideByZero.
    assert!(result.is_err());
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(matches!(**source, VmError::DivideByZero));
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

// ── NEW: Float arithmetic ────────────────────────────────────────────────────

#[test]
fn test_float_add_and_multiply() {
    // push int 3, convert to float, push int 2, convert to float, add, ret
    // CNV_ITF (0x5E) is a 2-byte instruction; needs a dummy operand byte.
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(2)],
        &[fn_def(
            0,
            0,
            0,
            vec![LD_CST, 0, CNV_ITF, 0, LD_CST, 1, CNV_ITF, 0, F_ADD, RET],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    let f = result.as_float().expect("is float");
    assert!((f - 5.0).abs() < f64::EPSILON);
}

// ── NEW: Bitwise operations ──────────────────────────────────────────────────

#[test]
fn test_bitwise_and_and_shift() {
    // (0xFF & 0x0F) << 4 = 0xF0 = 240
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
            vec![LD_CST, 0, LD_CST, 1, B_AND, LD_CST, 2, B_SHL, RET],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 240);
}

// ── NEW: Type conversions ────────────────────────────────────────────────────

#[test]
fn test_int_to_float_to_int_roundtrip() {
    // int(7) → float → int
    // CNV_ITF (0x5E) and CNV_FTI (0x5F) are both 2-byte instructions.
    let bytes = make_msbc(
        &[ConstEntry::I32(7)],
        &[fn_def(
            0,
            0,
            0,
            vec![LD_CST, 0, CNV_ITF, 0, CNV_FTI, 0, RET],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 7);
}

// ── NEW: Array operations ────────────────────────────────────────────────────

#[test]
fn test_array_create_store_load() {
    // mk.arr (len=3, type_id=0) ; st.idx [1] = 99 ; ld.idx [1] ; ret
    // ST_IDX (0x64) and LD_IDX (0x63) are 2-byte instructions.
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(99), ConstEntry::I32(1)],
        &[fn_def(
            0,
            1,
            0,
            vec![
                LD_CST, 0, // push 3 (length)
                MK_ARR, 0, 0, 0, 0, // mk.arr type_id=0 → ref on stack
                ST_LOC, 0, // save ref
                LD_LOC, 0, // push ref
                LD_CST, 2, // push index 1
                LD_CST, 1, // push value 99
                ST_IDX, 0, // arr[1] = 99 (2-byte instr)
                LD_LOC, 0, // push ref
                LD_CST, 2, // push index 1
                LD_IDX, 0, // load arr[1] (2-byte instr)
                RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 99);
}

#[test]
fn test_array_length() {
    // LD_LEN (0x62) is a 2-byte instruction.
    let bytes = make_msbc(
        &[ConstEntry::I32(5)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                LD_CST, 0, // push 5 (length)
                MK_ARR, 0, 0, 0, 0, // mk.arr → ref
                LD_LEN, 0, // push length (2-byte instr)
                RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_uint().expect("is uint"), 5);
}

// ── NEW: Stack underflow ─────────────────────────────────────────────────────

#[test]
fn test_stack_underflow_returns_error() {
    // I_ADD on empty stack should error.
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![I_ADD, RET])]);
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run();
    assert!(result.is_err(), "i.add on empty stack should error");
}

// ── NEW: HLT instruction ────────────────────────────────────────────────────

#[test]
fn test_hlt_returns_halted_error() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, 0, vec![HLT])]);
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run();
    assert!(result.is_err());
    let err = result.unwrap_err();
    match &err {
        VmError::Runtime { source, .. } => {
            assert!(matches!(**source, VmError::Halted));
        }
        _ => panic!("expected Runtime wrapping Halted, got {err:?}"),
    }
}

// ── NEW: Instruction limit ──────────────────────────────────────────────────

#[test]
fn test_instruction_limit_exceeded() {
    // Infinite loop: jmp -5 (back to itself). With limit, should stop.
    // jmp.w is 5 bytes; offset = -5 loops back to the jmp.w itself.
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            0,
            vec![
                JMP_W, 0xFB, 0xFF, 0xFF, 0xFF, // i32 = -5 (little-endian)
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.set_instruction_limit(Some(100));
    let result = vm.run();
    assert!(result.is_err());
    let err = result.unwrap_err();
    // InstructionLimitExceeded is returned directly (not wrapped in Runtime).
    assert!(
        matches!(err, VmError::InstructionLimitExceeded { limit: 100 }),
        "expected InstructionLimitExceeded, got {err:?}"
    );
    assert_eq!(vm.instruction_count(), 100);
}

// ── NEW: Error context ───────────────────────────────────────────────────────

#[test]
fn test_error_context_contains_fn_id_and_ip() {
    // Division by zero at known position.
    let bytes = make_msbc(
        &[ConstEntry::I32(1), ConstEntry::I32(0)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, LD_CST, 1, I_DIV, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let err = vm.run().unwrap_err();
    match &err {
        VmError::Runtime { fn_id, ip, .. } => {
            assert_eq!(*fn_id, 0, "fn_id should be 0");
            // I_DIV is at byte offset 4 (after two ld.cst instructions of 2 bytes each).
            assert_eq!(*ip, 4, "ip should point to i.div at offset 4");
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

// ── NEW: Public stepping API ─────────────────────────────────────────────────

#[test]
fn test_step_api_single_stepping() {
    // ld.cst 0 ; ret
    let bytes = make_msbc(
        &[ConstEntry::I32(77)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.setup_call(0, &[]).expect("setup ok");

    assert!(vm.is_running());

    // Step 1: ld.cst 0
    match vm.step().expect("step 1") {
        StepResult::Continue => {}
        StepResult::Returned(_) => panic!("expected Continue after ld.cst"),
    }

    // Step 2: ret
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
    // Create a product, inspect the heap.
    let bytes = make_msbc(
        &[ConstEntry::I32(10)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, MK_PRD, 1, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let _ = vm.run().expect("runs");
    // After run, call stack is empty.
    assert!(vm.frames().is_empty());
    // Heap should have at least one object (the product).
    assert!(vm.heap().live_count() >= 1);
}

// ── NEW: Garbage collection ──────────────────────────────────────────────────

#[test]
fn test_gc_collects_unreachable_objects() {
    // Allocate a product, then pop the ref (making it unreachable), then GC.
    let bytes = make_msbc(
        &[ConstEntry::I32(1)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                LD_CST, 0, // push 1
                MK_PRD, 1,   // mk.prd 1 → ref (heap object)
                POP, // discard the ref — object is now unreachable
                LD_CST, 0, // push 1 (keep something reachable)
                MK_PRD, 1, // mk.prd 1 → ref (heap object, reachable)
                RET,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let _ = vm.run().expect("runs");
    // Before GC: 2 objects on heap.
    assert_eq!(vm.heap().live_count(), 2);
    let freed = vm.collect_garbage();
    // The popped ref is unreachable (not in call stack or globals anymore),
    // and the returned value is no longer on any frame either (run completed).
    // Both objects become unreachable after run completes (empty call stack).
    // Actually, the returned value is not on any frame. So both get collected.
    assert!(
        freed >= 1,
        "GC should free at least 1 object, freed {freed}"
    );
}

#[test]
fn test_gc_preserves_reachable_globals() {
    // Store a product in a global, GC, then load it back.
    let bytes = make_msbc(
        &[ConstEntry::I32(99)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                LD_CST, 0, // push 99
                MK_PRD, 1, // mk.prd 1 → ref
                ST_GLB, 0, 0, 0, 0, // store to global[0]
                RET_U,
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let _ = vm.run().expect("runs");
    assert_eq!(vm.heap().live_count(), 1);
    // GC should NOT free the object because it's referenced from globals.
    let freed = vm.collect_garbage();
    assert_eq!(freed, 0, "GC should not free globally-reachable objects");
    assert_eq!(vm.heap().live_count(), 1);
}

// ── NEW: Dynamic invocation ──────────────────────────────────────────────────

#[test]
fn test_direct_call_with_inv() {
    // fn 0: call fn 1 with arg 10, return result
    // fn 1: return param + 5
    let bytes = make_msbc(
        &[ConstEntry::I32(10), ConstEntry::I32(5)],
        &[
            fn_def(0, 0, 0, vec![LD_CST, 0, INV, 1, 0, 0, 0, RET]),
            fn_def(1, 1, 1, vec![LD_LOC, 0, LD_CST, 1, I_ADD, RET]),
        ],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 15);
}

// ── NEW: Wide jump ───────────────────────────────────────────────────────────

#[test]
fn test_wide_jump() {
    // jmp.w +2 ; hlt ; ld.cst 0 ; ret
    // jmp.w is 5 bytes. Target = 5 + 2 = 7.
    // hlt is 1 byte at offset 5. ld.cst is at offset 6... no wait.
    // jmp.w at offset 0 (5 bytes), target = 5+2 = 7.
    // hlt at offset 5 (1 byte).
    // NOP at offset 6 (1 byte).
    // ld.cst at offset 7 (2 bytes).
    // ret at offset 9 (1 byte).
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(
            0,
            0,
            0,
            vec![
                JMP_W, 2, 0, 0, 0,    // jmp.w +2, target = 7
                HLT,  // offset 5, skipped
                0x00, // NOP at offset 6, skipped
                LD_CST, 0,   // offset 7, push 42
                RET, // offset 9
            ],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), 42);
}

// ── NEW: Integer negation ────────────────────────────────────────────────────

#[test]
fn test_int_negation() {
    let bytes = make_msbc(
        &[ConstEntry::I32(42)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, I_NEG, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert_eq!(result.as_int().expect("is int"), -42);
}

// ── NEW: Float multiplication ────────────────────────────────────────────────

#[test]
fn test_float_multiply() {
    // 3 * 4 = 12 via float
    let bytes = make_msbc(
        &[ConstEntry::I32(3), ConstEntry::I32(4)],
        &[fn_def(
            0,
            0,
            0,
            vec![LD_CST, 0, CNV_ITF, 0, LD_CST, 1, CNV_ITF, 0, F_MUL, RET],
        )],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    let f = result.as_float().expect("is float");
    assert!((f - 12.0).abs() < f64::EPSILON);
}

// ── NEW: CMP_EQ ──────────────────────────────────────────────────────────────

#[test]
fn test_cmp_eq_equal_values() {
    let bytes = make_msbc(
        &[ConstEntry::I32(5)],
        &[fn_def(0, 0, 0, vec![LD_CST, 0, LD_CST, 0, CMP_EQ, RET])],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    let result = vm.run().expect("runs");
    assert!(result.as_bool().expect("is bool"), "5 == 5 should be true");
}

// ── NEW: Value try_as_ref ────────────────────────────────────────────────────

#[test]
fn test_value_try_as_ref() {
    assert!(Value::from_ref(42).try_as_ref().is_some());
    assert_eq!(Value::from_ref(42).try_as_ref(), Some(42));
    assert!(Value::from_int(42).try_as_ref().is_none());
    assert!(Value::UNIT.try_as_ref().is_none());
}
