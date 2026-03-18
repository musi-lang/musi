use super::*;

// ---------------------------------------------------------------------------
// Loader tests
// ---------------------------------------------------------------------------

#[test]
fn test_load_valid_header_succeeds() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::RET_UNIT.0])]);
    let result = load(&bytes);
    assert!(result.is_ok(), "expected Ok, got {result:?}");
}

#[test]
fn test_load_bad_magic_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::RET_UNIT.0])]);
    bytes[0] = b'X';
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadMagic)),
        "expected BadMagic, got {result:?}"
    );
}

#[test]
fn test_load_bad_checksum_returns_error() {
    let mut bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::RET_UNIT.0])]);
    bytes[16] ^= 0xFF;
    let result = load(&bytes);
    assert!(
        matches!(result, Err(VmError::BadChecksum)),
        "expected BadChecksum, got {result:?}"
    );
}

// ---------------------------------------------------------------------------
// Verifier tests
// ---------------------------------------------------------------------------

#[test]
fn test_verifier_rejects_oob_const() {
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, code(&[&ld_const(5), &[Opcode::RET.0]]))],
    );
    let module = load(&bytes).expect("loads ok");
    let result = verify(&module);
    assert!(result.is_err(), "expected Verify error, got Ok");
}

#[test]
fn test_verifier_rejects_stack_overflow() {
    let mut bytecode = ld_const(0).to_vec();
    bytecode.extend(iter::repeat_n(Opcode::DUP.0, 20));
    bytecode.push(Opcode::RET.0);
    let bytes = make_msbc(&[ConstEntry::Int(42)], &[fn_def(0, 0, bytecode)]);
    let module = load(&bytes).expect("loads ok");
    let result = verify(&module);
    assert!(result.is_err(), "expected Verify error for stack overflow");
}

#[test]
fn test_verifier_accepts_max_stack_exact_match() {
    let bytes = make_msbc(
        &[ConstEntry::Int(1), ConstEntry::Int(2)],
        &[fn_def_with_max_stack(
            0,
            0,
            2,
            code(&[&ld_const(0), &ld_const(1), &[Opcode::ADD.0, Opcode::RET.0]]),
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
        &[ConstEntry::Int(1), ConstEntry::Int(2)],
        &[fn_def_with_max_stack(
            0,
            0,
            1,
            code(&[&ld_const(0), &ld_const(1), &[Opcode::ADD.0, Opcode::RET.0]]),
        )],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(result.is_err(), "max_stack=1 should fail when peak is 2");
}

#[test]
fn test_verifier_resets_depth_after_unconditional_jump() {
    // offset 0: LD_CONST 0 (3 bytes) -> depth 1
    // offset 3: BR +0      (3 bytes) -> jumps to offset 6; propagates depth 1
    // offset 6: RET_UNIT   (1 byte)  -> depth 1, but RET_UNIT expects 0 → ok (it resets)
    // max_stack=1 to allow the single-value depth.
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[fn_def_with_max_stack(
            0,
            0,
            1,
            code(&[&ld_const(0), &br(0), &[Opcode::RET_UNIT.0]]),
        )],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(result.is_ok(), "depth must reset after BR, got {result:?}");
}

#[test]
fn test_verifier_depth_resets_to_zero_after_terminator() {
    // RET at offset 0 terminates; dead NOP after is accepted (depth=0).
    // LD_CONST 0, RET, NOP — verifier sees NOP with depth=0 after RET.
    let bytes = make_msbc(
        &[ConstEntry::Int(1)],
        &[fn_def_with_max_stack(
            0,
            0,
            2,
            code(&[&ld_const(0), &[Opcode::RET.0, Opcode::NOP.0]]),
        )],
    );
    let module = load(&bytes).expect("loads");
    let result = verify(&module);
    assert!(
        result.is_ok(),
        "dead NOP after RET should not cause verifier failure"
    );
}

// ---------------------------------------------------------------------------
// Basic execution tests
// ---------------------------------------------------------------------------

#[test]
fn test_run_constant_return_i32() {
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[fn_def(0, 0, code(&[&ld_const(0), &[Opcode::RET.0]]))],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_run_ld_true_ld_false() {
    let bytes = make_msbc(
        &[],
        &[fn_def(
            0,
            0,
            vec![
                Opcode::LD_TRUE.0,
                Opcode::POP.0,
                Opcode::LD_FALSE.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm(&bytes);
    let v = result.expect("runs");
    assert!(v.as_bool().is_ok());
    assert!(!v.as_bool().expect("is bool"));
}

#[test]
fn test_run_ld_unit() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::LD_UNIT.0, Opcode::RET.0])]);
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").is_unit());
}

#[test]
fn test_run_ld_smi() {
    // LD_SMI with signed 16-bit value 1000.
    let smi: i16 = 1000;
    let [hi, lo] = smi.to_be_bytes();
    let bytes = make_msbc(
        &[],
        &[fn_def(0, 0, vec![Opcode::LD_SMI.0, hi, lo, Opcode::RET.0])],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 1000);
    // Negative SMI: -5.
    let smi2: i16 = -5;
    let [h2, l2] = smi2.to_be_bytes();
    let bytes2 = make_msbc(
        &[],
        &[fn_def(0, 0, vec![Opcode::LD_SMI.0, h2, l2, Opcode::RET.0])],
    );
    let (_, r2) = run_vm(&bytes2);
    assert_eq!(r2.expect("runs").as_int().expect("is int"), -5);
}

#[test]
fn test_string_const_returns_heap_ref() {
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hello".to_vec())],
        &[fn_def(0, 0, code(&[&ld_const(0), &[Opcode::RET.0]]))],
    );
    let (_, result) = run_vm(&bytes);
    let v = result.expect("runs");
    assert!(v.try_as_ref().is_some(), "string should be a heap ref");
}

#[test]
fn test_string_const_two_distinct_loads_produce_separate_objects() {
    let bytes = make_msbc(
        &[ConstEntry::Str(b"hi".to_vec())],
        &[fn_def_with_max_stack(
            0,
            0,
            2,
            code(&[
                &ld_const(0),
                &ld_const(0),
                &[Opcode::RET.0], // return top; two allocs happened
            ]),
        )],
    );
    let (vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert!(
        vm.heap().live_count() >= 2,
        "two string loads should allocate two objects"
    );
}

#[test]
fn test_introspection_frames_and_heap() {
    let bytes = make_msbc(
        &[ConstEntry::Int(10)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &rec_new(0, 1), &[Opcode::RET.0]]),
        )],
    );
    let (vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert!(vm.frames().is_empty());
    assert!(vm.heap().live_count() >= 1);
}
