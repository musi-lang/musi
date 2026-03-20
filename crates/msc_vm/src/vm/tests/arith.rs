use super::*;

#[test]
fn test_run_add_two_ints() {
    let bytes = make_msbc(
        &[],
        &[fn_def_with_max_stack(
            0,
            2,
            2,
            vec![
                Opcode::LD_LOC.0,
                0,
                Opcode::LD_LOC.0,
                1,
                Opcode::ADD.0,
                Opcode::RET.0,
            ],
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(3), Value::from_int(4)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

#[test]
fn test_int_negation() {
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &[Opcode::NEG.0, Opcode::RET.0]]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), -42);
}

#[test]
fn test_float_add() {
    let bytes = make_msbc(
        &[ConstEntry::F64(3.0), ConstEntry::F64(2.0)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &ld_const(1), &[Opcode::ADD.0, Opcode::RET.0]]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 5.0).abs() < f64::EPSILON);
}

#[test]
fn test_float_multiply() {
    let bytes = make_msbc(
        &[ConstEntry::F64(3.0), ConstEntry::F64(4.0)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &ld_const(1), &[Opcode::MUL.0, Opcode::RET.0]]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    let f = result.expect("runs").as_float().expect("is float");
    assert!((f - 12.0).abs() < f64::EPSILON);
}

#[test]
fn test_bitwise_and_and_mul() {
    // 0xFF BAND 0x0F = 0x0F, then 0x0F * 16 = 240
    let bytes = make_msbc(
        &[
            ConstEntry::Int(0xFF),
            ConstEntry::Int(0x0F),
            ConstEntry::Int(16),
        ],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &[Opcode::BAND.0],
                &ld_const(2),
                &[Opcode::MUL.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 240);
}

#[test]
fn test_division_by_zero_returns_error() {
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(0)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &ld_const(1), &[Opcode::DIV.0, Opcode::RET.0]]),
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
fn test_stack_underflow_returns_error() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::ADD.0, Opcode::RET.0])]);
    let (_, result) = run_vm(&bytes);
    assert!(result.is_err(), "add on empty stack should error");
}

#[test]
fn test_cmp_eq_equal_values() {
    let bytes = make_msbc(
        &[ConstEntry::Int(5)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(0),
                &[Opcode::CMP_EQ.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_le_true_when_equal() {
    let bytes = make_msbc(
        &[ConstEntry::Int(5)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(0),
                &[Opcode::CMP_LE.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_lt_true() {
    let bytes = make_msbc(
        &[ConstEntry::Int(3), ConstEntry::Int(5)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &[Opcode::CMP_LT.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_gt_true() {
    let bytes = make_msbc(
        &[ConstEntry::Int(7), ConstEntry::Int(3)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &[Opcode::CMP_GT.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_cmp_f_eq_true() {
    let bytes = make_msbc(
        &[ConstEntry::F64(5.0)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(0),
                &[Opcode::CMP_EQ.0, Opcode::RET.0],
            ]),
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
        // from_nat now uses TAG_INT; as_int succeeds
        (Value::from_nat(0xDEAD_BEEF), |v| v.as_int().is_ok()),
        (Value::from_float(1.5), |v| v.as_float().is_ok()),
        // from_bool now uses TAG_INT (0 or 1); as_bool and as_int both succeed
        (Value::from_bool(true), |v| v.as_bool().is_ok()),
        (Value::from_bool(false), |v| v.as_bool().is_ok()),
        // from_rune now uses TAG_INT; as_int succeeds
        (Value::from_rune('A'), |v| v.as_int().is_ok()),
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

    let neg_nan = Value::from_float(f64::from_bits(0xFFF8_0000_0000_0000));
    assert!(neg_nan.as_float().unwrap().is_nan());

    let snan = Value::from_float(f64::from_bits(0x7FF0_0000_0000_0002));
    assert!(snan.as_float().unwrap().is_nan());

    let normal = Value::from_float(1.5);
    assert!(normal.is_float());
    assert!(!normal.as_float().unwrap().is_nan());

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
fn test_value_try_as_ref() {
    assert!(Value::from_ref(42).try_as_ref().is_some());
    assert_eq!(Value::from_ref(42).try_as_ref(), Some(42));
    assert!(Value::from_int(42).try_as_ref().is_none());
    assert!(Value::UNIT.try_as_ref().is_none());
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
