use super::*;

#[test]
fn test_run_make_product_and_load_field() {
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(20)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &rec_new(0, 2),
                &[Opcode::REC_GET.0, 1, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 20);
}

#[test]
fn test_run_make_variant_and_check_tag() {
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[&ld_const(0), &rec_new(8, 1), &mat_tag(7), &[Opcode::RET.0]]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert!(result.expect("runs").as_bool().expect("is bool"));
}

#[test]
fn test_make_variant_multi_field_field_0() {
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(20)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &rec_new(1, 2),
                &[Opcode::REC_GET.0, 0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 10);
}

#[test]
fn test_make_variant_multi_field_field_1() {
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(20)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &rec_new(1, 2),
                &[Opcode::REC_GET.0, 1, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 20);
}

#[test]
fn test_array_create_store_load() {
    let bytes = make_msbc(
        &[ConstEntry::Int(3), ConstEntry::Int(99), ConstEntry::Int(1)],
        &[fn_def_with_max_stack(
            0,
            1,
            0,
            code(&[
                &ld_const(0),
                &[Opcode::ARR_NEW.0],
                &[Opcode::ST_LOC.0, 0],
                &[Opcode::LD_LOC.0, 0],
                &ld_const(2),
                &ld_const(1),
                &[Opcode::ARR_SET.0],
                &[Opcode::LD_LOC.0, 0],
                &ld_const(2),
                &[Opcode::ARR_GET.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_array_length() {
    let bytes = make_msbc(
        &[ConstEntry::Int(5)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &[Opcode::ARR_NEW.0, Opcode::ARR_LEN.0, Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_nat().expect("is nat"), 5);
}
