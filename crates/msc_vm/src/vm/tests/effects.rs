use super::*;

#[test]
fn test_eff_do_cross_frame_finds_handler() {
    let effect_id: u8 = 1;
    let fn0_code = code(&[
        &eff_hdl(u16::from(effect_id)),
        &cls_new(1),
        &call(0),
        &[Opcode::RET.0],
    ]);
    let fn1_code = code(&[&eff_need(1, 0), &[Opcode::RET.0]]);
    let fn2_code = code(&[&ld_const(0), &[Opcode::RET.0]]);

    let bytes = make_seam_with_effects(
        &[ConstEntry::Int(42)],
        &[SeamEffectDef {
            id: 1,
            name: "test_eff",
            ops: vec![SeamEffectOpDef {
                id: 1,
                name: "test_op",
            }],
        }],
        &[
            FnDef {
                local_count: 0,
                param_count: 0,
                upvalue_count: 0,
                code: fn0_code,
                handlers: vec![(effect_id, 2)],
                max_stack: None,
            },
            fn_def(0, 0, fn1_code),
            fn_def(0, 0, fn2_code),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_eff_res_resumes_continuation() {
    // fn 2 (handler): push 99, EFF_RES → resume fn 1 with 99; RET_UNIT.
    let effect_id: u8 = 1;
    let fn0_code = code(&[
        &eff_hdl(u16::from(effect_id)),
        &cls_new(1),
        &call(0),
        &[Opcode::RET.0],
    ]);
    let fn1_code = code(&[&eff_need(1, 0), &[Opcode::RET.0]]);
    let fn2_code = code(&[&ld_const(0), &[Opcode::EFF_RES.0], &[Opcode::RET_UNIT.0]]);

    let bytes = make_seam_with_effects(
        &[ConstEntry::Int(99)],
        &[SeamEffectDef {
            id: 1,
            name: "test_eff",
            ops: vec![SeamEffectOpDef {
                id: 1,
                name: "test_op",
            }],
        }],
        &[
            FnDef {
                local_count: 0,
                param_count: 0,
                upvalue_count: 0,
                code: fn0_code,
                handlers: vec![(effect_id, 2)],
                max_stack: None,
            },
            fn_def(0, 0, fn1_code),
            fn_def(0, 0, fn2_code),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}
