use super::*;

#[test]
fn test_run_conditional_jump() {
    // CMP_LT pops (b=3, a=5): 5 < 3 = false.
    // BR_FALSE taken, skips push idx=0, returns 3.
    let bytes = make_msbc(
        &[ConstEntry::Int(5), ConstEntry::Int(3)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &ld_const(1),
                &[Opcode::CMP_LT.0],
                &br_false(3),
                &ld_const(0),
                &ld_const(1),
                &[Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(
        result.expect("runs").as_int().expect("is int"),
        3,
        "br_false taken: should return 3"
    );
}

#[test]
fn test_wide_jump() {
    // BR +2 skips PANIC + NOP, lands on LD_CONST 0.
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &br(2),
                &[Opcode::PANIC.0, Opcode::NOP.0],
                &ld_const(0),
                &[Opcode::RET.0],
            ]),
        )],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_run_tail_call_countdown() {
    let bytes = make_msbc(
        &[ConstEntry::Int(0), ConstEntry::Int(1)],
        &[fn_def(
            1,
            1,
            code(&[
                &[Opcode::LD_LOC.0, 0],
                &ld_const(0),
                &[Opcode::CMP_EQ.0],
                &br_false(4),
                &ld_const(0),
                &[Opcode::RET.0],
                &cls_new(0),
                &[Opcode::LD_LOC.0, 0],
                &ld_const(1),
                &[Opcode::SUB.0],
                &call_tail(1),
            ]),
        )],
    );
    let (_, result) = run_vm_call(&bytes, 0, &[Value::from_int(10)]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 0);
}

#[test]
fn test_direct_call_with_call() {
    // fn 0: CLS_NEW fn1, push 10, CALL 1 → fn1(10) = 15
    // fn 1(x): x + 5
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(5)],
        &[
            fn_def(
                0,
                0,
                code(&[&cls_new(1), &ld_const(0), &call(1), &[Opcode::RET.0]]),
            ),
            fn_def(
                1,
                1,
                code(&[
                    &[Opcode::LD_LOC.0, 0],
                    &ld_const(1),
                    &[Opcode::ADD.0, Opcode::RET.0],
                ]),
            ),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 15);
}

#[test]
fn test_panic_returns_halted_error() {
    let bytes = make_msbc(&[], &[fn_def(0, 0, vec![Opcode::PANIC.0])]);
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
    let bytes = make_msbc(&[], &[fn_def(0, 0, code(&[&br(-3)]))]);
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.set_instruction_limit(Some(100));
    let result = vm.run();
    assert!(result.is_err());
    assert!(matches!(
        result.unwrap_err(),
        VmError::InstructionLimitExceeded { limit: 100 }
    ));
    assert_eq!(vm.instruction_count(), 100);
}

#[test]
fn test_error_context_contains_fn_id_and_ip() {
    let bytes = make_msbc(
        &[ConstEntry::Int(1), ConstEntry::Int(0)],
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
        VmError::Runtime { fn_id, ip, .. } => {
            assert_eq!(*fn_id, 0);
            assert_eq!(*ip, 6, "ip should point to div at offset 6");
        }
        _ => panic!("expected Runtime error, got {err:?}"),
    }
}

#[test]
fn test_step_api_single_stepping() {
    let bytes = make_msbc(
        &[ConstEntry::Int(77)],
        &[fn_def(0, 0, code(&[&ld_const(0), &[Opcode::RET.0]]))],
    );
    let module = load(&bytes).expect("loads");
    let mut vm = Vm::new(module);
    vm.setup_call(0, &[]).expect("setup ok");

    assert!(vm.is_running());

    match vm.step().expect("step 1") {
        StepResult::Continue => {}
        StepResult::Returned(_) => panic!("expected Continue after ld.const"),
    }

    match vm.step().expect("step 2") {
        StepResult::Continue => panic!("expected Returned after ret"),
        StepResult::Returned(v) => {
            assert_eq!(v.as_int().expect("is int"), 77);
        }
    }

    assert!(!vm.is_running());
}
