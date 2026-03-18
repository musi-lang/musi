use super::*;
use crate::loader::load;
use crate::verifier::verify;

#[test]
fn test_closure_captures_upvalue() {
    let fn1_code = vec![Opcode::LD_UPV.0, 0, Opcode::RET.0];
    let fn0_code = code(&[
        &ld_const(0),
        &[Opcode::ST_LOC.0, 0],
        &cls_new(1),
        &cls_upv(0, 0),
        &call(0),
        &[Opcode::RET.0],
    ]);
    let bytes = make_msbc(
        &[ConstEntry::Int(42)],
        &[
            fn_def(1, 0, fn0_code),
            fn_def_with_upvalues(0, 0, 1, fn1_code),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 42);
}

#[test]
fn test_closure_shared_upvalue_mutation() {
    // fn 1 (writer): push 99, st.upv 0, ret.unit
    // fn 2 (reader): ld.upv 0, ret
    // fn 0: local 0 = 10, build writer+reader both over local 0, call writer, call reader → 99
    let fn1_code = code(&[&ld_const(1), &[Opcode::ST_UPV.0, 0], &[Opcode::RET_UNIT.0]]);
    let fn2_code = vec![Opcode::LD_UPV.0, 0, Opcode::RET.0];
    let fn0_code = code(&[
        &ld_const(0),
        &[Opcode::ST_LOC.0, 0],
        &cls_new(1),
        &cls_upv(0, 0),
        &[Opcode::ST_LOC.0, 1],
        &cls_new(2),
        &cls_upv(0, 0),
        &[Opcode::ST_LOC.0, 2],
        &[Opcode::LD_LOC.0, 1],
        &call(0),
        &[Opcode::POP.0],
        &[Opcode::LD_LOC.0, 2],
        &call(0),
        &[Opcode::RET.0],
    ]);
    let bytes = make_msbc(
        &[ConstEntry::Int(10), ConstEntry::Int(99)],
        &[
            fn_def(3, 0, fn0_code),
            fn_def_with_upvalues(0, 0, 1, fn1_code),
            fn_def_with_upvalues(0, 0, 1, fn2_code),
        ],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 99);
}

#[test]
fn test_closure_close_on_return() {
    // fn 0: local 0 = 7, build closure (fn 1) over local 0, return closure.
    // fn 1 (reader, 1 upvalue): ld.upv 0, ret
    // fn 2 (entry): call fn 0, call escaped closure → 7
    let fn0_code = code(&[
        &ld_const(0),
        &[Opcode::ST_LOC.0, 0],
        &cls_new(1),
        &cls_upv(0, 0),
        &[Opcode::RET.0],
    ]);
    let fn1_code = vec![Opcode::LD_UPV.0, 0, Opcode::RET.0];
    let entry_code = code(&[&cls_new(0), &call(0), &call(0), &[Opcode::RET.0]]);
    let bytes = make_msbc(
        &[ConstEntry::Int(7)],
        &[
            fn_def(1, 0, fn0_code),
            fn_def_with_upvalues(0, 0, 1, fn1_code),
            fn_def(0, 0, entry_code),
        ],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.call_fn(2, &[]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}

#[test]
fn test_closure_nested_recapture() {
    // fn 0: local 0 = 55, build outer closure (fn 1) over local 0.
    // fn 1 (outer): build inner closure (fn 2) re-capturing upvalue 0, return it.
    // fn 2 (inner): ld.upv 0, ret
    // fn 3 (entry): call fn 0 → outer, call outer → inner, call inner → 55
    let fn0_code = code(&[
        &ld_const(0),
        &[Opcode::ST_LOC.0, 0],
        &cls_new(1),
        &cls_upv(0, 0),
        &[Opcode::RET.0],
    ]);
    let fn1_code = code(&[&cls_new(2), &cls_upv(1, 0), &[Opcode::RET.0]]);
    let fn2_code = vec![Opcode::LD_UPV.0, 0, Opcode::RET.0];
    let entry_code = code(&[&cls_new(0), &call(0), &call(0), &call(0), &[Opcode::RET.0]]);
    let bytes = make_msbc(
        &[ConstEntry::Int(55)],
        &[
            fn_def(1, 0, fn0_code),
            fn_def_with_upvalues(0, 0, 1, fn1_code),
            fn_def_with_upvalues(0, 0, 1, fn2_code),
            fn_def(0, 0, entry_code),
        ],
    );
    let module = load(&bytes).expect("loads");
    verify(&module).expect("verifies");
    let mut vm = Vm::new(module);
    let result = vm.call_fn(3, &[]);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 55);
}
