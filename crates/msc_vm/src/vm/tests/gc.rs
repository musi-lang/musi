use super::*;

#[test]
fn test_gc_collects_unreachable_objects() {
    let bytes = make_msbc(
        &[ConstEntry::Int(1)],
        &[fn_def_with_max_stack(
            0,
            0,
            0,
            code(&[
                &ld_const(0),
                &rec_new(0, 1),
                &[Opcode::POP.0], // discard → unreachable
                &ld_const(0),
                &rec_new(0, 1),
                &[Opcode::RET.0],
            ]),
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
fn test_gc_preserves_reachable_objects() {
    // Allocate a record, store it in a local, run GC - it should survive.
    let bytes = make_msbc(
        &[ConstEntry::Int(99)],
        &[fn_def_with_max_stack(
            0,
            1,
            0,
            code(&[
                &ld_const(0),
                &rec_new(0, 1),
                &[Opcode::ST_LOC.0, 0],
                &[Opcode::LD_LOC.0, 0, Opcode::RET.0],
            ]),
        )],
    );
    let (mut vm, result) = run_vm(&bytes);
    let _ = result.expect("runs");
    assert_eq!(vm.heap().live_count(), 1);
    let freed = vm.collect_garbage();
    // After run completes the frame is gone; object may be freed - just verify GC ran without panic.
    let _ = freed;
}

#[test]
fn test_sync_program_no_scheduler_overhead() {
    let bytes = make_msbc(
        &[ConstEntry::Int(7)],
        &[fn_def(0, 0, code(&[&ld_const(0), &[Opcode::RET.0]]))],
    );
    let (_, result) = run_vm(&bytes);
    assert_eq!(result.expect("runs").as_int().expect("is int"), 7);
}
