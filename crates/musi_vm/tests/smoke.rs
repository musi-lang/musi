#![allow(
    clippy::unwrap_used,
    clippy::panic,
    clippy::arithmetic_side_effects,
    clippy::as_conversions,
    clippy::tests_outside_test_module
)]

use musi_vm::internal::ENTRY_POINT_NAME;
use musi_vm::{Vm, load};
use music_il::format::ANON_METHOD_NAME;
use music_il::opcode::Opcode;

const fn op(o: Opcode) -> u8 {
    o as u8
}

fn minimal_seam(instr_bytes: &[u8], instr_count: u16) -> Vec<u8> {
    let mut content = Vec::new();
    content.extend_from_slice(&1u16.to_le_bytes());
    content.extend_from_slice(&ENTRY_POINT_NAME.to_le_bytes()); // entry point
    content.extend_from_slice(&0u16.to_le_bytes());
    content.extend_from_slice(&instr_count.to_le_bytes());
    content.extend_from_slice(instr_bytes);

    let content_len = u32::try_from(content.len()).unwrap();
    let total_size = u32::try_from(16 + 8 + content.len()).unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&1u32.to_le_bytes());
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());
    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&content_len.to_le_bytes());
    buf.extend_from_slice(&content);

    buf
}

#[test]
fn smoke_ldsmi_halt() {
    let seam = minimal_seam(&[op(Opcode::LdSmi), 42, 0, op(Opcode::Halt)], 2);
    let module = load(&seam).unwrap();
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn smoke_arithmetic() {
    // (10 + 32) * 1 = 42
    let seam = minimal_seam(
        &[
            op(Opcode::LdSmi),
            10,
            0,
            op(Opcode::LdSmi),
            32,
            0,
            op(Opcode::IAdd),
            op(Opcode::LdSmi),
            1,
            0,
            op(Opcode::IMul),
            op(Opcode::Halt),
        ],
        6,
    );
    let module = load(&seam).unwrap();
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn smoke_conditional() {
    // 5 > 3 → True; BrTrue skips LdSmi(0), loads LdSmi(1)
    // pc layout: LdSmi(5)@0, LdSmi(3)@3, CmpGt@6, BrTrue@7 after=10 +3→13, LdSmi(0)@10, LdSmi(1)@13, Halt@16
    let seam = minimal_seam(
        &[
            op(Opcode::LdSmi),
            5,
            0,
            op(Opcode::LdSmi),
            3,
            0,
            op(Opcode::CmpGt),
            op(Opcode::BrTrue),
            3,
            0, // after=10, target=13
            op(Opcode::LdSmi),
            0,
            0, // skipped
            op(Opcode::LdSmi),
            1,
            0,
            op(Opcode::Halt),
        ],
        7,
    );
    let module = load(&seam).unwrap();
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 1);
}

// ── Phase 2: function calls and globals ──────────────────────────────────────

/// Build a two-method METH section:
/// - method 0 (`name=u32::MAX`, `locals=0`): entry bytecode
/// - method 1 (`name=ANON_METHOD_NAME`, `locals=callee_locals`): callee bytecode
fn two_method_seam(
    entry_bytes: &[u8],
    entry_instr: u16,
    callee_locals: u16,
    callee_bytes: &[u8],
    callee_instr: u16,
) -> Vec<u8> {
    let mut content = Vec::new();
    content.extend_from_slice(&2u16.to_le_bytes()); // method count = 2

    // method 0: entry
    content.extend_from_slice(&ENTRY_POINT_NAME.to_le_bytes()); // name
    content.extend_from_slice(&0u16.to_le_bytes()); // locals
    content.extend_from_slice(&entry_instr.to_le_bytes());
    content.extend_from_slice(entry_bytes);

    // method 1: callee
    content.extend_from_slice(&ANON_METHOD_NAME.to_le_bytes()); // anonymous callee
    content.extend_from_slice(&callee_locals.to_le_bytes());
    content.extend_from_slice(&callee_instr.to_le_bytes());
    content.extend_from_slice(callee_bytes);

    let content_len = u32::try_from(content.len()).unwrap();
    let total_size = u32::try_from(16 + 8 + content.len()).unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&1u32.to_le_bytes());
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());
    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&content_len.to_le_bytes());
    buf.extend_from_slice(&content);
    buf
}

/// Build a SEAM with one method and one global slot.
fn seam_with_global(instr_bytes: &[u8], instr_count: u16) -> Vec<u8> {
    // Three sections: STRT + METH + GLOB
    let strt_content = b"g\0";
    let mut meth_content = Vec::new();
    meth_content.extend_from_slice(&1u16.to_le_bytes()); // 1 method
    meth_content.extend_from_slice(&ENTRY_POINT_NAME.to_le_bytes());
    meth_content.extend_from_slice(&0u16.to_le_bytes()); // locals
    meth_content.extend_from_slice(&instr_count.to_le_bytes());
    meth_content.extend_from_slice(instr_bytes);

    let mut glob_content = Vec::new();
    glob_content.extend_from_slice(&1u16.to_le_bytes()); // 1 global
    glob_content.extend_from_slice(&0u32.to_le_bytes()); // STRT offset for "g"
    glob_content.push(0x01u8); // flags: exported

    let strt_len = u32::try_from(strt_content.len()).unwrap();
    let meth_len = u32::try_from(meth_content.len()).unwrap();
    let glob_len = u32::try_from(glob_content.len()).unwrap();
    let total_size = u32::try_from(
        16 + 8 + strt_content.len() + 8 + meth_content.len() + 8 + glob_content.len(),
    )
    .unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&3u32.to_le_bytes()); // 3 sections
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());
    buf.extend_from_slice(b"STRT");
    buf.extend_from_slice(&strt_len.to_le_bytes());
    buf.extend_from_slice(strt_content);
    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&meth_len.to_le_bytes());
    buf.extend_from_slice(&meth_content);
    buf.extend_from_slice(b"GLOB");
    buf.extend_from_slice(&glob_len.to_le_bytes());
    buf.extend_from_slice(&glob_content);
    buf
}

#[test]
fn smoke_function_call() {
    // entry: push callee=1 (bare int), push arg 7, Call(1), Halt → 7
    // callee: LdLoc(0), Ret
    let entry_bytes = &[
        op(Opcode::LdSmi),
        1,
        0, // callee index
        op(Opcode::LdSmi),
        7,
        0, // arg
        op(Opcode::Call),
        1, // arity=1
        op(Opcode::Halt),
    ];
    let callee_bytes = &[op(Opcode::LdLoc), 0, op(Opcode::Ret)];
    let seam = two_method_seam(entry_bytes, 4, 1, callee_bytes, 2);
    let module = load(&seam).unwrap();
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 7);
}

#[test]
fn smoke_effect_basic() {
    // Direct Module construction (not .seam) verifying HdlPush → EffInvk → EffCont round-trip.
    // Byte layout: same as effect_perform_resume in vm/tests.rs
    // [0]  HdlPush   [1,2]  0,0 [3,4] 0,0 [5,6] 5,0      skip 5 bytes → land at main body [12]
    // [7]  LdSmi        [8,9]  77,0                         handler: push resume value
    // [10] EffCont      [11]   1                            handler: pop value, pop cont_ptr, restore
    // [12] LdSmi        [13,14] 0,0                         main: dummy arg for perform
    // [15] EffInvk      [16,17] 0,0 [18,19] 0,0             main: suspend → resume_pc=20
    // [20] Halt                                             returns 77
    use musi_vm::internal::{Method, Module};
    let module = Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: 0,
            code: vec![
                op(Opcode::HdlPush),
                0,
                0, // effect_id = 0
                0,
                0, // op_id = 0
                5,
                0, // skip = 5
                op(Opcode::LdSmi),
                77,
                0,
                op(Opcode::EffCont),
                1,
                op(Opcode::LdSmi),
                0,
                0,
                op(Opcode::EffInvk),
                0,
                0,
                0,
                0,
                op(Opcode::Halt),
            ],
        }],
        globals: Vec::new(),
        types: Vec::new(),
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn smoke_globals() {
    // StGlob(0) stores 42, LdGlob(0) loads it back
    let instr_bytes = &[
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::StGlob),
        0,
        0,
        op(Opcode::LdGlob),
        0,
        0,
        op(Opcode::Halt),
    ];
    let seam = seam_with_global(instr_bytes, 4);
    let module = load(&seam).unwrap();
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 42);
}
