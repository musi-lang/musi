#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use music_il::opcode::Opcode;

use super::Vm;
use crate::error::VmError;
use crate::module::{GlobalDef, Method, Module};
use crate::value::Value;

const fn op(o: Opcode) -> u8 {
    o as u8
}

fn module_with_code(code: Vec<u8>) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: u32::MAX,
            locals_count: 0,
            code,
        }],
        globals: Vec::new(),
    }
}

fn module_with_locals(locals: u16, code: Vec<u8>) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: u32::MAX,
            locals_count: locals,
            code,
        }],
        globals: Vec::new(),
    }
}

#[test]
fn ldsmi_halt_returns_int() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn halt_empty_stack_returns_unit() {
    let mut vm = Vm::new(module_with_code(vec![op(Opcode::Halt)]));
    assert_eq!(vm.run().unwrap(), Value::UNIT);
}

#[test]
fn ldunit_halt() {
    let mut vm = Vm::new(module_with_code(vec![op(Opcode::LdUnit), op(Opcode::Halt)]));
    assert_eq!(vm.run().unwrap(), Value::UNIT);
}

#[test]
fn iadd_two_ints() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::LdSmi),
        32,
        0,
        op(Opcode::IAdd),
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn idiv_by_zero_error() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        1,
        0,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::IDiv),
        op(Opcode::Halt),
    ]));
    assert!(matches!(vm.run(), Err(VmError::DivisionByZero)));
}

#[test]
fn brtrue_taken() {
    // after BrTrue operand, pc=4; offset +3 → land at LdSmi(99) skipping LdSmi(1)
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTrue),
        op(Opcode::BrTrue),
        3,
        0, // after=4, target=7
        op(Opcode::LdSmi),
        1,
        0, // skipped
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn brfalse_not_taken_when_true() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTrue),
        op(Opcode::BrFalse),
        3,
        0, // not taken
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn brjmp_unconditional() {
    // after BrJmp operand pc=3; offset +3 → land at LdSmi(7)
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::BrJmp),
        3,
        0, // after=3, target=6
        op(Opcode::LdSmi),
        1,
        0, // skipped
        op(Opcode::LdSmi),
        7,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 7);
}

#[test]
fn stloc_ldloc() {
    let mut vm = Vm::new(module_with_locals(
        1,
        vec![
            op(Opcode::LdSmi),
            5,
            0,
            op(Opcode::StLoc),
            0,
            op(Opcode::LdSmi),
            3,
            0,
            op(Opcode::LdLoc),
            0,
            op(Opcode::IAdd),
            op(Opcode::Halt),
        ],
    ));
    assert_eq!(vm.run().unwrap().as_int(), 8);
}

#[test]
fn brback_forward_jump() {
    // BrBack with a positive offset behaves identically to BrJmp
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::BrBack),
        3,
        0, // after=3, target=6
        op(Opcode::LdSmi),
        1,
        0, // skipped
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn no_entry_point_error() {
    let module = Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: Vec::new(),
        globals: Vec::new(),
    };
    let mut vm = Vm::new(module);
    assert!(matches!(vm.run(), Err(VmError::NoEntryPoint)));
}

#[test]
fn panic_opcode() {
    let mut vm = Vm::new(module_with_code(vec![op(Opcode::Panic)]));
    assert!(matches!(vm.run(), Err(VmError::ExplicitPanic)));
}

#[test]
fn nested_branches() {
    // if true { if false { 1 } else { 42 } } else { 0 }  → 42
    //
    // pc layout:
    //  0: LdTrue
    //  1: BrFalse +16 → after=4, target=20 (else_outer)
    //  4: LdFalse
    //  5: BrFalse +6  → after=8, target=14 (else_inner)
    //  8: LdSmi 1
    // 11: BrJmp +9    → after=14, target=23 (Halt)
    // 14: LdSmi 42
    // 17: BrJmp +3    → after=20, target=23 (Halt)
    // 20: LdSmi 0     (else_outer)
    // 23: Halt
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTrue),
        op(Opcode::BrFalse),
        16,
        0,
        op(Opcode::LdFalse),
        op(Opcode::BrFalse),
        6,
        0,
        op(Opcode::LdSmi),
        1,
        0,
        op(Opcode::BrJmp),
        9,
        0,
        op(Opcode::LdSmi),
        42,
        0, // else_inner
        op(Opcode::BrJmp),
        3,
        0,
        op(Opcode::LdSmi),
        0,
        0, // else_outer
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

// ── Phase 2: calls, globals, closures ────────────────────────────────────────

fn two_method_module_locals(
    entry_locals: u16,
    callee_locals: u16,
    entry_code: Vec<u8>,
    callee_code: Vec<u8>,
) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![
            Method {
                name: u32::MAX,
                locals_count: entry_locals,
                code: entry_code,
            },
            Method {
                name: 1,
                locals_count: callee_locals,
                code: callee_code,
            },
        ],
        globals: Vec::new(),
    }
}

fn two_method_module(entry_code: Vec<u8>, callee_code: Vec<u8>) -> Module {
    two_method_module_locals(0, 0, entry_code, callee_code)
}

#[test]
fn call_simple() {
    // entry: push bare method index 1 as callee, push arg 99, Call(1)
    // callee (idx=1, locals=1): load local 0, Ret
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0, // callee = method index 1
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::Call),
        1, // arity=1
        op(Opcode::Halt),
    ];
    let callee_code = vec![op(Opcode::LdLoc), 0, op(Opcode::Ret)];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, callee_code));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn call_nested() {
    // entry (idx=0) → method 1 → method 2 which returns arg + 1
    // entry passes 10 → expects 11
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0, // callee = method 1
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::Call),
        1,
        op(Opcode::Halt),
    ];
    let m1_code = vec![
        op(Opcode::LdSmi),
        2,
        0, // callee = method 2
        op(Opcode::LdLoc),
        0,
        op(Opcode::Call),
        1,
        op(Opcode::Ret),
    ];
    let m2_code = vec![
        op(Opcode::LdLoc),
        0,
        op(Opcode::LdOne),
        op(Opcode::IAdd),
        op(Opcode::Ret),
    ];
    let module = Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![
            Method {
                name: u32::MAX,
                locals_count: 0,
                code: entry_code,
            },
            Method {
                name: 1,
                locals_count: 1,
                code: m1_code,
            },
            Method {
                name: 2,
                locals_count: 1,
                code: m2_code,
            },
        ],
        globals: Vec::new(),
    };
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 11);
}

#[test]
fn call_tail() {
    // tail call reuses the current frame, so no stack growth
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0, // callee = method 1
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::CallTail),
        1,
        op(Opcode::Halt), // unreachable
    ];
    let callee_code = vec![op(Opcode::LdLoc), 0, op(Opcode::Ret)];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, callee_code));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn globals_store_load() {
    // StGlb(0) then LdGlb(0) round-trips a value
    let code = vec![
        op(Opcode::LdSmi),
        77,
        0,
        op(Opcode::StGlb),
        0,
        0,
        op(Opcode::LdGlb),
        0,
        0,
        op(Opcode::Halt),
    ];
    let module = Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: u32::MAX,
            locals_count: 0,
            code,
        }],
        globals: vec![GlobalDef {
            name: 0,
            exported: false,
            opaque: false,
        }],
    };
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn closure_basic() {
    // entry: push upvalue 55, ClsNew(method=1, upvals=1), Call(0)
    // closure body: LdUpv(0), Ret → returns captured value
    // ClsNew encoding: opcode, u16 method_idx LE, u8 upval_count
    let entry_code = vec![
        op(Opcode::LdSmi),
        55,
        0, // push upvalue
        op(Opcode::ClsNew),
        1,
        0,
        1, // method_idx=1, upval_count=1
        op(Opcode::Call),
        0, // call closure with 0 args
        op(Opcode::Halt),
    ];
    let closure_body = vec![
        op(Opcode::LdUpv),
        0,
        0, // slot=0 (u16 LE)
        op(Opcode::Ret),
    ];
    let mut vm = Vm::new(two_method_module(entry_code, closure_body));
    assert_eq!(vm.run().unwrap().as_int(), 55);
}

#[test]
fn closure_mutation() {
    // entry: push upvalue 10, ClsNew(method=1, upvals=1), push arg 99, Call(1)
    // closure body: load arg, StUpv(0), LdUpv(0), Ret → returns 99
    let entry_code = vec![
        op(Opcode::LdSmi),
        10,
        0, // initial upvalue
        op(Opcode::ClsNew),
        1,
        0,
        1, // method_idx=1, upval_count=1
        op(Opcode::LdSmi),
        99,
        0, // arg
        op(Opcode::Call),
        1, // arity=1
        op(Opcode::Halt),
    ];
    let closure_body = vec![
        op(Opcode::LdLoc),
        0, // load arg
        op(Opcode::StUpv),
        0,
        0, // store into upvalue slot 0
        op(Opcode::LdUpv),
        0,
        0, // load upvalue slot 0
        op(Opcode::Ret),
    ];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, closure_body));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn call_stack_overflow() {
    // entry self-calls infinitely via bare method index 0
    let code = vec![
        op(Opcode::LdZero), // method index 0
        op(Opcode::Call),
        0,
        op(Opcode::Halt),
    ];
    let mut vm = Vm::new(module_with_code(code));
    assert!(matches!(vm.run(), Err(VmError::CallStackOverflow)));
}
