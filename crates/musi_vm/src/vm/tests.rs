#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use music_il::opcode::Opcode;

use super::Vm;
use crate::errors::VmError;
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

fn module_with_constants_and_code(constants: Vec<Value>, code: Vec<u8>) -> Module {
    Module {
        constants,
        strings: Vec::new(),
        methods: vec![Method {
            name: u32::MAX,
            locals_count: 0,
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

#[test]
fn effect_push_pop() {
    // Install handler, main body runs normally, handler removed via EffPop.
    // Byte layout:
    // [0]  EffPush  [1,2]  5,0      skip 5 bytes past handler body → land at [8]
    // [3]  LdSmi   [4,5]  99,0     handler body (dead code)
    // [6]  EffResume [7]  0        handler body cont. (dead code)
    // [8]  LdSmi   [9,10] 42,0    main body
    // [11] EffPop
    // [12] Halt                     returns 42
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        5,
        0,
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::EffResume),
        0,
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::EffPop),
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn effect_need_resume() {
    // Install handler; main body calls EffNeed (suspends); handler resumes with 77.
    // Byte layout:
    // [0]  EffPush   [1,2]  5,0     skip 5 bytes to main body → land at [8]
    // [3]  LdSmi     [4,5]  77,0    handler: push resume value onto stack
    // [6]  EffResume [7]    1       handler: pop value, pop cont_ptr, restore
    // [8]  LdSmi     [9,10] 0,0     main: dummy arg for need
    // [11] EffNeed   [12,13] 0,0    main: suspend → resume_pc=14
    // [14] Halt                      returns 77
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        5,
        0,
        op(Opcode::LdSmi),
        77,
        0,
        op(Opcode::EffResume),
        1,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::EffNeed),
        0,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn effect_nested() {
    // Two handlers installed; inner EffNeed fires (LIFO), inner handler resumes
    // with 77, outer handler survives and is removed via EffPop.
    //
    // Byte layout:
    // [0-2]   EffPush 5,0      outer: skip=5 → handler_pc=3, land at 8
    // [3-5]   LdSmi 88,0       outer handler body (unreachable)
    // [6-7]   EffResume 1
    // [8-10]  EffPush 5,0      inner: skip=5 → handler_pc=11, land at 16
    // [11-13] LdSmi 77,0       inner handler body
    // [14-15] EffResume 1
    // [16-18] LdSmi 0,0        main: dummy arg
    // [19-21] EffNeed 0,0      suspend → resume_pc=22
    // [22]    EffPop            remove outer
    // [23]    Halt              returns 77
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        5,
        0,
        op(Opcode::LdSmi),
        88,
        0,
        op(Opcode::EffResume),
        1,
        op(Opcode::EffPush),
        5,
        0,
        op(Opcode::LdSmi),
        77,
        0,
        op(Opcode::EffResume),
        1,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::EffNeed),
        0,
        0,
        op(Opcode::EffPop),
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

// ── Array opcodes ─────────────────────────────────────────────────────────────

#[test]
fn arr_new_and_geti_seti() {
    // ArrNew(2); LdSmi(10); ArrSeti(0); ArrGeti(0); Halt → 10
    //
    // Stack trace:
    //   ArrNew(2): [arr_ptr]
    //   LdSmi(10): [arr_ptr, 10]
    //   ArrSeti(0): pop 10, peek arr_ptr, arr[0]=10 → [arr_ptr]
    //   ArrGeti(0): pop arr_ptr, push arr[0]=10 → [10]
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        2,
        0, // u16 LE: len=2
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::ArrSeti),
        0,
        op(Opcode::ArrGeti),
        0,
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 10);
}

#[test]
fn arr_get_set_dynamic() {
    // ArrNew(1); Dup; LdZero(idx); LdSmi(42); ArrSet; LdZero(idx); ArrGet; Halt → 42
    //
    // Stack trace:
    //   ArrNew(1): [ptr]
    //   Dup:       [ptr, ptr]
    //   LdZero:    [ptr, ptr, 0]
    //   LdSmi(42): [ptr, ptr, 0, 42]
    //   ArrSet:    pop 42, pop 0, pop ptr → arr[0]=42; [ptr]
    //   LdZero:    [ptr, 0]
    //   ArrGet:    pop 0, pop ptr, push arr[0]=42 → [42]
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::Dup),
        op(Opcode::LdZero),
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::ArrSet),
        op(Opcode::LdZero),
        op(Opcode::ArrGet),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn arr_len() {
    // ArrNew(3); ArrLen; Halt → 3
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        3,
        0,
        op(Opcode::ArrLen),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 3);
}

#[test]
fn arr_tag_variant() {
    // constants[0] = Tag(0).
    // ArrNewt(pool_idx=0, len=1); ArrTag; LdCst(0); CmpEq; Halt → true
    //
    // ArrTag returns the stored tag (Tag(0)).
    // LdCst(0) loads the same value.
    // CmpEq compares bit-identical values → true.
    let constants = vec![Value::from_tag(0)];
    let code = vec![
        op(Opcode::ArrNewt),
        0, // tag_pool_idx = 0
        1,
        0, // len = 1 (u16 LE)
        op(Opcode::ArrTag),
        op(Opcode::LdCst),
        0,
        0, // constants[0]
        op(Opcode::CmpEq),
        op(Opcode::Halt),
    ];
    let mut vm = Vm::new(module_with_constants_and_code(constants, code));
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn arr_copy() {
    // ArrNew(1); LdSmi(5); ArrSeti(0); ArrCopy; ArrGeti(0); Halt → 5
    //
    // Copy produces an independent array with the same elements.
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        5,
        0,
        op(Opcode::ArrSeti),
        0,
        op(Opcode::ArrCopy),
        op(Opcode::ArrGeti),
        0,
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 5);
}

#[test]
fn arr_concat() {
    // Build two length-1 arrays, concat them, verify the result has length 2.
    //
    // Stack trace:
    //   ArrNew(1): [a]
    //   LdSmi(10); ArrSeti(0): a[0]=10, [a]
    //   ArrNew(1): [a, b]
    //   LdSmi(20); ArrSeti(0): b[0]=20, [a, b]
    //   ArrConcat: pop b, pop a, create [10,20], [c]
    //   ArrLen: [2]
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::ArrSeti),
        0,
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        20,
        0,
        op(Opcode::ArrSeti),
        0,
        op(Opcode::ArrConcat),
        op(Opcode::ArrLen),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 2);
}

#[test]
fn tychk_passthrough() {
    // TyChk is a no-op — value passes through unchanged.
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyChk),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn tycast_passthrough() {
    // TyCast is a no-op — value passes through unchanged.
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyCast),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

// ── BrTbl ─────────────────────────────────────────────────────────────────────

#[test]
fn brtbl_in_range() {
    // BrTbl with count=3, index=1 → lands on LdSmi(99).
    //
    // Byte layout:
    //  [0-2]   LdSmi 1 0          push index=1
    //  [3]     BrTbl
    //  [4-5]   3 0                count=3
    //  [6-7]   0 0                offset[0]=0  → after-table(12) + 0  = 12
    //  [8-9]   6 0                offset[1]=6  → after-table(12) + 6  = 18
    //  [10-11] 12 0               offset[2]=12 → after-table(12) + 12 = 24
    //  [12-14] LdSmi 10 0         branch 0
    //  [15-17] BrJmp 9 0          skip to Halt at 27
    //  [18-20] LdSmi 99 0         branch 1 ← idx=1 lands here
    //  [21-23] BrJmp 3 0          skip to Halt at 27
    //  [24-26] LdSmi 77 0         branch 2
    //  [27]    Halt
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        1,
        0,
        op(Opcode::BrTbl),
        3,
        0,
        0,
        0,
        6,
        0,
        12,
        0,
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::BrJmp),
        9,
        0,
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::BrJmp),
        3,
        0,
        op(Opcode::LdSmi),
        77,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn brtbl_out_of_range() {
    // index=5, count=3 → out of range, falls through to LdSmi(42).
    //
    // Byte layout:
    //  [0-2]   LdSmi 5 0    push index=5
    //  [3]     BrTbl
    //  [4-5]   3 0          count=3
    //  [6-11]  zeros        three offsets (never used)
    //  [12-14] LdSmi 42 0   fall-through target
    //  [15]    Halt
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        5,
        0,
        op(Opcode::BrTbl),
        3,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn brtbl_negative_index() {
    // index=-1 → usize::try_from fails, falls through to LdSmi(42).
    //
    // Same layout as brtbl_out_of_range except LdSmi encodes -1 (i16 LE = 0xFF 0xFF).
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        0xFF,
        0xFF,
        op(Opcode::BrTbl),
        3,
        0,
        0,
        0,
        0,
        0,
        0,
        0,
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}
