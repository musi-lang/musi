#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use music_il::opcode::Opcode;

use super::Vm;
use crate::errors::VmError;
use crate::module::{ConstantEntry, GlobalDef, Method, Module, ENTRY_POINT_NAME};
use crate::value::Value;

const fn op(o: Opcode) -> u8 {
    o as u8
}

fn module_with_code(code: Vec<u8>) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: 0,
            code,
        }],
        globals: Vec::new(),
        types: Vec::new(),
    }
}

fn module_with_locals(locals: u16, code: Vec<u8>) -> Module {
    Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: locals,
            code,
        }],
        globals: Vec::new(),
        types: Vec::new(),
    }
}

fn module_with_constants_and_code(constants: Vec<ConstantEntry>, code: Vec<u8>) -> Module {
    Module {
        constants,
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: 0,
            code,
        }],
        globals: Vec::new(),
        types: Vec::new(),
    }
}

fn module_with_strings_and_code(
    strings: Vec<String>,
    constants: Vec<ConstantEntry>,
    code: Vec<u8>,
) -> Module {
    Module {
        constants,
        strings,
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: 0,
            code,
        }],
        globals: Vec::new(),
        types: Vec::new(),
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
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTru),
        op(Opcode::BrTrue),
        3,
        0,
        op(Opcode::LdSmi),
        1,
        0,
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
        op(Opcode::LdTru),
        op(Opcode::BrFalse),
        3,
        0,
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn brjmp_unconditional() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::BrJmp),
        3,
        0,
        op(Opcode::LdSmi),
        1,
        0,
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
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::BrBack),
        3,
        0,
        op(Opcode::LdSmi),
        1,
        0,
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
        types: Vec::new(),
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
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTru),
        op(Opcode::BrFalse),
        16,
        0,
        op(Opcode::LdFls),
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
        0,
        op(Opcode::BrJmp),
        3,
        0,
        op(Opcode::LdSmi),
        0,
        0,
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
                name: ENTRY_POINT_NAME,
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
        types: Vec::new(),
    }
}

fn two_method_module(entry_code: Vec<u8>, callee_code: Vec<u8>) -> Module {
    two_method_module_locals(0, 0, entry_code, callee_code)
}

#[test]
fn call_simple() {
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0,
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::Call),
        1,
        op(Opcode::Halt),
    ];
    let callee_code = vec![op(Opcode::LdLoc), 0, op(Opcode::Ret)];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, callee_code));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn call_nested() {
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0,
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
        0,
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
                name: ENTRY_POINT_NAME,
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
        types: Vec::new(),
    };
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 11);
}

#[test]
fn call_tail() {
    let entry_code = vec![
        op(Opcode::LdSmi),
        1,
        0,
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::CallTail),
        1,
        op(Opcode::Halt),
    ];
    let callee_code = vec![op(Opcode::LdLoc), 0, op(Opcode::Ret)];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, callee_code));
    assert_eq!(vm.run().unwrap().as_int(), 42);
}

#[test]
fn globals_store_load() {
    let code = vec![
        op(Opcode::LdSmi),
        77,
        0,
        op(Opcode::StGlob),
        0,
        0,
        op(Opcode::LdGlob),
        0,
        0,
        op(Opcode::Halt),
    ];
    let module = Module {
        constants: Vec::new(),
        strings: Vec::new(),
        methods: vec![Method {
            name: ENTRY_POINT_NAME,
            locals_count: 0,
            code,
        }],
        globals: vec![GlobalDef {
            name: 0,
            exported: false,
            opaque: false,
        }],
        types: Vec::new(),
    };
    let mut vm = Vm::new(module);
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn closure_basic() {
    let entry_code = vec![
        op(Opcode::LdSmi),
        55,
        0,
        op(Opcode::ClsNew),
        1,
        0,
        1,
        op(Opcode::Call),
        0,
        op(Opcode::Halt),
    ];
    let closure_body = vec![op(Opcode::LdUpv), 0, 0, op(Opcode::Ret)];
    let mut vm = Vm::new(two_method_module(entry_code, closure_body));
    assert_eq!(vm.run().unwrap().as_int(), 55);
}

#[test]
fn closure_mutation() {
    let entry_code = vec![
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::ClsNew),
        1,
        0,
        1,
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::Call),
        1,
        op(Opcode::Halt),
    ];
    let closure_body = vec![
        op(Opcode::LdLoc),
        0,
        op(Opcode::StUpv),
        0,
        0,
        op(Opcode::LdUpv),
        0,
        0,
        op(Opcode::Ret),
    ];
    let mut vm = Vm::new(two_method_module_locals(0, 1, entry_code, closure_body));
    assert_eq!(vm.run().unwrap().as_int(), 99);
}

#[test]
fn call_stack_overflow() {
    let code = vec![op(Opcode::LdNil), op(Opcode::Call), 0, op(Opcode::Halt)];
    let mut vm = Vm::new(module_with_code(code));
    assert!(matches!(vm.run(), Err(VmError::CallStackOverflow)));
}

#[test]
fn effect_push_pop() {
    // EffPush: u16 effect_id=0 + i16 skip=5
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
        5,
        0, // skip = 5 bytes (handler body)
        op(Opcode::LdSmi),
        99,
        0,
        op(Opcode::EffCont),
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
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
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
        op(Opcode::EffNeed),
        0,
        0,
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn effect_nested() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
        5,
        0, // skip = 5
        op(Opcode::LdSmi),
        88,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
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
        op(Opcode::EffNeed),
        0,
        0,
        op(Opcode::EffPop),
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 77);
}

#[test]
fn effect_dispatch_by_id() {
    // Handler A (effect=0): resume with 10
    // Handler B (effect=1): resume with 20
    // EffNeed(effect=0) should hit Handler A, not B
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
        5,
        0, // skip = 5 (LdSmi + EffCont)
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::EffPush),
        1,
        0, // effect_id = 1
        5,
        0, // skip = 5
        op(Opcode::LdSmi),
        20,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::EffNeed),
        0,
        0,                  // effect_id = 0 -> should find Handler A
        op(Opcode::EffPop), // clean up Handler B
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 10);
}

#[test]
fn effect_dispatch_skips_wrong_handler() {
    // Handler A (effect=0): resume with 10
    // Handler B (effect=1): resume with 20
    // EffNeed(effect=1) should hit Handler B (topmost matching)
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
        5,
        0, // skip = 5
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::EffPush),
        1,
        0, // effect_id = 1
        5,
        0, // skip = 5
        op(Opcode::LdSmi),
        20,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::EffNeed),
        1,
        0,                  // effect_id = 1 -> should find Handler B
        op(Opcode::EffPop), // clean up Handler A
        op(Opcode::Halt),
    ]));
    assert_eq!(vm.run().unwrap().as_int(), 20);
}

#[test]
fn effect_no_matching_handler() {
    // Only handler is for effect=0, but we need effect=99
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::EffPush),
        0,
        0, // effect_id = 0
        5,
        0, // skip = 5
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::EffCont),
        1,
        op(Opcode::LdSmi),
        0,
        0,
        op(Opcode::EffNeed),
        99,
        0, // effect_id = 99 -> no match
        op(Opcode::Halt),
    ]));
    assert!(matches!(vm.run(), Err(VmError::NoEffectHandler)));
}

// ── Array opcodes ─────────────────────────────────────────────────────────────

#[test]
fn arr_new_and_geti_seti() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        2,
        0,
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::ArrSetI),
        0,
        op(Opcode::ArrGetI),
        0,
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 10);
}

#[test]
fn arr_get_set_dynamic() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::Dup),
        op(Opcode::LdNil),
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::ArrSet),
        op(Opcode::LdNil),
        op(Opcode::ArrGet),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn arr_len() {
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
    let constants = vec![ConstantEntry::Value(Value::from_tag(0))];
    let code = vec![
        op(Opcode::ArrNewT),
        0,
        1,
        0,
        op(Opcode::ArrTag),
        op(Opcode::LdConst),
        0,
        0,
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
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        5,
        0,
        op(Opcode::ArrSetI),
        0,
        op(Opcode::ArrCopy),
        op(Opcode::ArrGetI),
        0,
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 5);
}

#[test]
fn arr_concat() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        10,
        0,
        op(Opcode::ArrSetI),
        0,
        op(Opcode::ArrNew),
        1,
        0,
        op(Opcode::LdSmi),
        20,
        0,
        op(Opcode::ArrSetI),
        0,
        op(Opcode::ArrCaten),
        op(Opcode::ArrLen),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 2);
}

#[test]
fn tychk_int_true() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyChk),
        0xF6,
        0xFF, // BUILTIN_TYPE_INT
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn tycast_int_success() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyCast),
        0xF6,
        0xFF, // BUILTIN_TYPE_INT
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

// ── BrTbl ─────────────────────────────────────────────────────────────────────

#[test]
fn brtbl_in_range() {
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

// ── String tests ──────────────────────────────────────────────────────────────

#[test]
fn ldcst_string_resolves_to_ptr() {
    // StringRef(0) → heap-allocated "hello" → LdConst pushes a PTR
    let module = module_with_strings_and_code(
        vec!["hello".into()],
        vec![ConstantEntry::StringRef(0)],
        vec![op(Opcode::LdConst), 0, 0, op(Opcode::Halt)],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_ptr(), "expected PTR, got {result:?}");
}

#[test]
fn string_cmpeq_same_content() {
    // Two separate StringRef entries pointing at the same string → structural equality
    let module = module_with_strings_and_code(
        vec!["hello".into()],
        vec![ConstantEntry::StringRef(0), ConstantEntry::StringRef(0)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdConst),
            1,
            0,
            op(Opcode::CmpEq),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool(), "identical strings should be equal");
}

#[test]
fn string_cmpeq_different_content() {
    let module = module_with_strings_and_code(
        vec!["hello".into(), "world".into()],
        vec![ConstantEntry::StringRef(0), ConstantEntry::StringRef(1)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdConst),
            1,
            0,
            op(Opcode::CmpEq),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(!result.as_bool(), "different strings should not be equal");
}

#[test]
fn string_cmpneq() {
    let module = module_with_strings_and_code(
        vec!["hello".into(), "world".into()],
        vec![ConstantEntry::StringRef(0), ConstantEntry::StringRef(1)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdConst),
            1,
            0,
            op(Opcode::CmpNeq),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(
        result.as_bool(),
        "CmpNeq on different strings should be true"
    );
}

#[test]
fn string_concat_via_arrconcat() {
    // "hel" ++ "lo" → new heap string
    let module = module_with_strings_and_code(
        vec!["hel".into(), "lo".into()],
        vec![ConstantEntry::StringRef(0), ConstantEntry::StringRef(1)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdConst),
            1,
            0,
            op(Opcode::ArrCaten),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_ptr());
    let display = super::display_value(result, &vm.heap);
    assert_eq!(display, "hello");
}

#[test]
fn string_concat_then_cmpeq() {
    // ("hel" ++ "lo") == "hello" → true
    let module = module_with_strings_and_code(
        vec!["hel".into(), "lo".into(), "hello".into()],
        vec![
            ConstantEntry::StringRef(0),
            ConstantEntry::StringRef(1),
            ConstantEntry::StringRef(2),
        ],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdConst),
            1,
            0,
            op(Opcode::ArrCaten),
            op(Opcode::LdConst),
            2,
            0,
            op(Opcode::CmpEq),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(
        result.as_bool(),
        "concatenated string should equal original"
    );
}

#[test]
fn string_arrlen() {
    let module = module_with_strings_and_code(
        vec!["hello".into()],
        vec![ConstantEntry::StringRef(0)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::ArrLen),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 5);
}

#[test]
fn string_arrget() {
    // "hello"[1] → 'e' → 101
    let module = module_with_strings_and_code(
        vec!["hello".into()],
        vec![ConstantEntry::StringRef(0)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::LdOne),
            op(Opcode::ArrGet),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 101); // 'e'
}

#[test]
fn string_arrset_errors() {
    let module = module_with_strings_and_code(
        vec!["hello".into()],
        vec![ConstantEntry::StringRef(0)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::Dup),
            op(Opcode::LdNil),
            op(Opcode::LdSmi),
            65,
            0,
            op(Opcode::ArrSet),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    assert!(matches!(vm.run(), Err(VmError::TypeError { .. })));
}

#[test]
fn string_arrcopy() {
    // ArrCopy on a string creates a new heap string
    let module = module_with_strings_and_code(
        vec!["test".into()],
        vec![ConstantEntry::StringRef(0)],
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::ArrCopy),
            op(Opcode::ArrLen),
            op(Opcode::Halt),
        ],
    );
    let mut vm = Vm::new(module);
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 4);
}

// ── Type operations ───────────────────────────────────────────────────────────

#[test]
fn ty_chk_int_false_for_float() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyChk),
        0xF7,
        0xFF, // BUILTIN_TYPE_FLOAT
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(!result.as_bool());
}

#[test]
fn ty_chk_bool_true() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdTru),
        op(Opcode::TyChk),
        0xF5,
        0xFF, // BUILTIN_TYPE_BOOL
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn ty_chk_unit() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdUnit),
        op(Opcode::TyChk),
        0xF4,
        0xFF, // BUILTIN_TYPE_UNIT
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_bool());
    assert!(result.as_bool());
}

#[test]
fn ty_cast_success() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyCast),
        0xF6,
        0xFF, // BUILTIN_TYPE_INT
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 42);
}

#[test]
fn ty_cast_failure() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyCast),
        0xF7,
        0xFF, // BUILTIN_TYPE_FLOAT
        op(Opcode::Halt),
    ]));
    assert!(matches!(vm.run(), Err(VmError::TypeCastFailed)));
}

#[test]
fn ty_tag_int() {
    let mut vm = Vm::new(module_with_code(vec![
        op(Opcode::LdSmi),
        42,
        0,
        op(Opcode::TyTag),
        op(Opcode::Halt),
    ]));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 1); // NAN_BOX_SMI = 0b001
}

#[test]
fn ty_tag_float() {
    let constants = vec![ConstantEntry::Value(Value::from_float(3.14))];
    let mut vm = Vm::new(module_with_constants_and_code(
        constants,
        vec![
            op(Opcode::LdConst),
            0,
            0,
            op(Opcode::TyTag),
            op(Opcode::Halt),
        ],
    ));
    let result = vm.run().unwrap();
    assert!(result.is_int());
    assert_eq!(result.as_int(), 0); // Floats have tag 0
}
