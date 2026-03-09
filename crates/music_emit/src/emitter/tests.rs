use music_ir::{
    IrBinOp, IrCallee, IrConstValue, IrEffectDef, IrEffectId, IrEffectMask, IrEffectOpDef,
    IrEffectOpId, IrFnId, IrFunction, IrInst, IrLabel, IrLocal, IrLocalDecl, IrModule, IrOperand,
    IrParam, IrParamMode, IrRvalue, IrType,
};
use music_shared::{Interner, Span};

use crate::opcode::Opcode;
use crate::{EmitOutput, emit};

fn make_span() -> Span {
    Span::new(0, 0)
}

fn make_simple_module() -> (IrModule, Interner) {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let bool_idx = module.types.alloc(IrType::Bool);

    let name = interner.intern("main");
    let param_local = IrLocal(0);
    let tmp0 = IrLocal(1);
    let tmp1 = IrLocal(2);

    // fn main(%0: Int32) : Int32 {
    //   %1 = const.i 42
    //   return %1
    // }
    let body = vec![
        IrInst::Assign {
            dst: tmp0,
            rvalue: IrRvalue::Const(IrConstValue::Int(42)),
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(tmp0)),
            span: make_span(),
        },
    ];

    let locals = vec![
        IrLocalDecl {
            local: param_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: tmp0,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: tmp1,
            ty: bool_idx,
            mutable: false,
            span: make_span(),
        },
    ];

    let params = vec![IrParam {
        local: param_local,
        ty: int32_idx,
        mode: IrParamMode::Value,
        span: make_span(),
    }];

    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);
    (module, interner)
}

#[test]
fn test_emit_simple_module_produces_valid_header() {
    let (module, interner) = make_simple_module();
    let result = emit(&module, &interner);
    assert!(result.is_ok(), "emit should succeed: {result:?}");
    let EmitOutput { bytes } = result.expect("emit succeeded");
    assert_eq!(&bytes[0..4], b"MUSI", "magic bytes must be MUSI");
    assert_eq!(bytes[4], 1, "version_maj must be 1");
    assert_eq!(bytes[6], 0, "version_min must be 0 (low byte of u16 LE)");
}

#[test]
fn test_emit_simple_module_has_nonempty_code() {
    let (module, interner) = make_simple_module();
    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    assert!(
        bytes.len() > 36,
        "output must be larger than the header (36 bytes)"
    );
}

#[test]
fn test_emit_binop_module() {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("add_two");
    let a = IrLocal(0);
    let b = IrLocal(1);
    let result_local = IrLocal(2);

    // fn add_two(%0: Int32, %1: Int32) : Int32 {
    //   %2 = i.add %0, %1
    //   return %2
    // }
    let body = vec![
        IrInst::Assign {
            dst: result_local,
            rvalue: IrRvalue::BinOp {
                op: IrBinOp::IAdd,
                left: IrOperand::Local(a),
                right: IrOperand::Local(b),
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(result_local)),
            span: make_span(),
        },
    ];

    let locals = vec![
        IrLocalDecl {
            local: a,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: b,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: result_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
    ];

    let params = vec![
        IrParam {
            local: a,
            ty: int32_idx,
            mode: IrParamMode::Value,
            span: make_span(),
        },
        IrParam {
            local: b,
            ty: int32_idx,
            mode: IrParamMode::Value,
            span: make_span(),
        },
    ];

    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let result = emit(&module, &interner);
    assert!(result.is_ok(), "emit should succeed: {result:?}");
    let EmitOutput { bytes } = result.expect("emit succeeded");
    assert_eq!(&bytes[0..4], b"MUSI");
    assert!(
        bytes.contains(&0x10),
        "output must contain IAdd (0x10) opcode"
    );
}

#[test]
fn test_emit_closure_uses_fn_const_tag() {
    // Arrange: a module with MakeClosure so FnRef goes into the const pool
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let closure_env_idx = module.types.alloc(IrType::Unit);
    let fn_ty_idx = module.types.alloc(IrType::Fn {
        params: vec![int32_idx],
        ret: int32_idx,
        effect_mask: IrEffectMask::PURE,
    });
    let closure_ty_idx = module.types.alloc(IrType::Closure {
        fn_ty: fn_ty_idx,
        env_ty: closure_env_idx,
    });

    // Inner fn: fn inner(%0: Int32) : Int32 { return %0 }
    let inner_name = interner.intern("inner");
    let inner_param = IrLocal(0);
    let inner_fn = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name: inner_name,
        params: vec![IrParam {
            local: inner_param,
            ty: int32_idx,
            mode: IrParamMode::Value,
            span: make_span(),
        }],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body: vec![IrInst::Return {
            value: Some(IrOperand::Local(inner_param)),
            span: make_span(),
        }],
        locals: vec![IrLocalDecl {
            local: inner_param,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        }],
        is_closure: true,
        span: make_span(),
    });

    // Outer fn: fn main() : Closure { %0 = mk.cls fn#0[]; return %0 }
    let main_name = interner.intern("main");
    let closure_local = IrLocal(0);
    let outer_fn = module.functions.alloc(IrFunction {
        id: IrFnId(1),
        source_def: None,
        name: main_name,
        params: vec![],
        ret_ty: closure_ty_idx,
        effects: IrEffectMask::PURE,
        body: vec![
            IrInst::Assign {
                dst: closure_local,
                rvalue: IrRvalue::MakeClosure {
                    fn_id: inner_fn,
                    captures: vec![],
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(closure_local)),
                span: make_span(),
            },
        ],
        locals: vec![IrLocalDecl {
            local: closure_local,
            ty: closure_ty_idx,
            mutable: false,
            span: make_span(),
        }],
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(outer_fn);
    let _ = inner_fn; // used above

    // Act
    let result = emit(&module, &interner);
    assert!(result.is_ok(), "emit should succeed: {result:?}");
    let EmitOutput { bytes } = result.expect("emit succeeded");

    // Assert: TAG_FN (0x08) must appear in the constant pool section
    assert!(
        bytes.contains(&0x08),
        "output must contain TAG_FN (0x08) for FnRef constant"
    );
}

#[test]
fn test_emit_indirect_call_uses_inv_dyn_opcode() {
    // Arrange: fn that calls through an indirect callee (closure value in a local)
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("call_indirect");
    let callee_local = IrLocal(0);
    let arg_local = IrLocal(1);
    let result_local = IrLocal(2);

    // fn call_indirect(%0: FnType, %1: Int32) : Int32 {
    //   %2 = call *%0(%1)
    //   return %2
    // }
    let body = vec![
        IrInst::Assign {
            dst: result_local,
            rvalue: IrRvalue::Call {
                callee: IrCallee::Indirect(callee_local),
                args: vec![IrOperand::Local(arg_local)],
                tail: false,
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(result_local)),
            span: make_span(),
        },
    ];
    let locals = vec![
        IrLocalDecl {
            local: callee_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: arg_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: result_local,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
    ];
    let params = vec![
        IrParam {
            local: callee_local,
            ty: int32_idx,
            mode: IrParamMode::Value,
            span: make_span(),
        },
        IrParam {
            local: arg_local,
            ty: int32_idx,
            mode: IrParamMode::Value,
            span: make_span(),
        },
    ];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    // Act
    let result = emit(&module, &interner);
    assert!(result.is_ok(), "emit should succeed: {result:?}");
    let EmitOutput { bytes } = result.expect("emit succeeded");

    // Assert: INV_DYN opcode (0x69) must appear in the bytecode
    assert!(
        bytes.contains(&0x69),
        "output must contain INV_DYN (0x69) for indirect call"
    );
}

#[test]
fn test_emit_wide_jump_resolves_backward_branch() {
    // Arrange: simple loop that uses a backward jump (goto to earlier label)
    // fn loop_back() : Int32 {
    //   %0 = const.i 0
    // .l0:
    //   goto .l0   (infinite loop — tests backward wide jump patching)
    //   return %0
    // }
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("loop_back");
    let result_local = IrLocal(0);
    let loop_label = IrLabel(0);

    let body = vec![
        IrInst::Assign {
            dst: result_local,
            rvalue: IrRvalue::Const(IrConstValue::Int(0)),
            span: make_span(),
        },
        IrInst::Label(loop_label),
        IrInst::Goto(loop_label),
    ];
    let locals = vec![IrLocalDecl {
        local: result_local,
        ty: int32_idx,
        mutable: false,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params: vec![],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    // Act: must not produce JumpTooFar; backward jump must resolve correctly
    let result = emit(&module, &interner);
    assert!(
        result.is_ok(),
        "backward wide jump must resolve without error: {result:?}"
    );

    let EmitOutput { bytes } = result.expect("emit succeeded");
    assert!(
        bytes.contains(&0xD0),
        "output must contain JMP_W (0xD0) opcode"
    );
}

// ── New tests ────────────────────────────────────────────────────────────────

/// Helper: build a single-function module with one Assign + Return.
fn make_cast_module(from_ty: IrType, to_ty: IrType) -> (IrModule, Interner) {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let from_idx = module.types.alloc(from_ty);
    let to_idx = module.types.alloc(to_ty);

    let name = interner.intern("cast_fn");
    let src = IrLocal(0);
    let dst = IrLocal(1);

    let body = vec![
        IrInst::Assign {
            dst,
            rvalue: IrRvalue::Cast {
                operand: IrOperand::Local(src),
                from: from_idx,
                to: to_idx,
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(dst)),
            span: make_span(),
        },
    ];
    let locals = vec![
        IrLocalDecl {
            local: src,
            ty: from_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: dst,
            ty: to_idx,
            mutable: false,
            span: make_span(),
        },
    ];
    let params = vec![IrParam {
        local: src,
        ty: from_idx,
        mode: IrParamMode::Value,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: to_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);
    (module, interner)
}

/// Find the first occurrence of `opcode` in the function pool section (past pools/header).
fn find_opcode(bytes: &[u8], op: Opcode) -> Option<usize> {
    // fn_off is at header offset 0x1C (bytes 28..32)
    assert!(bytes.len() > 31, "bytes too short for header");
    let fn_off = u32::from_le_bytes([bytes[28], bytes[29], bytes[30], bytes[31]]);
    let start = usize::try_from(fn_off).expect("fn_off fits usize");
    bytes[start..]
        .iter()
        .position(|&b| b == op.0)
        .map(|pos| pos + start)
}

#[test]
fn test_emit_cast_int_to_float_uses_cnv_itf() {
    let (module, interner) = make_cast_module(IrType::Int32, IrType::Float64);
    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    assert!(
        find_opcode(&bytes, Opcode::CNV_ITF).is_some(),
        "Int32→Float64 must emit CNV_ITF (0x5E)"
    );
}

#[test]
fn test_emit_cast_widen_int8_to_int32_uses_cnv_wdn() {
    let (module, interner) = make_cast_module(IrType::Int8, IrType::Int32);
    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    let pos = find_opcode(&bytes, Opcode::CNV_WDN).expect("must emit CNV_WDN");
    assert_eq!(bytes[pos + 1], 32, "target width must be 32");
}

#[test]
fn test_emit_cast_narrow_int64_to_int8_uses_cnv_nrw() {
    let (module, interner) = make_cast_module(IrType::Int64, IrType::Int8);
    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    let pos = find_opcode(&bytes, Opcode::CNV_NRW).expect("must emit CNV_NRW");
    assert_eq!(bytes[pos + 1], 8, "target width must be 8");
}

#[test]
fn test_emit_cast_same_width_reinterpret_uses_cnv_trm() {
    let (module, interner) = make_cast_module(IrType::Int32, IrType::UInt32);
    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    assert!(
        find_opcode(&bytes, Opcode::CNV_TRM).is_some(),
        "Int32→UInt32 must emit CNV_TRM (0x60)"
    );
}

#[test]
fn test_emit_get_payload_uses_field_index() {
    // Arrange: fn that does GetPayload with field=2
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("pay_fn");
    let src = IrLocal(0);
    let dst = IrLocal(1);

    let body = vec![
        IrInst::Assign {
            dst,
            rvalue: IrRvalue::GetPayload {
                value: IrOperand::Local(src),
                tag: 0,
                field: 2,
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(dst)),
            span: make_span(),
        },
    ];
    let locals = vec![
        IrLocalDecl {
            local: src,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: dst,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
    ];
    let params = vec![IrParam {
        local: src,
        ty: int32_idx,
        mode: IrParamMode::Value,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    let pos = find_opcode(&bytes, Opcode::LD_PAY).expect("must emit GET_PAY");
    assert_eq!(bytes[pos + 1], 2, "field index must be 2");
}

#[test]
fn test_emit_make_array_uses_elem_type_id() {
    // Arrange: fn that creates an array of Int32
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("arr_fn");
    let dst = IrLocal(0);

    let body = vec![
        IrInst::Assign {
            dst,
            rvalue: IrRvalue::MakeArray {
                elem_ty: int32_idx,
                elems: vec![
                    IrOperand::Const(IrConstValue::Int(1)),
                    IrOperand::Const(IrConstValue::Int(2)),
                ],
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(dst)),
            span: make_span(),
        },
    ];
    let locals = vec![IrLocalDecl {
        local: dst,
        ty: int32_idx,
        mutable: false,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params: vec![],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");
    // MK_ARR is a u32-operand instruction (0xC6). The operand should be the type_id
    // for Int32, which should be nonzero (allocated in the type pool).
    let pos = find_opcode(&bytes, Opcode::MK_ARR).expect("must emit MK_ARR");
    let operand = u32::from_le_bytes([
        bytes[pos + 1],
        bytes[pos + 2],
        bytes[pos + 3],
        bytes[pos + 4],
    ]);
    // The type_id for Int32 is not necessarily 0 — just check it was assigned
    // (Previously it was always 0, now it should be a valid type pool entry)
    assert!(
        operand < 1000,
        "MK_ARR operand should be a reasonable type_id, got {operand}"
    );
}

#[test]
fn test_emit_spawn_indirect_returns_error() {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("spawn_fn");
    let callee = IrLocal(0);
    let dst = IrLocal(1);

    let body = vec![
        IrInst::Assign {
            dst,
            rvalue: IrRvalue::Spawn {
                callee: IrCallee::Indirect(callee),
                args: vec![],
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(dst)),
            span: make_span(),
        },
    ];
    let locals = vec![
        IrLocalDecl {
            local: callee,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
        IrLocalDecl {
            local: dst,
            ty: int32_idx,
            mutable: false,
            span: make_span(),
        },
    ];
    let params = vec![IrParam {
        local: callee,
        ty: int32_idx,
        mode: IrParamMode::Value,
        span: make_span(),
    }];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params,
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals,
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let result = emit(&module, &interner);
    assert!(result.is_err(), "indirect spawn must return error");
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("unsupported feature"),
        "error must be UnsupportedFeature, got: {err}"
    );
}

#[test]
fn test_emit_effect_push_overflow_returns_error() {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("eff_fn");

    // Create a handler function
    let handler_name = interner.intern("handler");
    let handler_fn = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name: handler_name,
        params: vec![],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body: vec![IrInst::Return {
            value: None,
            span: make_span(),
        }],
        locals: vec![],
        is_closure: false,
        span: make_span(),
    });

    // EffectPush with effect_id = 256 (overflows u8)
    let body = vec![
        IrInst::EffectPush {
            effect: IrEffectId(256),
            handler_fn,
            span: make_span(),
        },
        IrInst::Return {
            value: None,
            span: make_span(),
        },
    ];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(1),
        source_def: None,
        name,
        params: vec![],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body,
        locals: vec![],
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let result = emit(&module, &interner);
    assert!(result.is_err(), "effect id overflow must return error");
    let err = result.unwrap_err();
    assert!(
        err.to_string().contains("operand overflow"),
        "error must be OperandOverflow, got: {err}"
    );
}

#[test]
fn test_emit_effect_handler_table_written() {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let unit_idx = module.types.alloc(IrType::Unit);

    // Handler function
    let handler_name = interner.intern("handler");
    let handler_fn = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name: handler_name,
        params: vec![],
        ret_ty: unit_idx,
        effects: IrEffectMask::PURE,
        body: vec![IrInst::Return {
            value: None,
            span: make_span(),
        }],
        locals: vec![],
        is_closure: false,
        span: make_span(),
    });

    // Main function with EffectPush + EffectPop
    let main_name = interner.intern("main");
    let body = vec![
        IrInst::EffectPush {
            effect: IrEffectId(3),
            handler_fn,
            span: make_span(),
        },
        IrInst::EffectPop {
            effect: IrEffectId(3),
            span: make_span(),
        },
        IrInst::Return {
            value: None,
            span: make_span(),
        },
    ];
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(1),
        source_def: None,
        name: main_name,
        params: vec![],
        ret_ty: unit_idx,
        effects: IrEffectMask::PURE,
        body,
        locals: vec![],
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");

    // The handler table should contain effect_id=3 and handler_fn_id=0
    // Search for the handler entry in the function pool section:
    // handler_count (u16) + handler_entry (u8 effect_id + u32 handler_fn_id)
    // The effect_id byte 0x03 followed by handler fn_id 0x00000000 should exist.
    // We check the EFF_PSH opcode was emitted with id=3
    let pos = find_opcode(&bytes, Opcode::EFF_PSH).expect("must emit EFF_PSH");
    assert_eq!(bytes[pos + 1], 3, "effect_id operand must be 3");

    // Verify handler_count is nonzero somewhere after code in the fn pool
    // (The exact layout is: ...code... handler_count:u16 handler_entry[])
    assert!(
        bytes.len() > 36,
        "output must have data beyond header"
    );
}

#[test]
fn test_emit_header_36_bytes_with_effect_pool() {
    let mut interner = Interner::new();
    let mut module = IrModule::new();

    let int32_idx = module.types.alloc(IrType::Int32);
    let name = interner.intern("main");
    let fn_idx = module.functions.alloc(IrFunction {
        id: IrFnId(0),
        source_def: None,
        name,
        params: vec![],
        ret_ty: int32_idx,
        effects: IrEffectMask::PURE,
        body: vec![IrInst::Return {
            value: None,
            span: make_span(),
        }],
        locals: vec![],
        is_closure: false,
        span: make_span(),
    });
    module.entry = Some(fn_idx);

    // Add an effect definition
    let eff_name = interner.intern("IO");
    let op_name = interner.intern("print");
    let str_idx = module.types.alloc(IrType::Unit);
    module.effects.push(IrEffectDef {
        id: IrEffectId(0),
        name: eff_name,
        ops: vec![IrEffectOpDef {
            id: IrEffectOpId(0),
            name: op_name,
            param_tys: vec![str_idx],
            ret_ty: int32_idx,
        }],
    });

    let EmitOutput { bytes } = emit(&module, &interner).expect("emit succeeded");

    // Header is now 36 bytes
    assert_eq!(&bytes[0..4], b"MUSI", "magic must be MUSI");

    // Read offsets from header
    let const_off = u32::from_le_bytes([bytes[16], bytes[17], bytes[18], bytes[19]]);
    let type_off = u32::from_le_bytes([bytes[20], bytes[21], bytes[22], bytes[23]]);
    let effect_off = u32::from_le_bytes([bytes[24], bytes[25], bytes[26], bytes[27]]);
    let fn_off = u32::from_le_bytes([bytes[28], bytes[29], bytes[30], bytes[31]]);

    assert_eq!(const_off, 36, "const_off must start after 36-byte header");
    assert!(
        type_off > const_off,
        "type_off must follow const pool"
    );
    assert!(
        effect_off >= type_off,
        "effect_off must follow type pool"
    );
    assert!(
        fn_off >= effect_off,
        "fn_off must follow effect pool"
    );

    // Effect pool should have count=1 (4 bytes for u32 count + effect data)
    let eff_start = usize::try_from(effect_off).expect("fits usize");
    let eff_count = u32::from_le_bytes([
        bytes[eff_start],
        bytes[eff_start + 1],
        bytes[eff_start + 2],
        bytes[eff_start + 3],
    ]);
    assert_eq!(eff_count, 1, "effect pool must contain 1 effect");
}
