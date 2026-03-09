use music_ir::{
    IrBinOp, IrCallee, IrConstValue, IrEffectMask, IrFnId, IrFunction, IrInst, IrLabel, IrLocal,
    IrLocalDecl, IrModule, IrOperand, IrParam, IrParamMode, IrRvalue, IrType,
};
use music_shared::{Interner, Span};

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
        bytes.len() > 32,
        "output must be larger than the header (32 bytes)"
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
        bytes.contains(&0xD3),
        "output must contain JMP_W (0xD3) opcode"
    );
}
