use music_ir::{
    IrBinOp, IrCallee, IrConstValue, IrEffectDef, IrEffectId, IrEffectMask, IrEffectOpDef,
    IrEffectOpId, IrFnId, IrFunction, IrInst, IrLabel, IrLocal, IrLocalDecl, IrModule, IrOperand,
    IrParam, IrParamMode, IrRvalue, IrType,
};
use music_shared::{Idx, Interner, Span, Symbol};

use crate::error::EmitError;
use crate::opcode::Opcode;
use crate::{EmitOutput, emit};

// ── Helpers ──────────────────────────────────────────────────────────────────

fn make_span() -> Span {
    Span::new(0, 0)
}

fn make_param(local: IrLocal, ty: Idx<IrType>) -> IrParam {
    IrParam {
        local,
        ty,
        mode: IrParamMode::Value,
        span: make_span(),
    }
}

fn make_local(local: IrLocal, ty: Idx<IrType>, mutable: bool) -> IrLocalDecl {
    IrLocalDecl {
        local,
        ty,
        mutable,
        span: make_span(),
    }
}

/// Find the first occurrence of `op` in the function pool section (past header/pools).
fn find_opcode(bytes: &[u8], op: Opcode) -> Option<usize> {
    // `fn_off` is at header offset 0x1C (bytes 28..32)
    assert!(bytes.len() > 31, "bytes too short for header");
    let fn_off = u32::from_le_bytes([bytes[28], bytes[29], bytes[30], bytes[31]]);
    let start = usize::try_from(fn_off).expect("fn_off fits usize");
    bytes[start..]
        .iter()
        .position(|&b| b == op.0)
        .map(|pos| pos + start)
}

// ── TestModule builder ───────────────────────────────────────────────────────

/// Builder for test modules with common boilerplate.
struct TestModule {
    module: IrModule,
    interner: Interner,
    next_fn_id: u32,
}

impl TestModule {
    fn new() -> Self {
        Self {
            module: IrModule::new(),
            interner: Interner::new(),
            next_fn_id: 0,
        }
    }

    fn int32(&mut self) -> Idx<IrType> {
        self.module.types.alloc(IrType::Int32)
    }

    fn unit(&mut self) -> Idx<IrType> {
        self.module.types.alloc(IrType::Unit)
    }

    fn bool_ty(&mut self) -> Idx<IrType> {
        self.module.types.alloc(IrType::Bool)
    }

    fn alloc_type(&mut self, ty: IrType) -> Idx<IrType> {
        self.module.types.alloc(ty)
    }

    fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    fn add_fn(
        &mut self,
        name: &str,
        params: Vec<IrParam>,
        ret_ty: Idx<IrType>,
        body: Vec<IrInst>,
        locals: Vec<IrLocalDecl>,
    ) -> Idx<IrFunction> {
        self.add_fn_ext(name, params, ret_ty, body, locals, false)
    }

    fn add_fn_ext(
        &mut self,
        name: &str,
        params: Vec<IrParam>,
        ret_ty: Idx<IrType>,
        body: Vec<IrInst>,
        locals: Vec<IrLocalDecl>,
        is_closure: bool,
    ) -> Idx<IrFunction> {
        let sym = self.interner.intern(name);
        let id = IrFnId(self.next_fn_id);
        self.next_fn_id += 1;
        self.module.functions.alloc(IrFunction {
            id,
            source_def: None,
            name: sym,
            params,
            ret_ty,
            effects: IrEffectMask::PURE,
            body,
            locals,
            is_closure,
            span: make_span(),
        })
    }

    fn set_entry(&mut self, idx: Idx<IrFunction>) {
        self.module.entry = Some(idx);
    }

    fn emit(self) -> Result<EmitOutput, EmitError> {
        emit(&self.module, &self.interner)
    }
}

// ── Shared module factories ──────────────────────────────────────────────────

fn make_simple_module() -> TestModule {
    let mut tm = TestModule::new();
    let int32 = tm.int32();
    let bool_ty = tm.bool_ty();

    let body = vec![
        IrInst::Assign {
            dst: IrLocal(1),
            rvalue: IrRvalue::Const(IrConstValue::Int(42)),
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(IrLocal(1))),
            span: make_span(),
        },
    ];
    let locals = vec![
        make_local(IrLocal(0), int32, false),
        make_local(IrLocal(1), int32, false),
        make_local(IrLocal(2), bool_ty, false),
    ];
    let params = vec![make_param(IrLocal(0), int32)];
    let fn_idx = tm.add_fn("main", params, int32, body, locals);
    tm.set_entry(fn_idx);
    tm
}

fn make_cast_module(from_ty: IrType, to_ty: IrType) -> TestModule {
    let mut tm = TestModule::new();
    let from_idx = tm.alloc_type(from_ty);
    let to_idx = tm.alloc_type(to_ty);

    let body = vec![
        IrInst::Assign {
            dst: IrLocal(1),
            rvalue: IrRvalue::Cast {
                operand: IrOperand::Local(IrLocal(0)),
                from: from_idx,
                to: to_idx,
            },
            span: make_span(),
        },
        IrInst::Return {
            value: Some(IrOperand::Local(IrLocal(1))),
            span: make_span(),
        },
    ];
    let locals = vec![
        make_local(IrLocal(0), from_idx, false),
        make_local(IrLocal(1), to_idx, false),
    ];
    let params = vec![make_param(IrLocal(0), from_idx)];
    let fn_idx = tm.add_fn("cast_fn", params, to_idx, body, locals);
    tm.set_entry(fn_idx);
    tm
}

// ── Test modules ─────────────────────────────────────────────────────────────

mod header {
    use super::*;

    #[test]
    fn test_emit_simple_module_produces_valid_header() {
        let tm = make_simple_module();
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert_eq!(&bytes[0..4], b"MUSI", "magic bytes must be MUSI");
        assert_eq!(bytes[4], 1, "version_maj must be 1");
        assert_eq!(bytes[6], 0, "version_min must be 0 (low byte of u16 LE)");
    }

    #[test]
    fn test_emit_simple_module_has_nonempty_code() {
        let tm = make_simple_module();
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            bytes.len() > 36,
            "output must be larger than the header (36 bytes)"
        );
    }

    #[test]
    fn test_emit_header_36_bytes_with_effect_pool() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let fn_idx = tm.add_fn(
            "main",
            vec![],
            int32,
            vec![IrInst::Return {
                value: None,
                span: make_span(),
            }],
            vec![],
        );
        tm.set_entry(fn_idx);

        // Add an effect definition
        let eff_name = tm.intern("IO");
        let op_name = tm.intern("print");
        let str_idx = tm.unit();
        tm.module.effects.push(IrEffectDef {
            id: IrEffectId(0),
            name: eff_name,
            ops: vec![IrEffectOpDef {
                id: IrEffectOpId(0),
                name: op_name,
                param_tys: vec![str_idx],
                ret_ty: int32,
            }],
        });

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");

        // Header is now 36 bytes
        assert_eq!(&bytes[0..4], b"MUSI", "magic must be MUSI");

        // Read offsets from header
        let const_off = u32::from_le_bytes([bytes[16], bytes[17], bytes[18], bytes[19]]);
        let type_off = u32::from_le_bytes([bytes[20], bytes[21], bytes[22], bytes[23]]);
        let effect_off = u32::from_le_bytes([bytes[24], bytes[25], bytes[26], bytes[27]]);
        let fn_off = u32::from_le_bytes([bytes[28], bytes[29], bytes[30], bytes[31]]);

        assert_eq!(const_off, 36, "const_off must start after 36-byte header");
        assert!(type_off > const_off, "type_off must follow const pool");
        assert!(effect_off >= type_off, "effect_off must follow type pool");
        assert!(fn_off >= effect_off, "fn_off must follow effect pool");

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
}

mod code {
    use super::*;

    #[test]
    fn test_emit_binop_module() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(2),
                rvalue: IrRvalue::BinOp {
                    op: IrBinOp::IAdd,
                    left: IrOperand::Local(IrLocal(0)),
                    right: IrOperand::Local(IrLocal(1)),
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(2))),
                span: make_span(),
            },
        ];
        let locals = vec![
            make_local(IrLocal(0), int32, false),
            make_local(IrLocal(1), int32, false),
            make_local(IrLocal(2), int32, false),
        ];
        let params = vec![make_param(IrLocal(0), int32), make_param(IrLocal(1), int32)];
        let fn_idx = tm.add_fn("add_two", params, int32, body, locals);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert_eq!(&bytes[0..4], b"MUSI");
        assert!(
            bytes.contains(&0x10),
            "output must contain IAdd (0x10) opcode"
        );
    }

    #[test]
    fn test_emit_cast_int_to_float_uses_cnv_itf() {
        let tm = make_cast_module(IrType::Int32, IrType::Float64);
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            find_opcode(&bytes, Opcode::CNV_ITF).is_some(),
            "Int32→Float64 must emit CNV_ITF (0x5E)"
        );
    }

    #[test]
    fn test_emit_cast_widen_int8_to_int32_uses_cnv_wdn() {
        let tm = make_cast_module(IrType::Int8, IrType::Int32);
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        let pos = find_opcode(&bytes, Opcode::CNV_WDN).expect("must emit CNV_WDN");
        assert_eq!(bytes[pos + 1], 32, "target width must be 32");
    }

    #[test]
    fn test_emit_cast_narrow_int64_to_int8_uses_cnv_nrw() {
        let tm = make_cast_module(IrType::Int64, IrType::Int8);
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        let pos = find_opcode(&bytes, Opcode::CNV_NRW).expect("must emit CNV_NRW");
        assert_eq!(bytes[pos + 1], 8, "target width must be 8");
    }

    #[test]
    fn test_emit_cast_same_width_reinterpret_uses_cnv_trm() {
        let tm = make_cast_module(IrType::Int32, IrType::UInt32);
        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            find_opcode(&bytes, Opcode::CNV_TRM).is_some(),
            "Int32→UInt32 must emit CNV_TRM (0x60)"
        );
    }

    #[test]
    fn test_emit_get_payload_uses_field_index() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(1),
                rvalue: IrRvalue::GetPayload {
                    value: IrOperand::Local(IrLocal(0)),
                    tag: 0,
                    field: 2,
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(1))),
                span: make_span(),
            },
        ];
        let locals = vec![
            make_local(IrLocal(0), int32, false),
            make_local(IrLocal(1), int32, false),
        ];
        let params = vec![make_param(IrLocal(0), int32)];
        let fn_idx = tm.add_fn("pay_fn", params, int32, body, locals);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        let pos = find_opcode(&bytes, Opcode::LD_PAY).expect("must emit GET_PAY");
        assert_eq!(bytes[pos + 1], 2, "field index must be 2");
    }

    #[test]
    fn test_emit_make_array_uses_elem_type_id() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(0),
                rvalue: IrRvalue::MakeArray {
                    elem_ty: int32,
                    elems: vec![
                        IrOperand::Const(IrConstValue::Int(1)),
                        IrOperand::Const(IrConstValue::Int(2)),
                    ],
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(0))),
                span: make_span(),
            },
        ];
        let locals = vec![make_local(IrLocal(0), int32, false)];
        let fn_idx = tm.add_fn("arr_fn", vec![], int32, body, locals);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        let pos = find_opcode(&bytes, Opcode::MK_ARR).expect("must emit MK_ARR");
        let operand = u32::from_le_bytes([
            bytes[pos + 1],
            bytes[pos + 2],
            bytes[pos + 3],
            bytes[pos + 4],
        ]);
        assert!(
            operand < 1000,
            "MK_ARR operand should be a reasonable type_id, got {operand}"
        );
    }

    #[test]
    fn test_emit_closure_uses_fn_const_tag() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();
        let closure_env = tm.unit();
        let fn_ty = tm.alloc_type(IrType::Fn {
            params: vec![int32],
            ret: int32,
            effect_mask: IrEffectMask::PURE,
        });
        let closure_ty = tm.alloc_type(IrType::Closure {
            fn_ty,
            env_ty: closure_env,
        });

        // Inner fn: fn inner(%0: Int32) : Int32 { return %0 }
        let inner_fn = tm.add_fn_ext(
            "inner",
            vec![make_param(IrLocal(0), int32)],
            int32,
            vec![IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(0))),
                span: make_span(),
            }],
            vec![make_local(IrLocal(0), int32, false)],
            true,
        );

        // Outer fn: fn main() : Closure { %0 = mk.cls fn#0[]; return %0 }
        let outer_body = vec![
            IrInst::Assign {
                dst: IrLocal(0),
                rvalue: IrRvalue::MakeClosure {
                    fn_id: inner_fn,
                    captures: vec![],
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(0))),
                span: make_span(),
            },
        ];
        let outer_locals = vec![make_local(IrLocal(0), closure_ty, false)];
        let outer_fn = tm.add_fn("main", vec![], closure_ty, outer_body, outer_locals);
        tm.set_entry(outer_fn);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            bytes.contains(&0x08),
            "output must contain TAG_FN (0x08) for FnRef constant"
        );
    }

    #[test]
    fn test_emit_indirect_call_uses_inv_dyn_opcode() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(2),
                rvalue: IrRvalue::Call {
                    callee: IrCallee::Indirect(IrLocal(0)),
                    args: vec![IrOperand::Local(IrLocal(1))],
                    tail: false,
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(2))),
                span: make_span(),
            },
        ];
        let locals = vec![
            make_local(IrLocal(0), int32, false),
            make_local(IrLocal(1), int32, false),
            make_local(IrLocal(2), int32, false),
        ];
        let params = vec![make_param(IrLocal(0), int32), make_param(IrLocal(1), int32)];
        let fn_idx = tm.add_fn("call_indirect", params, int32, body, locals);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            bytes.contains(&0x69),
            "output must contain INV_DYN (0x69) for indirect call"
        );
    }

    #[test]
    fn test_emit_wide_jump_resolves_backward_branch() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(0),
                rvalue: IrRvalue::Const(IrConstValue::Int(0)),
                span: make_span(),
            },
            IrInst::Label(IrLabel(0)),
            IrInst::Goto(IrLabel(0)),
        ];
        let locals = vec![make_local(IrLocal(0), int32, false)];
        let fn_idx = tm.add_fn("loop_back", vec![], int32, body, locals);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");
        assert!(
            bytes.contains(&0xD0),
            "output must contain JMP_W (0xD0) opcode"
        );
    }
}

mod effect {
    use super::*;

    #[test]
    fn test_emit_effect_push_overflow_returns_error() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        // Create a handler function
        let handler_fn = tm.add_fn(
            "handler",
            vec![],
            int32,
            vec![IrInst::Return {
                value: None,
                span: make_span(),
            }],
            vec![],
        );

        // EffectPush with `effect_id` = 256 (overflows u8)
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
        let fn_idx = tm.add_fn("eff_fn", vec![], int32, body, vec![]);
        tm.set_entry(fn_idx);

        let result = tm.emit();
        assert!(result.is_err(), "effect id overflow must return error");
        let err = result.expect_err("expected error");
        assert!(
            err.to_string().contains("operand overflow"),
            "error must be OperandOverflow, got: {err}"
        );
    }

    #[test]
    fn test_emit_effect_handler_table_written() {
        let mut tm = TestModule::new();
        let unit = tm.unit();

        // Handler function
        let handler_fn = tm.add_fn(
            "handler",
            vec![],
            unit,
            vec![IrInst::Return {
                value: None,
                span: make_span(),
            }],
            vec![],
        );

        // Main function with EffectPush + EffectPop
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
        let fn_idx = tm.add_fn("main", vec![], unit, body, vec![]);
        tm.set_entry(fn_idx);

        let EmitOutput { bytes } = tm.emit().expect("emit succeeded");

        let pos = find_opcode(&bytes, Opcode::EFF_PSH).expect("must emit EFF_PSH");
        assert_eq!(bytes[pos + 1], 3, "effect_id operand must be 3");

        assert!(bytes.len() > 36, "output must have data beyond header");
    }
}

mod error {
    use super::*;

    #[test]
    fn test_emit_spawn_indirect_returns_error() {
        let mut tm = TestModule::new();
        let int32 = tm.int32();

        let body = vec![
            IrInst::Assign {
                dst: IrLocal(1),
                rvalue: IrRvalue::Spawn {
                    callee: IrCallee::Indirect(IrLocal(0)),
                    args: vec![],
                },
                span: make_span(),
            },
            IrInst::Return {
                value: Some(IrOperand::Local(IrLocal(1))),
                span: make_span(),
            },
        ];
        let locals = vec![
            make_local(IrLocal(0), int32, false),
            make_local(IrLocal(1), int32, false),
        ];
        let params = vec![make_param(IrLocal(0), int32)];
        let fn_idx = tm.add_fn("spawn_fn", params, int32, body, locals);
        tm.set_entry(fn_idx);

        let result = tm.emit();
        assert!(result.is_err(), "indirect spawn must return error");
        let err = result.expect_err("expected error");
        assert!(
            err.to_string().contains("unsupported feature"),
            "error must be UnsupportedFeature, got: {err}"
        );
    }
}
