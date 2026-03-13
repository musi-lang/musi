//! Per-function bytecode emitter.
//!
//! Manages local slot allocation, label targets, forward-jump fixups,
//! and the code buffer for one function.

use std::collections::HashMap;

use crate::error::EmitError;
use musi_bc::{Opcode, encode_i32, encode_no_operand, encode_u8, encode_u16, encode_u32};

/// Fixup record: a forward jump that needs patching once we know the label target.
struct Fixup {
    instr_offset: usize,
    instr_len: usize,
    label: u32,
}

/// Handler table entry for effect push instructions.
pub struct HandlerEntry {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

pub struct FnEmitter {
    pub code: Vec<u8>,
    label_targets: HashMap<u32, usize>,
    fixups: Vec<Fixup>,
    stack_depth: i32,
    pub max_stack: u16,
    pub local_count: u16,
    pub param_count: u16,
    pub handlers: Vec<HandlerEntry>,
}

impl FnEmitter {
    pub fn new(param_count: u16, local_count: u16) -> Self {
        Self {
            code: vec![],
            label_targets: HashMap::new(),
            fixups: vec![],
            stack_depth: 0,
            max_stack: 0,
            local_count,
            param_count,
            handlers: vec![],
        }
    }

    pub fn push_n(&mut self, n: i32) {
        self.stack_depth += n;
        if self.stack_depth > 0 {
            let d = u16::try_from(self.stack_depth).unwrap_or(u16::MAX);
            if d > self.max_stack {
                self.max_stack = d;
            }
        }
    }

    pub const fn pop_n(&mut self, n: i32) {
        self.stack_depth -= n;
    }

    /// Allocate a new local slot and return its index.
    pub fn alloc_local(&mut self) -> u32 {
        let slot = u32::from(self.local_count);
        self.local_count += 1;
        slot
    }

    /// Emit `ld.loc` or `ld.loc.w` depending on slot index.
    pub fn emit_ld_loc(&mut self, slot: u32) {
        if let Ok(s) = u8::try_from(slot) {
            encode_u8(&mut self.code, Opcode::LD_LOC, s);
        } else {
            let s = u16::try_from(slot).expect("slot fits in u16");
            encode_u16(&mut self.code, Opcode::LD_LOC_W, s);
        }
        self.push_n(1);
    }

    /// Emit `st.loc` or `st.loc.w` depending on slot index.
    pub fn emit_st_loc(&mut self, slot: u32) {
        if let Ok(s) = u8::try_from(slot) {
            encode_u8(&mut self.code, Opcode::ST_LOC, s);
        } else {
            let s = u16::try_from(slot).expect("slot fits in u16");
            encode_u16(&mut self.code, Opcode::ST_LOC_W, s);
        }
        self.pop_n(1);
    }

    /// Emit `ld.cst` or `ld.cst.w` depending on const pool index.
    pub fn emit_ld_cst(&mut self, idx: u16) {
        if let Ok(i) = u8::try_from(idx) {
            encode_u8(&mut self.code, Opcode::LD_CST, i);
        } else {
            encode_u16(&mut self.code, Opcode::LD_CST_W, idx);
        }
        self.push_n(1);
    }

    pub fn emit_dup(&mut self) {
        encode_no_operand(&mut self.code, Opcode::DUP);
        self.push_n(1);
    }

    pub fn emit_pop(&mut self) {
        encode_no_operand(&mut self.code, Opcode::POP);
        self.pop_n(1);
    }

    pub fn emit_ret(&mut self) {
        encode_no_operand(&mut self.code, Opcode::RET);
        self.stack_depth = 0;
    }

    pub fn emit_ret_u(&mut self) {
        encode_no_operand(&mut self.code, Opcode::RET_U);
        self.stack_depth = 0;
    }

    /// Emit a binary op instruction (pops 2, pushes 1).
    pub fn emit_binop(&mut self, op: Opcode) {
        encode_no_operand(&mut self.code, op);
        self.pop_n(1);
    }

    /// Emit a unary op.
    pub fn emit_unop(&mut self, op: Opcode) {
        encode_no_operand(&mut self.code, op);
    }

    pub fn emit_ld_fld(&mut self, index: u32) -> Result<(), EmitError> {
        let i = u8::try_from(index).map_err(|_| EmitError::OperandOverflow {
            desc: "field index exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::LD_FLD, i);
        Ok(())
    }

    pub fn emit_mk_prd(&mut self, field_count: u32, stack_pop: i32) -> Result<(), EmitError> {
        let n = u8::try_from(field_count).map_err(|_| EmitError::OperandOverflow {
            desc: "product field count exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::MK_PRD, n);
        self.pop_n(stack_pop - 1);
        Ok(())
    }

    pub fn emit_mk_var(&mut self, tag: u32) -> Result<(), EmitError> {
        if let Ok(t) = u8::try_from(tag) {
            encode_u8(&mut self.code, Opcode::MK_VAR, t);
        } else {
            let t = u16::try_from(tag).map_err(|_| EmitError::OperandOverflow {
                desc: "variant tag exceeds 65535".into(),
            })?;
            encode_u16(&mut self.code, Opcode::MK_VAR_W, t);
        }
        Ok(())
    }

    pub fn emit_ld_tag(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_TAG);
    }

    /// Emit a direct call to `fn_id`.
    pub fn emit_inv(&mut self, fn_id: u32, effectful: bool, arg_count: i32) {
        let op = if effectful {
            Opcode::INV_EFF
        } else {
            Opcode::INV
        };
        encode_u32(&mut self.code, op, fn_id);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    /// Emit a tail call to `fn_id`.
    pub fn emit_inv_tail(&mut self, fn_id: u32, effectful: bool) {
        let op = if effectful {
            Opcode::INV_TAL_EFF
        } else {
            Opcode::INV_TAL
        };
        encode_u32(&mut self.code, op, fn_id);
    }

    /// Emit an indirect (dynamic) call through a closure or fn value.
    pub fn emit_inv_dyn(&mut self, arg_count: i32) -> Result<(), EmitError> {
        let n = u8::try_from(arg_count).map_err(|_| EmitError::OperandOverflow {
            desc: "dynamic call arg count exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::INV_DYN, n);
        self.pop_n(arg_count + 1);
        self.push_n(1);
        Ok(())
    }

    /// Emit `cmp.eq` (pops 2, pushes 1 → net pop 1).
    pub fn emit_cmp_eq(&mut self) {
        encode_no_operand(&mut self.code, Opcode::CMP_EQ);
        self.pop_n(1);
    }

    /// Emit `ld.pay` with field index (pops ref, pushes payload → net 0).
    pub fn emit_ld_pay(&mut self, field_idx: u8) {
        encode_u8(&mut self.code, Opcode::LD_PAY, field_idx);
    }

    /// Emit `inv.ffi` (pops args, pushes 1 result).
    pub fn emit_inv_ffi(&mut self, ffi_idx: u32, arg_count: i32) {
        encode_u32(&mut self.code, Opcode::INV_FFI, ffi_idx);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    /// Emit `ld.idx` (pops array+index, pushes value → net pop 1).
    pub fn emit_ld_idx(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_IDX);
        self.pop_n(1);
    }

    /// Emit `st.idx` (pops array+index+value → net pop 3).
    pub fn emit_st_idx(&mut self) {
        encode_no_operand(&mut self.code, Opcode::ST_IDX);
        self.pop_n(3);
    }

    /// Emit `mk.arr` with type id (pops length — actually takes count operand, pushes array ref).
    pub fn emit_mk_arr(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::MK_ARR, type_id);
        self.push_n(1);
    }

    /// Emit `alc.ref` — pops initial value, pushes ref. Net stack: 0.
    pub fn emit_alc_ref(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::ALC_REF, type_id);
        // pops initial value, pushes ref → net 0
    }

    /// Emit `st.fld` — pops [obj, value]. Net stack: -2.
    pub fn emit_st_fld(&mut self, index: u32) -> Result<(), EmitError> {
        let i = u8::try_from(index).map_err(|_| EmitError::OperandOverflow {
            desc: "field index exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::ST_FLD, i);
        self.pop_n(2);
        Ok(())
    }

    /// Emit `type.chk type_id` — pops value, pushes bool. Net stack: 0.
    pub fn emit_type_chk(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::TYP_CHK, type_id);
        // pops 1, pushes 1 → net 0
    }

    pub fn emit_eff_psh(&mut self, effect_id: u32, handler_fn_id: u32) -> Result<(), EmitError> {
        let id = u8::try_from(effect_id).map_err(|_| EmitError::OperandOverflow {
            desc: "effect id exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::EFF_PSH, id);
        self.handlers.push(HandlerEntry {
            effect_id: id,
            handler_fn_id,
        });
        Ok(())
    }

    pub fn emit_eff_pop(&mut self, effect_id: u32) -> Result<(), EmitError> {
        let id = u8::try_from(effect_id).map_err(|_| EmitError::OperandOverflow {
            desc: "effect id exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::EFF_POP, id);
        Ok(())
    }

    pub fn emit_eff_do(&mut self, op_id: u32, arg_count: i32) {
        encode_u32(&mut self.code, Opcode::EFF_DO, op_id);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    pub fn emit_eff_res(&mut self) {
        encode_no_operand(&mut self.code, Opcode::EFF_RES_C);
        self.pop_n(1);
    }

    /// Emit `tsk.spn fn_id` — spawn a task running `fn_id`, popping `arg_count` args from the
    /// caller's stack and pushing a task handle.
    pub fn emit_tsk_spn(&mut self, fn_id: u32, arg_count: i32) {
        encode_u32(&mut self.code, Opcode::TSK_SPN, fn_id);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    /// Emit `tsk.awt` — pop task handle, push its result. Net stack: 0.
    pub fn emit_tsk_awt(&mut self) {
        encode_no_operand(&mut self.code, Opcode::TSK_AWT);
    }

    /// Emit `tsk.cmk` — create a channel, pushing its handle. Net stack: +1.
    pub fn emit_tsk_cmk(&mut self) {
        encode_u32(&mut self.code, Opcode::TSK_CMK, 0);
        self.push_n(1);
    }

    /// Emit `tsk.chs` — pop [chan, value], push unit. Net stack: -1.
    pub fn emit_tsk_chs(&mut self) {
        encode_u32(&mut self.code, Opcode::TSK_CHS, 0);
        self.pop_n(2);
        self.push_n(1);
    }

    /// Emit `tsk.chr` — pop chan, push received value. Net stack: 0.
    pub fn emit_tsk_chr(&mut self) {
        encode_u32(&mut self.code, Opcode::TSK_CHR, 0);
    }

    /// Emit `cmp.tag` or `cmp.tag.w` — compare object tag inline (pop obj, push bool → net 0).
    pub fn emit_cmp_tag(&mut self, tag: u32) -> Result<(), EmitError> {
        if let Ok(t) = u8::try_from(tag) {
            encode_u8(&mut self.code, Opcode::CMP_TAG, t);
        } else {
            let t = u16::try_from(tag).map_err(|_| EmitError::OperandOverflow {
                desc: "variant tag exceeds 65535".into(),
            })?;
            encode_u16(&mut self.code, Opcode::CMP_TAG_W, t);
        }
        Ok(())
    }

    /// Emit `ld.upv idx` — load upvalue from current closure. Net stack: +1.
    pub fn emit_ld_upv(&mut self, idx: u8) {
        encode_u8(&mut self.code, Opcode::LD_UPV, idx);
        self.push_n(1);
    }

    /// Emit `mk.clo fn_id` — pop N upvalues, allocate closure, push ref.
    /// `upvalue_count` is the number of values to pop.
    pub fn emit_mk_clo(&mut self, fn_id: u32, upvalue_count: u16) {
        encode_u32(&mut self.code, Opcode::MK_CLO, fn_id);
        self.pop_n(i32::from(upvalue_count));
        self.push_n(1);
    }

    /// Emit a conditional wide jump-if-false to `label` (5 bytes, i32 offset, pops condition).
    pub fn emit_jmp_f(&mut self, label: u32) {
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JMP_F_W, 0);
        let instr_len = 5;
        self.pop_n(1);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len,
            label,
        });
    }

    /// Emit an unconditional wide jump to `label` (5 bytes, i32 offset).
    pub fn emit_jmp(&mut self, label: u32) {
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JMP_W, 0);
        let instr_len = 5;
        self.fixups.push(Fixup {
            instr_offset,
            instr_len,
            label,
        });
        self.stack_depth = 0;
    }

    /// Emit a conditional wide jump-if-true to `label` (5 bytes, i32 offset, pops condition).
    pub fn emit_jmp_t(&mut self, label: u32) {
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JMP_T_W, 0);
        let instr_len = 5;
        self.pop_n(1);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len,
            label,
        });
    }

    /// Record a label target at the current code position.
    pub fn emit_label(&mut self, label: u32) {
        let prev = self.label_targets.insert(label, self.code.len());
        debug_assert!(prev.is_none(), "duplicate label target {label}");
    }

    /// Resolve all forward-jump fixups.
    pub fn resolve_fixups(&mut self, fn_name: &str) -> Result<(), EmitError> {
        for fixup in &self.fixups {
            let target = self
                .label_targets
                .get(&fixup.label)
                .copied()
                .ok_or_else(|| EmitError::UnresolvableLabel {
                    name: fn_name.into(),
                })?;
            let after_instr = fixup.instr_offset + fixup.instr_len;
            let raw_offset =
                i64::try_from(target).map_err(|_| EmitError::UnresolvableLabel {
                    name: fn_name.into(),
                })? - i64::try_from(after_instr).map_err(|_| EmitError::UnresolvableLabel {
                    name: fn_name.into(),
                })?;
            let offset = i32::try_from(raw_offset).map_err(|_| EmitError::JumpTooFar)?;
            let patch_offset = fixup.instr_offset + 1;
            let bytes = offset.to_le_bytes();
            self.code[patch_offset] = bytes[0];
            self.code[patch_offset + 1] = bytes[1];
            self.code[patch_offset + 2] = bytes[2];
            self.code[patch_offset + 3] = bytes[3];
        }
        self.fixups.clear();
        Ok(())
    }

    /// Check that code length fits in u32.
    pub fn validate_code_len(&self) -> Result<u32, EmitError> {
        u32::try_from(self.code.len()).map_err(|_| EmitError::FunctionTooLarge)
    }
}
