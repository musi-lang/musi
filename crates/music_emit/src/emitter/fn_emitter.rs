//! Per-function bytecode emitter.
//!
//! Manages local slot allocation, label targets, forward-jump fixups,
//! and the code buffer for one function.

use std::collections::HashMap;

use crate::error::EmitError;
use musi_bc::{
    Opcode, encode_i8, encode_i32, encode_no_operand, encode_u8, encode_u16, encode_u32,
    encode_wid_u16, pack_id_arity, pack_tag_arity_u16,
};

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

    /// Emit `ld.loc` with WID prefix if slot > 255.
    pub fn emit_ld_loc(&mut self, slot: u32) {
        if let Ok(s) = u8::try_from(slot) {
            encode_u8(&mut self.code, Opcode::LD_LOC, s);
        } else {
            let s = u16::try_from(slot).expect("slot fits in u16");
            encode_wid_u16(&mut self.code, Opcode::LD_LOC, s);
        }
        self.push_n(1);
    }

    /// Emit `st.loc` with WID prefix if slot > 255.
    pub fn emit_st_loc(&mut self, slot: u32) {
        if let Ok(s) = u8::try_from(slot) {
            encode_u8(&mut self.code, Opcode::ST_LOC, s);
        } else {
            let s = u16::try_from(slot).expect("slot fits in u16");
            encode_wid_u16(&mut self.code, Opcode::ST_LOC, s);
        }
        self.pop_n(1);
    }

    pub fn emit_ld_glb(&mut self, slot: u32) {
        encode_u32(&mut self.code, Opcode::LD_GLB, slot);
        self.push_n(1);
    }

    pub fn emit_st_glb(&mut self, slot: u32) {
        encode_u32(&mut self.code, Opcode::ST_GLB, slot);
        self.pop_n(1);
    }

    /// Emit `ld.cst` with WID prefix if index > 255.
    pub fn emit_ld_cst(&mut self, idx: u16) {
        if let Ok(i) = u8::try_from(idx) {
            encode_u8(&mut self.code, Opcode::LD_CST, i);
        } else {
            encode_wid_u16(&mut self.code, Opcode::LD_CST, idx);
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
        encode_no_operand(&mut self.code, Opcode::RET_UT);
        self.stack_depth = 0;
    }

    pub fn emit_ld_unit(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_UT);
        self.push_n(1);
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

    /// Emit `mk.var` with packed (tag, arity) in a u16 operand.
    /// If tag > 255, use WID prefix for u32 packed operand.
    pub fn emit_mk_var(&mut self, tag: u32, arity: u8) -> Result<(), EmitError> {
        if let Ok(t) = u8::try_from(tag) {
            let packed = pack_tag_arity_u16(t, arity);
            encode_u16(&mut self.code, Opcode::MK_VAR, packed);
        } else {
            // WID prefix: widen MK_VAR from u16 to u32
            let packed = (tag << 8) | u32::from(arity);
            musi_bc::encode_wid_u32(&mut self.code, Opcode::MK_VAR, packed);
        }
        self.pop_n(i32::from(arity));
        self.push_n(1);
        Ok(())
    }

    pub fn emit_ld_tag(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_TAG);
    }

    /// Emit a direct call with packed operand: (fn_id_u24 << 8) | arity_u8.
    pub fn emit_inv(&mut self, fn_id: u32, _effectful: bool, arg_count: i32) {
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        let packed = pack_id_arity(fn_id, arity);
        encode_u32(&mut self.code, Opcode::INV, packed);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    /// Emit a tail call with packed operand: (fn_id_u24 << 8) | arity_u8.
    pub fn emit_inv_tail(&mut self, fn_id: u32, _effectful: bool) {
        // Arity 0 as default — the VM reads param_count from the function anyway
        let packed = pack_id_arity(fn_id, 0);
        encode_u32(&mut self.code, Opcode::INV_TAL, packed);
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

    /// Emit `cmp.eq` (pops 2, pushes 1 -> net pop 1).
    pub fn emit_cmp_eq(&mut self) {
        encode_no_operand(&mut self.code, Opcode::CMP_EQ);
        self.pop_n(1);
    }

    /// Emit `ld.pay` with field index (pops ref, pushes payload -> net 0).
    pub fn emit_ld_pay(&mut self, field_idx: u8) {
        encode_u8(&mut self.code, Opcode::LD_PAY, field_idx);
    }

    /// Emit `inv.ffi` with packed operand: (ffi_id_u24 << 8) | arity_u8.
    pub fn emit_inv_ffi(&mut self, ffi_idx: u32, arg_count: i32) {
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        let packed = pack_id_arity(ffi_idx, arity);
        encode_u32(&mut self.code, Opcode::INV_FFI, packed);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    /// Emit `ld.len` (pops array ref, pushes length as nat -> net 0).
    pub fn emit_ld_len(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_LEN);
    }

    /// Emit `ld.idx` (pops array+index, pushes value -> net pop 1).
    pub fn emit_ld_idx(&mut self) {
        encode_no_operand(&mut self.code, Opcode::LD_IDX);
        self.pop_n(1);
    }

    /// Emit `st.idx` (pops array+index+value -> net pop 3).
    pub fn emit_st_idx(&mut self) {
        encode_no_operand(&mut self.code, Opcode::ST_IDX);
        self.pop_n(3);
    }

    /// Emit `mk.arr type_id` — pops a length value from the stack, allocates array, pushes ref.
    /// Net stack effect: 0 (pop length, push ref). Caller must push the length first.
    pub fn emit_mk_arr(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::MK_ARR, type_id);
        // pops 1 (length), pushes 1 (ref) -> net 0
    }

    /// Emit `int.add` — pops two integers, pushes their sum. Net stack: -1.
    pub fn emit_i_add(&mut self) {
        encode_no_operand(&mut self.code, Opcode::INT_ADD);
        self.pop_n(1);
    }

    /// Emit `alc.ref` — pops initial value, pushes ref. Net stack: 0.
    pub fn emit_alc_ref(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::ALC_REF, type_id);
        // pops initial value, pushes ref -> net 0
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

    /// Emit `typ.chk type_id` — pops value, pushes bool. Net stack: 0.
    pub fn emit_type_chk(&mut self, type_id: u32) {
        encode_u32(&mut self.code, Opcode::TYP_CHK, type_id);
        // pops 1, pushes 1 -> net 0
    }

    pub fn emit_cont_mark(&mut self, effect_id: u32, handler_fn_id: u32) -> Result<(), EmitError> {
        let id = u8::try_from(effect_id).map_err(|_| EmitError::OperandOverflow {
            desc: "effect id exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::CNT_MRK, id);
        self.handlers.push(HandlerEntry {
            effect_id: id,
            handler_fn_id,
        });
        Ok(())
    }

    pub fn emit_cont_unmark(&mut self, effect_id: u32) -> Result<(), EmitError> {
        let id = u8::try_from(effect_id).map_err(|_| EmitError::OperandOverflow {
            desc: "effect id exceeds 255".into(),
        })?;
        encode_u8(&mut self.code, Opcode::CNT_UMK, id);
        Ok(())
    }

    pub fn emit_cont_save(&mut self, op_id: u32, arg_count: i32) {
        encode_u32(&mut self.code, Opcode::CNT_SAV, op_id);
        self.pop_n(arg_count);
        self.push_n(1);
    }

    pub fn emit_cont_resume(&mut self) {
        encode_u32(&mut self.code, Opcode::CNT_RSM, 0);
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

    /// Emit `cmp.tag` with WID prefix if tag > 255.
    pub fn emit_cmp_tag(&mut self, tag: u32) -> Result<(), EmitError> {
        if let Ok(t) = u8::try_from(tag) {
            encode_u8(&mut self.code, Opcode::CMP_TAG, t);
        } else {
            let t = u16::try_from(tag).map_err(|_| EmitError::OperandOverflow {
                desc: "variant tag exceeds 65535".into(),
            })?;
            encode_wid_u16(&mut self.code, Opcode::CMP_TAG, t);
        }
        Ok(())
    }

    /// Emit `ld.upv idx` — load upvalue from current closure. Net stack: +1.
    pub fn emit_ld_upv(&mut self, idx: u8) {
        encode_u8(&mut self.code, Opcode::LD_UPV, idx);
        self.push_n(1);
    }

    /// Emit `mk.clo` with packed operand: (fn_id_u24 << 8) | upvalue_count_u8.
    pub fn emit_mk_clo(&mut self, fn_id: u32, upvalue_count: u16) {
        let upval_u8 = u8::try_from(upvalue_count).unwrap_or(u8::MAX);
        let packed = pack_id_arity(fn_id, upval_u8);
        encode_u32(&mut self.code, Opcode::MK_CLO, packed);
        self.pop_n(i32::from(upvalue_count));
        self.push_n(1);
    }

    /// Emit a conditional jump-if-false to `label`.
    /// Uses 2-byte `JNF_SH` for backward jumps that fit i8, otherwise 5-byte `JNF`.
    pub fn emit_jmp_f(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 2;
            let offset = target as isize - after_short as isize;
            if let Ok(off) = i8::try_from(offset) {
                encode_i8(&mut self.code, Opcode::JNF_SH, off);
                self.pop_n(1);
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JNF, 0);
        let instr_len = 5;
        self.pop_n(1);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len,
            label,
        });
    }

    /// Emit an unconditional jump to `label`.
    /// Uses 2-byte `JMP_SH` for backward jumps that fit i8, otherwise 5-byte `JMP`.
    pub fn emit_jmp(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 2;
            let offset = target as isize - after_short as isize;
            if let Ok(off) = i8::try_from(offset) {
                encode_i8(&mut self.code, Opcode::JMP_SH, off);
                self.stack_depth = 0;
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JMP, 0);
        let instr_len = 5;
        self.fixups.push(Fixup {
            instr_offset,
            instr_len,
            label,
        });
        self.stack_depth = 0;
    }

    /// Emit a conditional jump-if-true to `label`.
    /// Uses 2-byte `JIF_SH` for backward jumps that fit i8, otherwise 5-byte `JIF`.
    pub fn emit_jmp_t(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 2;
            let offset = target as isize - after_short as isize;
            if let Ok(off) = i8::try_from(offset) {
                encode_i8(&mut self.code, Opcode::JIF_SH, off);
                self.pop_n(1);
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_i32(&mut self.code, Opcode::JIF, 0);
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
