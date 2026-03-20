//! Per-function bytecode emitter.
//!
//! Manages local slot allocation, label targets, forward-jump fixups,
//! and the code buffer for one function.

use std::collections::HashMap;

use crate::error::{EmitError, EmitResult};
use msc_bc::{Opcode, encode_f0, encode_fi8, encode_fi8x2, encode_fi16};

/// Fixup record: a forward jump that needs patching once we know the label target.
struct Fixup {
    /// Byte offset of the opcode in `code`.
    instr_offset: usize,
    /// Total instruction length in bytes (opcode + operand).
    instr_len: usize,
    /// Whether this is a `BR_LONG` (4-byte i24) rather than a 3-byte i16 branch.
    long: bool,
    label: u32,
}

/// Handler table entry for effect push instructions.
pub struct HandlerEntry {
    pub effect_id: u8,
    pub handler_fn_id: u32,
}

/// A GC safepoint: records the bytecode offset and operand stack depth at
/// a call/allocation site so the garbage collector can find live references.
pub struct SafepointEntry {
    pub offset: u32,
    pub stack_depth: u16,
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
    pub safepoints: Vec<SafepointEntry>,
    /// Source map entries: (`bytecode_offset`, `span_byte_offset`, `0_column`).
    /// Recorded at function call and branch sites; `span_byte_offset` is used
    /// in place of line since no line map is available at emit time.
    pub source_map: Vec<(u32, u32, u16)>,
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
            safepoints: vec![],
            source_map: vec![],
        }
    }

    /// Record a GC safepoint at the current code position.
    ///
    /// Called immediately after encoding any opcode that may trigger a GC
    /// (allocations, calls, effect operations). Uses a saturating cast for
    /// the stack depth so that tracking stays correct even at extreme depths.
    fn record_safepoint(&mut self) {
        let offset = u32::try_from(self.code.len()).unwrap_or(u32::MAX);
        let depth = u16::try_from(self.stack_depth.max(0)).unwrap_or(u16::MAX);
        self.safepoints.push(SafepointEntry {
            offset,
            stack_depth: depth,
        });
    }

    /// Record a source map entry at the current code position for the given span.
    ///
    /// `span_start` is the byte offset in the source file; used as the "line"
    /// field until a line map is available at emit time.
    pub fn record_source(&mut self, span_start: u32) {
        let offset = u32::try_from(self.code.len()).unwrap_or(u32::MAX);
        self.source_map.push((offset, span_start, 0));
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

    /// Emit `ld.loc slot` - slot must fit in u8.
    pub fn emit_ld_loc(&mut self, slot: u32) {
        let s = u8::try_from(slot).expect("local slot exceeds 255");
        encode_fi8(&mut self.code, Opcode::LD_LOC, s);
        self.push_n(1);
    }

    /// Emit `st.loc slot` - slot must fit in u8.
    pub fn emit_st_loc(&mut self, slot: u32) {
        let s = u8::try_from(slot).expect("local slot exceeds 255");
        encode_fi8(&mut self.code, Opcode::ST_LOC, s);
        self.pop_n(1);
    }

    /// Emit `ld.const idx`.
    pub fn emit_ld_cst(&mut self, idx: u16) {
        encode_fi16(&mut self.code, Opcode::LD_CONST, idx);
        self.push_n(1);
    }

    /// Emit `ld.smi value` - load a small signed integer fitting in i16.
    ///
    /// Avoids a const pool entry for values in -32768..=32767.
    pub fn emit_ld_smi(&mut self, value: i16) {
        encode_fi16(&mut self.code, Opcode::LD_SMI, value.cast_unsigned());
        self.push_n(1);
    }

    /// Emit `tup.get idx` - pops tuple, pushes element at `idx`. Net 0.
    pub fn emit_tup_get(&mut self, index: u32) -> EmitResult {
        let i = u8::try_from(index).map_err(|_| EmitError::OperandOverflow {
            desc: "tuple field index exceeds 255".into(),
        })?;
        encode_fi8(&mut self.code, Opcode::TUP_GET, i);
        Ok(())
    }

    pub fn emit_dup(&mut self) {
        encode_f0(&mut self.code, Opcode::DUP);
        self.push_n(1);
    }

    pub fn emit_pop(&mut self) {
        encode_f0(&mut self.code, Opcode::POP);
        self.pop_n(1);
    }

    pub fn emit_ret(&mut self) {
        encode_f0(&mut self.code, Opcode::RET);
        self.stack_depth = 0;
    }

    pub fn emit_ret_u(&mut self) {
        encode_f0(&mut self.code, Opcode::RET_UNIT);
        self.stack_depth = 0;
    }

    pub fn emit_ld_unit(&mut self) {
        encode_f0(&mut self.code, Opcode::LD_UNIT);
        self.push_n(1);
    }

    pub fn emit_ld_true(&mut self) {
        encode_f0(&mut self.code, Opcode::LD_TRUE);
        self.push_n(1);
    }

    pub fn emit_ld_false(&mut self) {
        encode_f0(&mut self.code, Opcode::LD_FALSE);
        self.push_n(1);
    }

    pub fn emit_ld_none(&mut self) {
        encode_f0(&mut self.code, Opcode::LD_NONE);
        self.push_n(1);
    }

    /// Emit a binary op instruction (F0, pops 2, pushes 1 → net -1).
    pub fn emit_binop(&mut self, op: Opcode) {
        encode_f0(&mut self.code, op);
        self.pop_n(1);
    }

    /// Emit a unary op (F0, pops 1, pushes 1 → net 0).
    pub fn emit_unop(&mut self, op: Opcode) {
        encode_f0(&mut self.code, op);
    }

    /// Emit `rec.get field` - pops record, pushes field value. Net 0.
    pub fn emit_ld_fld(&mut self, index: u32) -> EmitResult {
        let i = u8::try_from(index).map_err(|_| EmitError::OperandOverflow {
            desc: "field index exceeds 255".into(),
        })?;
        encode_fi8(&mut self.code, Opcode::REC_GET, i);
        Ok(())
    }

    /// Emit `tup.new count` - pops count items, pushes tuple. Net -(count-1).
    pub fn emit_mk_prd(&mut self, field_count: u32, stack_pop: i32) -> EmitResult {
        let n = u8::try_from(field_count).map_err(|_| EmitError::OperandOverflow {
            desc: "product field count exceeds 255".into(),
        })?;
        encode_fi8(&mut self.code, Opcode::TUP_NEW, n);
        self.pop_n(stack_pop - 1);
        self.record_safepoint(); // tup.new
        Ok(())
    }

    pub fn emit_mk_var(&mut self, tag: u32, arity: u8) {
        let tag_u8 = u8::try_from(tag).unwrap_or(u8::MAX);
        encode_fi8x2(&mut self.code, Opcode::REC_NEW, tag_u8, arity);
        if arity > 0 {
            self.pop_n(i32::from(arity));
        }
        self.push_n(1);
        self.record_safepoint(); // rec.new
    }

    /// Emit `ty.of` - leaves type tag on stack. Net 0.
    pub fn emit_ld_tag(&mut self) {
        encode_f0(&mut self.code, Opcode::TY_OF);
    }

    pub fn emit_inv(&mut self, fn_id: u32, _effectful: bool, arg_count: i32) {
        let proto = u16::try_from(fn_id).unwrap_or(u16::MAX);
        encode_fi16(&mut self.code, Opcode::CLS_NEW, proto);
        self.push_n(1);
        self.record_safepoint(); // cls.new
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        encode_fi8(&mut self.code, Opcode::CALL, arity);
        self.pop_n(arg_count + 1);
        self.push_n(1);
        self.record_safepoint(); // call
    }

    pub fn emit_inv_tail(&mut self, fn_id: u32, _effectful: bool, arg_count: i32) {
        let proto = u16::try_from(fn_id).unwrap_or(u16::MAX);
        encode_fi16(&mut self.code, Opcode::CLS_NEW, proto);
        self.push_n(1);
        self.record_safepoint(); // cls.new
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        encode_fi8(&mut self.code, Opcode::CALL_TAIL, arity);
        self.record_safepoint(); // call.tail
        self.stack_depth = 0;
    }

    pub fn emit_inv_dyn(&mut self, arg_count: i32) -> EmitResult {
        let n = u8::try_from(arg_count).map_err(|_| EmitError::OperandOverflow {
            desc: "dynamic call arg count exceeds 255".into(),
        })?;
        encode_fi8(&mut self.code, Opcode::CALL, n);
        self.pop_n(arg_count + 1);
        self.push_n(1);
        self.record_safepoint(); // call (dynamic)
        Ok(())
    }

    /// Emit `cmp.eq` (pops 2, pushes 1 → net -1).
    pub fn emit_cmp_eq(&mut self) {
        encode_f0(&mut self.code, Opcode::CMP_EQ);
        self.pop_n(1);
    }

    /// Emit `mat.data` - pops variant, pushes its payload. Net 0.
    pub fn emit_ld_pay(&mut self, _field_idx: u8) {
        encode_f0(&mut self.code, Opcode::MAT_DATA);
    }

    /// Emit `ffi.call sym,arity`.
    pub fn emit_inv_ffi(&mut self, ffi_idx: u32, arg_count: i32) {
        let sym = u8::try_from(ffi_idx).unwrap_or(u8::MAX);
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        encode_fi8x2(&mut self.code, Opcode::FFI_CALL, sym, arity);
        self.pop_n(arg_count);
        self.push_n(1);
        self.record_safepoint(); // ffi.call
    }

    /// Emit `arr.len` - pops array ref, pushes length. Net 0.
    pub fn emit_ld_len(&mut self) {
        encode_f0(&mut self.code, Opcode::ARR_LEN);
    }

    /// Emit `arr.get` - pops [array, index], pushes element. Net -1.
    pub fn emit_ld_idx(&mut self) {
        encode_f0(&mut self.code, Opcode::ARR_GET);
        self.pop_n(1);
    }

    /// Emit `arr.set` - pops [array, index, value]. Net -3.
    pub fn emit_st_idx(&mut self) {
        encode_f0(&mut self.code, Opcode::ARR_SET);
        self.pop_n(3);
    }

    pub fn emit_mk_arr(&mut self, _type_id: u32) {
        encode_f0(&mut self.code, Opcode::ARR_NEW);
        self.record_safepoint(); // arr.new
    }

    /// Allocate a ref cell: wraps value on stack into a 1-field record (tag 0).
    /// `REC_NEW(0, 1)` pops 1 value, pushes 1 record. Net: 0.
    pub fn emit_alc_ref(&mut self, _type_id: u32) {
        encode_fi8x2(&mut self.code, Opcode::REC_NEW, 0, 1);
    }

    /// Emit `rec.set field` - pops [record, value]. Net -2.
    pub fn emit_st_fld(&mut self, index: u32) -> EmitResult {
        let i = u8::try_from(index).map_err(|_| EmitError::OperandOverflow {
            desc: "field index exceeds 255".into(),
        })?;
        encode_fi8(&mut self.code, Opcode::REC_SET, i);
        self.pop_n(2);
        Ok(())
    }

    pub fn emit_type_chk(&mut self, type_id: u32) {
        let tid = u16::try_from(type_id).unwrap_or(u16::MAX);
        encode_fi16(&mut self.code, Opcode::TY_DESC, tid);
        encode_f0(&mut self.code, Opcode::TY_TEST);
    }

    pub fn emit_ty_cast(&mut self, type_id: u32) {
        let tid = u16::try_from(type_id).unwrap_or(u16::MAX);
        encode_fi16(&mut self.code, Opcode::TY_DESC, tid);
        encode_f0(&mut self.code, Opcode::TY_CAST);
        self.record_safepoint(); // ty.cast
    }

    /// Emit `eff.hdl effect` - install an effect handler.
    ///
    /// `one_shot` sets bit 15 of the operand. The VM uses this hint to move
    /// captured continuation frames on the first resume instead of cloning them.
    pub fn emit_cont_mark(
        &mut self,
        effect_id: u32,
        handler_fn_id: u32,
        one_shot: bool,
    ) -> EmitResult {
        let id = u16::try_from(effect_id).map_err(|_| EmitError::OperandOverflow {
            desc: "effect id exceeds 65535".into(),
        })?;
        let operand = id | if one_shot { 0x8000 } else { 0 };
        let handler_u8 = u8::try_from(effect_id).unwrap_or(0);
        encode_fi16(&mut self.code, Opcode::EFF_HDL, operand);
        self.handlers.push(HandlerEntry {
            effect_id: handler_u8,
            handler_fn_id,
        });
        Ok(())
    }

    /// Emit `eff.pop` - remove the innermost effect handler.
    pub fn emit_cont_unmark(&mut self, _effect_id: u32) {
        encode_f0(&mut self.code, Opcode::EFF_POP);
    }

    /// Emit `eff.need op,arity` - perform an effect operation.
    pub fn emit_cont_save(&mut self, op_id: u32, arg_count: i32) {
        let op = u8::try_from(op_id).unwrap_or(u8::MAX);
        let arity = u8::try_from(arg_count).unwrap_or(u8::MAX);
        encode_fi8x2(&mut self.code, Opcode::EFF_NEED, op, arity);
        self.pop_n(arg_count);
        self.push_n(1);
        self.record_safepoint(); // eff.need
    }

    /// Emit `eff.res` - resume from a handler.
    pub fn emit_cont_resume(&mut self) {
        encode_f0(&mut self.code, Opcode::EFF_RES);
        self.pop_n(1);
        self.record_safepoint(); // eff.res
    }

    pub fn emit_cmp_tag(&mut self, tag: u32) -> EmitResult {
        let t = u16::try_from(tag).map_err(|_| EmitError::OperandOverflow {
            desc: "variant tag exceeds 65535".into(),
        })?;
        encode_fi16(&mut self.code, Opcode::MAT_TAG, t);
        self.push_n(1);
        Ok(())
    }

    /// Emit `ld.upv idx` - load upvalue from current closure. Net +1.
    pub fn emit_ld_upv(&mut self, idx: u8) {
        encode_fi8(&mut self.code, Opcode::LD_UPV, idx);
        self.push_n(1);
    }

    /// Emit `cls.new fn_id` - allocate a closure shell. Net +1.
    pub fn emit_cls_new(&mut self, fn_id: u32) {
        let proto = u16::try_from(fn_id).unwrap_or(u16::MAX);
        encode_fi16(&mut self.code, Opcode::CLS_NEW, proto);
        self.push_n(1);
        self.record_safepoint(); // cls.new
    }

    /// Emit `cls.upv 0, slot` - attach an open upvalue for local `slot`. Stack neutral.
    pub fn emit_cls_upv_local(&mut self, slot: u32) {
        let s = u8::try_from(slot).expect("local slot exceeds 255");
        encode_fi8x2(&mut self.code, Opcode::CLS_UPV, 0, s);
    }

    /// Emit `cls.upv 1, idx` - re-capture parent upvalue at `idx`. Stack neutral.
    pub fn emit_cls_upv_parent(&mut self, idx: u8) {
        encode_fi8x2(&mut self.code, Opcode::CLS_UPV, 1, idx);
    }

    /// Emit `cls.dict class_idx` - pops receiver, pushes a method dict record. Net 0.
    ///
    /// The VM resolves the instance at runtime by querying the class dispatch
    /// table keyed on `(class_idx, value_type_id(receiver))`.
    pub fn emit_cls_dict(&mut self, class_idx: u16) {
        encode_fi16(&mut self.code, Opcode::CLS_DICT, class_idx);
        // Net 0: pop receiver, push dict record.
        self.record_safepoint(); // cls.dict
    }

    /// Emit `cls.disp class_idx, method_idx` - pops receiver, calls method. Net 0.
    ///
    /// Used when the receiver type is not statically known (Any/Unknown): the VM
    /// looks up the method by `(class_idx, value_type_id(receiver))` and dispatches
    /// directly without materialising a full dict record.
    pub fn emit_cls_disp(&mut self, class_idx: u8, method_idx: u8) {
        encode_fi8x2(&mut self.code, Opcode::CLS_DISP, class_idx, method_idx);
        // Net 0: pop receiver, callee pushes result.
    }

    pub fn emit_jmp_f(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 3;
            let offset = target.cast_signed() - after_short.cast_signed();
            if let Ok(off16) = i16::try_from(offset) {
                encode_fi16(&mut self.code, Opcode::BR_FALSE, off16.cast_unsigned());
                self.pop_n(1);
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_fi16(&mut self.code, Opcode::BR_FALSE, 0);
        self.pop_n(1);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len: 3,
            long: false,
            label,
        });
    }

    /// Emit an unconditional jump to `label`.
    pub fn emit_jmp(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 3;
            let offset = target.cast_signed() - after_short.cast_signed();
            if let Ok(off16) = i16::try_from(offset) {
                encode_fi16(&mut self.code, Opcode::BR, off16.cast_unsigned());
                self.stack_depth = 0;
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_fi16(&mut self.code, Opcode::BR, 0);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len: 3,
            long: false,
            label,
        });
        self.stack_depth = 0;
    }

    /// Emit a conditional jump-if-true to `label`.
    pub fn emit_jmp_t(&mut self, label: u32) {
        if let Some(&target) = self.label_targets.get(&label) {
            let after_short = self.code.len() + 3;
            let offset = target.cast_signed() - after_short.cast_signed();
            if let Ok(off16) = i16::try_from(offset) {
                encode_fi16(&mut self.code, Opcode::BR_TRUE, off16.cast_unsigned());
                self.pop_n(1);
                return;
            }
        }
        let instr_offset = self.code.len();
        encode_fi16(&mut self.code, Opcode::BR_TRUE, 0);
        self.pop_n(1);
        self.fixups.push(Fixup {
            instr_offset,
            instr_len: 3,
            long: false,
            label,
        });
    }

    /// Record a label target at the current code position.
    pub fn emit_label(&mut self, label: u32) {
        let prev = self.label_targets.insert(label, self.code.len());
        debug_assert!(prev.is_none(), "duplicate label target {label}");
    }

    pub fn resolve_fixups(&mut self, fn_name: &str) -> EmitResult {
        let mut i = 0;
        while i < self.fixups.len() {
            let fixup = &self.fixups[i];
            let target = self
                .label_targets
                .get(&fixup.label)
                .copied()
                .ok_or_else(|| EmitError::UnresolvableLabel {
                    name: fn_name.into(),
                })?;
            let instr_offset = fixup.instr_offset;
            let instr_len = fixup.instr_len;
            let after_instr = instr_offset + instr_len;
            let raw_offset = i64::try_from(target).map_err(|_| EmitError::JumpTooFar)?
                - i64::try_from(after_instr).map_err(|_| EmitError::JumpTooFar)?;

            if let Ok(off16) = i16::try_from(raw_offset) {
                let [hi, lo] = off16.to_be_bytes();
                self.code[instr_offset + 1] = hi;
                self.code[instr_offset + 2] = lo;
            } else {
                // Upgrade 3-byte BR placeholder to 4-byte BR_LONG.
                // Conditional branches that need >i16 offsets are unsupported.
                if self.code[instr_offset] != Opcode::BR.0 {
                    return Err(EmitError::JumpTooFar);
                }
                let _ = self.code.remove(instr_offset + 2);
                let _ = self.code.remove(instr_offset + 1);
                self.code[instr_offset] = Opcode::BR_LONG.0;

                let new_after = instr_offset + 4;
                let new_raw = i64::try_from(target).map_err(|_| EmitError::JumpTooFar)?
                    - i64::try_from(new_after).map_err(|_| EmitError::JumpTooFar)?;
                let new_off = i32::try_from(new_raw).map_err(|_| EmitError::JumpTooFar)?;
                encode_fi24_into(&mut self.code, instr_offset + 1, new_off);

                for tgt in self.label_targets.values_mut() {
                    if *tgt > instr_offset {
                        *tgt += 1;
                    }
                }
                for other in &mut self.fixups {
                    if other.instr_offset > instr_offset {
                        other.instr_offset += 1;
                    }
                }
                self.fixups[i].instr_len = 4;
                self.fixups[i].long = true;
            }
            i += 1;
        }
        self.fixups.clear();
        Ok(())
    }

    /// Check that code length fits in u32.
    pub fn validate_code_len(&self) -> EmitResult<u32> {
        u32::try_from(self.code.len()).map_err(|_| EmitError::FunctionTooLarge)
    }

    /// Load a field from an imported dep's module record: `ld.loc M` + `rec.get N`.
    pub fn emit_ld_import(&mut self, module_slot: u32, field: u32) {
        let ms = u8::try_from(module_slot).unwrap_or(u8::MAX);
        let fi = u8::try_from(field).unwrap_or(u8::MAX);
        encode_fi8(&mut self.code, Opcode::LD_LOC, ms);
        self.push_n(1);
        encode_fi8(&mut self.code, Opcode::REC_GET, fi);
        // REC_GET: net 0 (pop record, push field value)
    }

    /// Load a global: `ld.loc 0` (module record) + `rec.get field`.
    pub fn emit_ld_glb(&mut self, slot: u32) {
        let field = u8::try_from(slot).unwrap_or(u8::MAX);
        encode_fi8(&mut self.code, Opcode::LD_LOC, 0);
        self.push_n(1);
        encode_fi8(&mut self.code, Opcode::REC_GET, field);
        // REC_GET: net 0 (pop record, push field value)
    }

    /// Store a global: save value to temp, load module record, reload value, rec.set.
    pub fn emit_st_glb(&mut self, slot: u32) {
        // Stack: [value]. Need [record, value] for REC_SET.
        let field = u8::try_from(slot).unwrap_or(u8::MAX);
        let temp = u32::from(self.local_count);
        self.local_count += 1;
        let temp_u8 = u8::try_from(temp).unwrap_or(u8::MAX);
        encode_fi8(&mut self.code, Opcode::ST_LOC, temp_u8);
        self.pop_n(1);
        encode_fi8(&mut self.code, Opcode::LD_LOC, 0);
        self.push_n(1);
        encode_fi8(&mut self.code, Opcode::LD_LOC, temp_u8);
        self.push_n(1);
        encode_fi8(&mut self.code, Opcode::REC_SET, field);
        self.pop_n(2);
    }
}

/// Write a signed i24 value in BE into `buf` at `offset` (bytes offset..offset+3).
fn encode_fi24_into(buf: &mut [u8], offset: usize, value: i32) {
    let clamped = value.clamp(-8_388_608, 8_388_607);
    let bits = clamped.cast_unsigned() & 0x00FF_FFFF;
    buf[offset] = u8::try_from((bits >> 16) & 0xFF).expect("masked to 8 bits");
    buf[offset + 1] = u8::try_from((bits >> 8) & 0xFF).expect("masked to 8 bits");
    buf[offset + 2] = u8::try_from(bits & 0xFF).expect("masked to 8 bits");
}
