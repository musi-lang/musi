//! Single-pass pre-execution bytecode verifier.
//!
//! Validates each function in the module before the interpreter begins.
//! Checks operand bounds, jump targets, and stack depth.

use std::collections::HashSet;

use musi_bc::{Opcode, instr_len, widened_operand_size};

use crate::error::VmError;
use crate::loader::{LoadedFn, LoadedModule};

/// Verify all functions in the module before execution.
///
/// # Errors
///
/// Returns `VmError::Verify` if any function has invalid operands, jump
/// targets that do not land on instruction boundaries, or a stack depth
/// that exceeds the declared `max_stack`.
pub fn verify(module: &LoadedModule) -> Result<(), VmError> {
    for func in &module.functions {
        verify_fn(func, module)?;
    }
    Ok(())
}

/// Collect instruction boundary offsets for the given bytecode.
fn collect_boundaries(code: &[u8]) -> Result<HashSet<usize>, VmError> {
    let mut boundaries = HashSet::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let _ = boundaries.insert(ip);
        let raw = code[ip];

        // Handle WID prefix
        if raw == Opcode::WID {
            if ip + 1 >= code.len() {
                return Err(VmError::Verify {
                    desc: "WID prefix at end of bytecode".into(),
                });
            }
            let inner_op = code[ip + 1];
            let zone = inner_op >> 6;
            let wid_size = widened_operand_size(zone);
            ip += 1 + 1 + wid_size; // WID + opcode + widened operand
            continue;
        }

        // Handle EXT prefix
        if raw == Opcode::EXT {
            ip += 2; // EXT + extended opcode byte
            continue;
        }

        let len = instr_len(raw);
        ip += len;
    }
    if ip != code.len() {
        return Err(VmError::Verify {
            desc: "instruction stream does not end on a boundary".into(),
        });
    }
    Ok(boundaries)
}

/// Check that a local slot index is within bounds.
fn check_local(slot: usize, local_count: usize, name: &str) -> Result<(), VmError> {
    if slot >= local_count {
        return Err(VmError::Verify {
            desc: format!("{name} slot {slot} >= local_count {local_count}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a constant pool index is within bounds.
fn check_const(idx: usize, const_len: usize, name: &str) -> Result<(), VmError> {
    if idx >= const_len {
        return Err(VmError::Verify {
            desc: format!("{name} index {idx} >= const pool size {const_len}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a function id exists in the module (when the module has functions).
fn check_fn_id(fn_id: u32, module: &LoadedModule, name: &str) -> Result<(), VmError> {
    let found = module.functions.iter().any(|f| f.fn_id == fn_id);
    if !found && !module.functions.is_empty() {
        return Err(VmError::Verify {
            desc: format!("{name} target fn_id {fn_id} not in module").into_boxed_str(),
        });
    }
    Ok(())
}

/// Check that a jump offset lands on an instruction boundary.
fn check_jump_target(
    ip: usize,
    len: usize,
    offset: isize,
    boundaries: &HashSet<usize>,
) -> Result<(), VmError> {
    let after = ip + len;
    let target = after.wrapping_add_signed(offset);
    if !boundaries.contains(&target) {
        return Err(VmError::Verify {
            desc: format!("jump to non-instruction boundary at {target}").into_boxed_str(),
        });
    }
    Ok(())
}

/// Operand decoders.
struct Operands<'a> {
    code: &'a [u8],
    ip: usize,
    /// Whether this instruction was preceded by a WID prefix.
    widened: bool,
}

impl Operands<'_> {
    fn u8_op(&self) -> usize {
        if self.widened {
            // Widened from u8 → u16
            let lo = self.code[self.ip + 2]; // WID + opcode + operand
            let hi = self.code[self.ip + 3];
            usize::from(u16::from_le_bytes([lo, hi]))
        } else {
            usize::from(self.code[self.ip + 1])
        }
    }

    fn u16_op_full_u32(&self) -> u32 {
        if self.widened {
            let b = [
                self.code[self.ip + 2],
                self.code[self.ip + 3],
                self.code[self.ip + 4],
                self.code[self.ip + 5],
            ];
            u32::from_le_bytes(b)
        } else {
            let lo = self.code[self.ip + 1];
            let hi = self.code[self.ip + 2];
            u32::from(u16::from_le_bytes([lo, hi]))
        }
    }

    fn u32_op(&self) -> u32 {
        let start = self.ip + 1;
        let b = [
            self.code[start],
            self.code[start + 1],
            self.code[start + 2],
            self.code[start + 3],
        ];
        u32::from_le_bytes(b)
    }

    fn i32_jump(&self) -> Result<isize, VmError> {
        let start = self.ip + 1;
        let b = [
            self.code[start],
            self.code[start + 1],
            self.code[start + 2],
            self.code[start + 3],
        ];
        let raw = i32::from_le_bytes(b);
        isize::try_from(raw).map_err(|_| VmError::Verify {
            desc: "jump offset overflows isize".into(),
        })
    }

    fn i8_jump(&self) -> isize {
        let byte = if self.widened {
            self.code[self.ip + 2]
        } else {
            self.code[self.ip + 1]
        };
        isize::from(byte.cast_signed())
    }
}

/// Stack depth delta for fixed-pattern opcodes (no operand validation needed).
///
/// Returns `Some(delta)` if the opcode is in a fixed group, `None` otherwise.
const fn fixed_stack_delta(op: Opcode) -> Option<i32> {
    match op {
        // Net 0: no-ops, terminators, unary, struct ops, effects, concurrency
        Opcode::NOP
        | Opcode::BRK
        | Opcode::HLT
        | Opcode::RET
        | Opcode::RET_UT
        | Opcode::UNR
        | Opcode::SWP
        | Opcode::INT_NEG
        | Opcode::FLT_NEG
        | Opcode::BIT_NOT
        | Opcode::CNV_ITF
        | Opcode::CNV_FTI
        | Opcode::LD_FLD
        | Opcode::MK_ARR
        | Opcode::LD_PAY
        | Opcode::LD_TAG
        | Opcode::CMP_TAG
        | Opcode::LD_LEN
        | Opcode::CNT_MRK
        | Opcode::CNT_UMK
        | Opcode::CNT_RSM
        | Opcode::TSK_CHR
        | Opcode::TSK_AWT
        | Opcode::ALC_REF
        | Opcode::TYP_CHK => Some(0),

        // Push 1: dup, load global, alloc, channel make, load upvalue, load unit
        Opcode::DUP
        | Opcode::LD_GLB
        | Opcode::ALC_ARN
        | Opcode::TSK_CMK
        | Opcode::LD_UPV
        | Opcode::LD_UT => Some(1),

        // Net -1: pop, store global, index load, channel send, binary ops
        Opcode::POP
        | Opcode::ST_GLB
        | Opcode::LD_IDX
        | Opcode::TSK_CHS
        | Opcode::INT_ADD
        | Opcode::INT_SUB
        | Opcode::INT_MUL
        | Opcode::INT_DIV
        | Opcode::INT_REM
        | Opcode::NAT_ADD
        | Opcode::NAT_SUB
        | Opcode::NAT_MUL
        | Opcode::NAT_DIV
        | Opcode::NAT_REM
        | Opcode::FLT_ADD
        | Opcode::FLT_SUB
        | Opcode::FLT_MUL
        | Opcode::FLT_DIV
        | Opcode::FLT_REM
        | Opcode::BIT_AND
        | Opcode::BIT_OR
        | Opcode::BIT_XOR
        | Opcode::BIT_SHL
        | Opcode::BIT_SHR
        | Opcode::BIT_SRU
        | Opcode::CMP_EQ
        | Opcode::CMP_NE
        | Opcode::CMP_LT
        | Opcode::CMP_LE
        | Opcode::CMP_GT
        | Opcode::CMP_GE
        | Opcode::CMP_LTU
        | Opcode::CMP_LEU
        | Opcode::CMP_GTU
        | Opcode::CMP_GEU
        | Opcode::CMP_FLT
        | Opcode::CMP_FLE
        | Opcode::CMP_FGT
        | Opcode::CMP_FGE => Some(-1),

        // Net -2: store field (pops obj+value)
        Opcode::ST_FLD => Some(-2),

        // Net -3: store index (pops value+base+index)
        Opcode::ST_IDX => Some(-3),

        _ => None,
    }
}

/// Verify opcodes that need operand validation and return their stack delta.
fn verify_operand_op(
    op: Opcode,
    ops: &Operands<'_>,
    len: usize,
    module: &LoadedModule,
    local_count: usize,
    const_len: usize,
    boundaries: &HashSet<usize>,
) -> Result<i32, VmError> {
    match op {
        Opcode::LD_LOC => {
            check_local(ops.u8_op(), local_count, "ld.loc")?;
            Ok(1)
        }
        Opcode::ST_LOC => {
            check_local(ops.u8_op(), local_count, "st.loc")?;
            Ok(-1)
        }
        Opcode::LD_CST => {
            check_const(ops.u8_op(), const_len, "ld.cst")?;
            Ok(1)
        }
        // Long jumps (i32 offset)
        Opcode::JMP => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(0)
        }
        Opcode::JIF | Opcode::JNF => {
            check_jump_target(ops.ip, len, ops.i32_jump()?, boundaries)?;
            Ok(-1)
        }
        // Short jumps (i8 offset)
        Opcode::JMP_SH => {
            check_jump_target(ops.ip, len, ops.i8_jump(), boundaries)?;
            Ok(0)
        }
        Opcode::JIF_SH | Opcode::JNF_SH => {
            check_jump_target(ops.ip, len, ops.i8_jump(), boundaries)?;
            Ok(-1)
        }
        // INV/INV_TAL use packed operand: (fn_id_u24 << 8) | arity_u8
        Opcode::INV => {
            let packed = ops.u32_op();
            let (fn_id, _arity) = musi_bc::unpack_id_arity(packed);
            check_fn_id(fn_id, module, "inv")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(1 - param_count)
        }
        Opcode::INV_TAL => {
            let packed = ops.u32_op();
            let (fn_id, _arity) = musi_bc::unpack_id_arity(packed);
            check_fn_id(fn_id, module, "inv.tal")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(-param_count)
        }
        // MK_VAR uses packed u16 operand: (tag_u8 << 8) | arity_u8
        Opcode::MK_VAR => {
            let packed = ops.u16_op_full_u32();
            let arity = (packed & 0xFF).cast_signed();
            Ok(1 - arity)
        }
        Opcode::MK_PRD => {
            let field_count = i32::try_from(ops.u8_op()).unwrap_or(i32::MAX);
            Ok(1 - field_count)
        }
        Opcode::INV_DYN => {
            let arg_count = i32::try_from(ops.u8_op()).unwrap_or(i32::MAX);
            Ok(-arg_count)
        }
        Opcode::CNT_SAV => {
            let op_id = ops.u32_op();
            let param_count = lookup_effect_op_param_count(module, op_id);
            Ok(1 - param_count)
        }
        Opcode::TSK_SPN => {
            let fn_id = ops.u32_op();
            check_fn_id(fn_id, module, "tsk.spn")?;
            let param_count = module
                .fn_by_id(fn_id)
                .map_or(0, |(_, f)| i32::from(f.param_count));
            Ok(1 - param_count)
        }
        // INV_FFI uses packed operand: (ffi_id_u24 << 8) | arity_u8
        Opcode::INV_FFI => {
            let packed = ops.u32_op();
            let (ffi_id, _arity) = musi_bc::unpack_id_arity(packed);
            let idx = usize::try_from(ffi_id).unwrap_or(usize::MAX);
            if idx >= module.foreign_fns.len() {
                return Err(VmError::Verify {
                    desc: format!("inv.ffi index {idx} out of bounds").into_boxed_str(),
                });
            }
            let param_count = i32::from(module.foreign_fns[idx].param_count);
            Ok(1 - param_count)
        }
        // MK_CLO uses packed operand: (fn_id_u24 << 8) | upval_count_u8
        Opcode::MK_CLO => {
            let packed = ops.u32_op();
            let (fn_id, upval_count) = musi_bc::unpack_id_arity(packed);
            check_fn_id(fn_id, module, "mk.clo")?;
            Ok(1 - i32::from(upval_count))
        }
        _ => Err(VmError::Verify {
            desc: format!("unknown opcode {:#04x}", op.0).into_boxed_str(),
        }),
    }
}

/// Look up the parameter count for an effect operation by its flat index.
fn lookup_effect_op_param_count(module: &LoadedModule, op_id: u32) -> i32 {
    let mut idx = 0u32;
    for effect in &module.effects {
        for op in &effect.ops {
            if idx == op_id {
                return i32::try_from(op.param_type_ids.len()).unwrap_or(0);
            }
            idx += 1;
        }
    }
    0
}

/// Compute the stack depth delta for an opcode and verify its operands.
///
/// Returns the net stack change.
fn verify_op(
    op: Opcode,
    ops: &Operands<'_>,
    len: usize,
    module: &LoadedModule,
    local_count: usize,
    const_len: usize,
    boundaries: &HashSet<usize>,
) -> Result<i32, VmError> {
    if let Some(delta) = fixed_stack_delta(op) {
        return Ok(delta);
    }
    verify_operand_op(op, ops, len, module, local_count, const_len, boundaries)
}

fn verify_fn(func: &LoadedFn, module: &LoadedModule) -> Result<(), VmError> {
    let code = &func.code;
    let const_len = module.consts.len();
    let local_count = usize::from(func.local_count);
    let max_stack = usize::from(func.max_stack);

    let boundaries = collect_boundaries(code)?;

    let mut ip = 0usize;
    let mut depth: i32 = 0;

    while ip < code.len() {
        let raw = code[ip];

        // Handle WID prefix - decode inner opcode and verify with widened operand
        if raw == Opcode::WID {
            let inner_op_byte = code[ip + 1];
            let inner_op = Opcode(inner_op_byte);
            let zone = inner_op_byte >> 6;
            let wid_size = widened_operand_size(zone);
            let total_len = 1 + 1 + wid_size;
            let ops = Operands {
                code,
                ip,
                widened: true,
            };
            depth += verify_op(
                inner_op,
                &ops,
                total_len,
                module,
                local_count,
                const_len,
                &boundaries,
            )?;

            if depth > 0 {
                let d = usize::try_from(depth).unwrap_or(usize::MAX);
                if d > max_stack && max_stack > 0 {
                    return Err(VmError::Verify {
                        desc: format!(
                            "stack depth {d} exceeds max_stack {max_stack} at offset {ip}"
                        )
                        .into_boxed_str(),
                    });
                }
            }

            let is_terminator = matches!(
                inner_op,
                Opcode::RET | Opcode::RET_UT | Opcode::UNR | Opcode::HLT
            );
            if is_terminator {
                depth = 0;
            }
            ip += total_len;
            continue;
        }

        // Handle EXT prefix
        if raw == Opcode::EXT {
            ip += 2;
            continue;
        }

        let op = Opcode(raw);
        let len = instr_len(raw);
        let ops = Operands {
            code,
            ip,
            widened: false,
        };

        depth += verify_op(op, &ops, len, module, local_count, const_len, &boundaries)?;

        if depth > 0 {
            let d = usize::try_from(depth).unwrap_or(usize::MAX);
            if d > max_stack && max_stack > 0 {
                return Err(VmError::Verify {
                    desc: format!("stack depth {d} exceeds max_stack {max_stack} at offset {ip}")
                        .into_boxed_str(),
                });
            }
        }

        let is_terminator = matches!(op, Opcode::RET | Opcode::RET_UT | Opcode::UNR | Opcode::HLT);
        let is_unconditional_jump = matches!(op, Opcode::JMP | Opcode::JMP_SH);
        if is_terminator || is_unconditional_jump {
            depth = 0;
        }

        ip += len;
    }

    Ok(())
}
