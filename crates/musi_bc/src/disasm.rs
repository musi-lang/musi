//! Bytecode disassembler for debugging.

use core::fmt::{self, Write};

use crate::{Opcode, instr_len, unpack_id_arity, unpack_tag_arity_u16, unpack_tag_arity_u32, widened_operand_size};

/// Disassemble a bytecode slice into a human-readable string.
///
/// Each line is formatted as `OFFSET: MNEMONIC [OPERAND]`.
#[must_use]
pub fn disassemble(code: &[u8]) -> String {
    let mut out = String::new();
    let _ok = write_disassembly(&mut out, code);
    out
}

/// Opcodes whose u32 operand is a packed `(id_u24 << 8) | arity_u8`.
fn is_packed_id_arity(op: Opcode) -> bool {
    matches!(op, Opcode::INV | Opcode::INV_TAL | Opcode::INV_FFI | Opcode::MK_CLO)
}

/// Opcodes that use i32 jump offsets.
fn is_long_jump(op: Opcode) -> bool {
    matches!(op, Opcode::JMP | Opcode::JIF | Opcode::JNF)
}

/// Opcodes that use i8 short jump offsets.
fn is_short_jump(op: Opcode) -> bool {
    matches!(op, Opcode::JMP_SH | Opcode::JIF_SH | Opcode::JNF_SH)
}

fn write_disassembly(out: &mut String, code: &[u8]) -> fmt::Result {
    let mut ip = 0usize;
    while ip < code.len() {
        let start_ip = ip;
        let raw = code[ip];

        // Handle WID prefix
        if raw == Opcode::WID {
            ip += 1;
            if ip >= code.len() {
                writeln!(out, "{start_ip:04x}: wid <truncated>")?;
                break;
            }
            let inner_op_byte = code[ip];
            let inner_op = Opcode(inner_op_byte);
            let zone = inner_op_byte >> 6;
            let wid_operand_size = widened_operand_size(zone);
            ip += 1; // past inner opcode

            if ip + wid_operand_size > code.len() {
                writeln!(out, "{start_ip:04x}: wid {inner_op} <truncated>")?;
                break;
            }

            let operand = match wid_operand_size {
                1 => u32::from(code[ip]),
                2 => u32::from(u16::from_le_bytes([code[ip], code[ip + 1]])),
                4 => u32::from_le_bytes([code[ip], code[ip + 1], code[ip + 2], code[ip + 3]]),
                _ => 0,
            };
            ip += wid_operand_size;

            // MK_VAR with WID: operand is u32 packed (tag_u24 << 8 | arity_u8)
            if inner_op == Opcode::MK_VAR {
                let (tag, arity) = unpack_tag_arity_u32(operand);
                writeln!(out, "{start_ip:04x}: wid {inner_op} tag={tag} arity={arity}")?;
            } else {
                writeln!(out, "{start_ip:04x}: wid {inner_op} {operand}")?;
            }
            continue;
        }

        // Handle EXT prefix
        if raw == Opcode::EXT {
            ip += 1;
            if ip >= code.len() {
                writeln!(out, "{start_ip:04x}: ext <truncated>")?;
                break;
            }
            let ext_op = code[ip];
            ip += 1;
            writeln!(out, "{start_ip:04x}: ext.{ext_op:#04x}")?;
            continue;
        }

        let op = Opcode(raw);
        let len = instr_len(raw);
        if ip + len > code.len() {
            writeln!(out, "{ip:04x}: <truncated>")?;
            break;
        }

        match len {
            1 => {
                writeln!(out, "{ip:04x}: {op}")?;
            }
            2 => {
                let operand = code[ip + 1];
                if is_short_jump(op) {
                    let offset = operand as i8;
                    let target = (ip + len).wrapping_add_signed(isize::from(offset));
                    writeln!(out, "{ip:04x}: {op} {offset:+} (-> {target:04x})")?;
                } else {
                    writeln!(out, "{ip:04x}: {op} {operand}")?;
                }
            }
            3 => {
                let lo = code[ip + 1];
                let hi = code[ip + 2];
                let operand = u16::from_le_bytes([lo, hi]);
                if op == Opcode::MK_VAR {
                    let (tag, arity) = unpack_tag_arity_u16(operand);
                    writeln!(out, "{ip:04x}: {op} tag={tag} arity={arity}")?;
                } else {
                    writeln!(out, "{ip:04x}: {op} {operand}")?;
                }
            }
            _ => {
                let b = [code[ip + 1], code[ip + 2], code[ip + 3], code[ip + 4]];
                if is_long_jump(op) {
                    let offset = i32::from_le_bytes(b);
                    let target = ip
                        .wrapping_add(len)
                        .wrapping_add_signed(isize::try_from(offset).unwrap_or(0));
                    writeln!(out, "{ip:04x}: {op} {offset:+} (-> {target:04x})")?;
                } else if is_packed_id_arity(op) {
                    let packed = u32::from_le_bytes(b);
                    let (id, arity) = unpack_id_arity(packed);
                    writeln!(out, "{ip:04x}: {op} id={id} arity={arity}")?;
                } else {
                    let operand = u32::from_le_bytes(b);
                    writeln!(out, "{ip:04x}: {op} {operand}")?;
                }
            }
        }
        ip += len;
    }
    Ok(())
}

#[cfg(test)]
mod tests;
