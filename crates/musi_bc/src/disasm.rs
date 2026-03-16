//! Bytecode disassembler for debugging.

use core::fmt::{self, Write};

use crate::{Opcode, instr_len};

/// Disassemble a bytecode slice into a human-readable string.
///
/// Each line is formatted as `OFFSET: MNEMONIC [OPERAND]`.
#[must_use]
pub fn disassemble(code: &[u8]) -> String {
    let mut out = String::new();
    let _ok = write_disassembly(&mut out, code);
    out
}

fn write_disassembly(out: &mut String, code: &[u8]) -> fmt::Result {
    let mut ip = 0usize;
    while ip < code.len() {
        let op_byte = code[ip];
        let op = Opcode(op_byte);
        let len = instr_len(op_byte);
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
                writeln!(out, "{ip:04x}: {op} {operand}")?;
            }
            3 => {
                let lo = code[ip + 1];
                let hi = code[ip + 2];
                let operand = u16::from_le_bytes([lo, hi]);
                writeln!(out, "{ip:04x}: {op} {operand}")?;
            }
            _ => {
                let b = [code[ip + 1], code[ip + 2], code[ip + 3], code[ip + 4]];
                if matches!(op, Opcode::JMP_W | Opcode::JMP_T_W | Opcode::JMP_F_W) {
                    let offset = i32::from_le_bytes(b);
                    let target = ip
                        .wrapping_add(len)
                        .wrapping_add_signed(isize::try_from(offset).unwrap_or(0));
                    writeln!(out, "{ip:04x}: {op} {offset:+} (-> {target:04x})")?;
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
