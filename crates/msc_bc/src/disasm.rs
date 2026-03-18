//! Bytecode disassembler for the SEAM bytecode format.
//!
//! Each line: `OFFSET: MNEMONIC [OPERAND]`

use core::fmt::{self, Write};

use crate::{Format, Opcode, format as opcode_format, instr_len};

/// Disassemble a bytecode slice into a human-readable string.
#[must_use]
pub fn disassemble(code: &[u8]) -> String {
    let mut out = String::new();
    let _ok = write_disassembly(&mut out, code);
    out
}

/// Opcodes that use a u16 BE operand as a branch target address.
const fn is_branch(op: Opcode) -> bool {
    matches!(op, Opcode::BR | Opcode::BR_TRUE | Opcode::BR_FALSE)
}

/// Decode a 24-bit big-endian signed value from three bytes.
fn decode_i24(hi: u8, mid: u8, lo: u8) -> i32 {
    let bits = (u32::from(hi) << 16) | (u32::from(mid) << 8) | u32::from(lo);
    if bits & 0x80_0000 != 0 {
        (bits | 0xFF00_0000).cast_signed()
    } else {
        bits.cast_signed()
    }
}

fn write_disassembly(out: &mut String, code: &[u8]) -> fmt::Result {
    let mut ip = 0usize;
    while ip < code.len() {
        let raw = code[ip];
        let op = Opcode(raw);
        let fmt = opcode_format(raw);
        let len = instr_len(raw);

        if ip + len > code.len() {
            writeln!(out, "{ip:04x}: <truncated>")?;
            break;
        }

        match fmt {
            Format::F0 => {
                writeln!(out, "{ip:04x}: {op}")?;
            }
            Format::FI8 => {
                let arg = code[ip + 1];
                writeln!(out, "{ip:04x}: {op} {arg}")?;
            }
            Format::FI16 => {
                let arg = u16::from_be_bytes([code[ip + 1], code[ip + 2]]);
                if is_branch(op) {
                    writeln!(out, "{ip:04x}: {op} {arg:#06x}")?;
                } else {
                    writeln!(out, "{ip:04x}: {op} {arg}")?;
                }
            }
            Format::FI8x2 => {
                let a = code[ip + 1];
                let b = code[ip + 2];
                if op == Opcode::CLS_UPV {
                    let kind = if a == 0 { "local" } else { "parent" };
                    writeln!(out, "{ip:04x}: {op} {kind}:{b}")?;
                } else {
                    writeln!(out, "{ip:04x}: {op} {a} {b}")?;
                }
            }
            Format::FI24 => {
                let offset = decode_i24(code[ip + 1], code[ip + 2], code[ip + 3]);
                // i32→isize: lossless on 32/64-bit (only targets we support).
                #[expect(clippy::as_conversions)]
                let target = (ip + len).wrapping_add_signed(offset as isize);
                writeln!(out, "{ip:04x}: {op} {offset:+} (-> {target:04x})")?;
            }
        }

        ip += len;
    }
    Ok(())
}

#[cfg(test)]
mod tests;
