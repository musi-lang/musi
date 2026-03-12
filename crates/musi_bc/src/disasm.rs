//! Bytecode disassembler for debugging.

use core::fmt::Write;

use crate::{Opcode, instr_len};

/// Disassemble a bytecode slice into a human-readable string.
///
/// Each line is formatted as `OFFSET: MNEMONIC [OPERAND]`.
#[must_use]
pub fn disassemble(code: &[u8]) -> String {
    let mut out = String::new();
    let mut ip = 0usize;
    while ip < code.len() {
        let op_byte = code[ip];
        let op = Opcode(op_byte);
        let len = instr_len(op_byte);
        if ip + len > code.len() {
            let _ = writeln!(out, "{ip:04x}: <truncated>");
            break;
        }
        match len {
            1 => {
                let _ = writeln!(out, "{ip:04x}: {op}");
            }
            2 => {
                let operand = code[ip + 1];
                let _ = writeln!(out, "{ip:04x}: {op} {operand}");
            }
            3 => {
                let lo = code[ip + 1];
                let hi = code[ip + 2];
                let operand = u16::from_le_bytes([lo, hi]);
                let _ = writeln!(out, "{ip:04x}: {op} {operand}");
            }
            _ => {
                let b = [code[ip + 1], code[ip + 2], code[ip + 3], code[ip + 4]];
                if matches!(op, Opcode::JMP_W | Opcode::JMP_T_W | Opcode::JMP_F_W) {
                    let offset = i32::from_le_bytes(b);
                    let target = ip
                        .wrapping_add(len)
                        .wrapping_add_signed(isize::try_from(offset).unwrap_or(0));
                    let _ = writeln!(out, "{ip:04x}: {op} {offset:+} (-> {target:04x})");
                } else {
                    let operand = u32::from_le_bytes(b);
                    let _ = writeln!(out, "{ip:04x}: {op} {operand}");
                }
            }
        }
        ip += len;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{encode_i32, encode_no_operand, encode_u8};

    #[test]
    fn test_disassemble_simple_sequence() {
        let mut code = vec![];
        encode_no_operand(&mut code, Opcode::NOP);
        encode_u8(&mut code, Opcode::LD_LOC, 3);
        encode_no_operand(&mut code, Opcode::RET);

        let output = disassemble(&code);
        assert!(output.contains("0000: nop"), "got: {output}");
        assert!(output.contains("0001: ld.loc 3"), "got: {output}");
        assert!(output.contains("0003: ret"), "got: {output}");
    }

    #[test]
    fn test_disassemble_jump_shows_target() {
        let mut code = vec![];
        encode_i32(&mut code, Opcode::JMP_W, 10);
        let output = disassemble(&code);
        assert!(output.contains("jmp.w +10 (-> 000f)"), "got: {output}");
    }

    #[test]
    fn test_disassemble_empty() {
        let output = disassemble(&[]);
        assert!(output.is_empty());
    }
}
