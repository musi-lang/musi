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
