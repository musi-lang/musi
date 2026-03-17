use super::*;
use crate::{encode_i32, encode_i8, encode_no_operand, encode_u8, encode_u16, encode_u32,
            encode_wid_u16, encode_wid_u32, pack_id_arity, pack_tag_arity_u16};

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
fn test_disassemble_long_jump_shows_target() {
    let mut code = vec![];
    encode_i32(&mut code, Opcode::JMP, 10);
    let output = disassemble(&code);
    assert!(output.contains("jmp +10 (-> 000f)"), "got: {output}");
}

#[test]
fn test_disassemble_short_jump_shows_target() {
    let mut code = vec![];
    encode_i8(&mut code, Opcode::JMP_SH, -5);
    let output = disassemble(&code);
    // ip=0, len=2, offset=-5, target = 2 + (-5) = wrapping
    assert!(output.contains("jmp.sh -5"), "got: {output}");
}

#[test]
fn test_disassemble_packed_inv() {
    let mut code = vec![];
    let packed = pack_id_arity(42, 3);
    encode_u32(&mut code, Opcode::INV, packed);
    let output = disassemble(&code);
    assert!(output.contains("inv id=42 arity=3"), "got: {output}");
}

#[test]
fn test_disassemble_mk_var_packed() {
    let mut code = vec![];
    let packed = pack_tag_arity_u16(5, 2);
    encode_u16(&mut code, Opcode::MK_VAR, packed);
    let output = disassemble(&code);
    assert!(output.contains("mk.var tag=5 arity=2"), "got: {output}");
}

#[test]
fn test_disassemble_wid_prefix() {
    let mut code = vec![];
    encode_wid_u16(&mut code, Opcode::LD_LOC, 300);
    let output = disassemble(&code);
    assert!(output.contains("wid ld.loc 300"), "got: {output}");
}

#[test]
fn test_disassemble_wid_mk_var() {
    let mut code = vec![];
    // WID + MK_VAR: zone 2 widened to u32, packed (tag_u24 << 8 | arity_u8)
    let packed = (1000u32 << 8) | 4;
    encode_wid_u32(&mut code, Opcode::MK_VAR, packed);
    let output = disassemble(&code);
    assert!(output.contains("wid mk.var tag=1000 arity=4"), "got: {output}");
}

#[test]
fn test_disassemble_empty() {
    let output = disassemble(&[]);
    assert!(output.is_empty());
}

#[test]
fn test_disassemble_new_mnemonics() {
    let mut code = vec![];
    encode_no_operand(&mut code, Opcode::INT_ADD);
    encode_no_operand(&mut code, Opcode::NAT_ADD);
    encode_no_operand(&mut code, Opcode::FLT_ADD);
    encode_no_operand(&mut code, Opcode::BIT_AND);
    encode_no_operand(&mut code, Opcode::LD_UT);
    encode_no_operand(&mut code, Opcode::RET_UT);

    let output = disassemble(&code);
    assert!(output.contains("int.add"), "got: {output}");
    assert!(output.contains("nat.add"), "got: {output}");
    assert!(output.contains("flt.add"), "got: {output}");
    assert!(output.contains("bit.and"), "got: {output}");
    assert!(output.contains("ld.ut"), "got: {output}");
    assert!(output.contains("ret.ut"), "got: {output}");
}
