use super::*;
use crate::{Opcode, encode_f0, encode_fi8, encode_fi8x2, encode_fi16, encode_fi24};

#[test]
fn test_disassemble_empty() {
    assert!(disassemble(&[]).is_empty());
}

#[test]
fn test_disassemble_f0_sequence() {
    let mut code = vec![];
    encode_f0(&mut code, Opcode::NOP);
    encode_f0(&mut code, Opcode::ADD);
    encode_f0(&mut code, Opcode::RET);

    let out = disassemble(&code);
    assert!(out.contains("0000: nop"), "got: {out}");
    assert!(out.contains("0001: add"), "got: {out}");
    assert!(out.contains("0002: ret"), "got: {out}");
}

#[test]
fn test_disassemble_fi8() {
    let mut code = vec![];
    encode_fi8(&mut code, Opcode::LD_LOC, 7);
    let out = disassemble(&code);
    assert!(out.contains("0000: ld.loc 7"), "got: {out}");
}

#[test]
fn test_disassemble_fi16_non_branch() {
    let mut code = vec![];
    encode_fi16(&mut code, Opcode::LD_CONST, 256);
    let out = disassemble(&code);
    assert!(out.contains("0000: ld.const 256"), "got: {out}");
}

#[test]
fn test_disassemble_fi16_big_endian_decode() {
    // 0x0102 = 258; big-endian bytes: [0x01, 0x02]
    let mut code = vec![];
    encode_fi16(&mut code, Opcode::LD_SMI, 0x0102);
    assert_eq!(code[1], 0x01);
    assert_eq!(code[2], 0x02);
    let out = disassemble(&code);
    assert!(out.contains("ld.smi 258"), "got: {out}");
}

#[test]
fn test_disassemble_branch_shows_hex_target() {
    let mut code = vec![];
    encode_fi16(&mut code, Opcode::BR, 0x0010);
    let out = disassemble(&code);
    assert!(out.contains("br 0x0010"), "got: {out}");
}

#[test]
fn test_disassemble_br_true_shows_hex_target() {
    let mut code = vec![];
    encode_fi16(&mut code, Opcode::BR_TRUE, 0x00FF);
    let out = disassemble(&code);
    assert!(out.contains("br.true 0x00ff"), "got: {out}");
}

#[test]
fn test_disassemble_br_false_shows_hex_target() {
    let mut code = vec![];
    encode_fi16(&mut code, Opcode::BR_FALSE, 0x0042);
    let out = disassemble(&code);
    assert!(out.contains("br.false 0x0042"), "got: {out}");
}

#[test]
fn test_disassemble_br_long_positive_offset() {
    // ip=0, len=4, offset=+10, target = 4 + 10 = 14 = 0x000e
    let mut code = vec![];
    encode_fi24(&mut code, Opcode::BR_LONG, 10);
    let out = disassemble(&code);
    assert!(out.contains("br.long +10 (-> 000e)"), "got: {out}");
}

#[test]
fn test_disassemble_br_long_negative_offset() {
    // ip=0, len=4, offset=-4, target = 4 + (-4) = 0 = 0x0000
    let mut code = vec![];
    encode_fi24(&mut code, Opcode::BR_LONG, -4);
    let out = disassemble(&code);
    assert!(out.contains("br.long -4 (-> 0000)"), "got: {out}");
}

#[test]
fn test_disassemble_fi8x2_rec_new() {
    let mut code = vec![];
    encode_fi8x2(&mut code, Opcode::REC_NEW, 5, 3);
    let out = disassemble(&code);
    assert!(out.contains("0000: rec.new 5 3"), "got: {out}");
}

#[test]
fn test_disassemble_fi8x2_eff_need() {
    let mut code = vec![];
    encode_fi8x2(&mut code, Opcode::EFF_NEED, 2, 1);
    let out = disassemble(&code);
    assert!(out.contains("0000: eff.need 2 1"), "got: {out}");
}

#[test]
fn test_disassemble_fi8x2_ffi_call() {
    let mut code = vec![];
    encode_fi8x2(&mut code, Opcode::FFI_CALL, 10, 4);
    let out = disassemble(&code);
    assert!(out.contains("0000: ffi.call 10 4"), "got: {out}");
}

#[test]
fn test_disassemble_mixed_formats() {
    let mut code = vec![];
    encode_f0(&mut code, Opcode::DUP); // 0000: 1 byte
    encode_fi8(&mut code, Opcode::ST_LOC, 2); // 0001: 2 bytes
    encode_fi16(&mut code, Opcode::CLS_NEW, 99); // 0003: 3 bytes
    encode_f0(&mut code, Opcode::RET); // 0006: 1 byte

    let out = disassemble(&code);
    assert!(out.contains("0000: dup"), "got: {out}");
    assert!(out.contains("0001: st.loc 2"), "got: {out}");
    assert!(out.contains("0003: cls.new 99"), "got: {out}");
    assert!(out.contains("0006: ret"), "got: {out}");
}

#[test]
fn test_disassemble_truncated_fi8() {
    // Opcode byte only, missing operand byte.
    let code = vec![Opcode::LD_LOC.0];
    let out = disassemble(&code);
    assert!(out.contains("<truncated>"), "got: {out}");
}

#[test]
fn test_disassemble_truncated_fi16() {
    // Opcode + one byte, missing second operand byte.
    let code = vec![Opcode::LD_CONST.0, 0x00];
    let out = disassemble(&code);
    assert!(out.contains("<truncated>"), "got: {out}");
}

#[test]
fn test_disassemble_truncated_fi24() {
    // Opcode + two bytes, missing third operand byte.
    let code = vec![Opcode::BR_LONG.0, 0x00, 0x00];
    let out = disassemble(&code);
    assert!(out.contains("<truncated>"), "got: {out}");
}

#[test]
fn test_disassemble_unassigned_opcode_advances_one_byte() {
    // 0x50 is unassigned; treated as F0 so the disassembler doesn't stall.
    let code = vec![0x50u8, Opcode::NOP.0];
    let out = disassemble(&code);
    assert!(out.contains("0001: nop"), "got: {out}");
}
