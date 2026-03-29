use super::*;

#[test]
fn simple_instruction_has_no_operand() {
    let instr = Instruction::simple(Opcode::Ret);
    assert_eq!(instr.opcode, Opcode::Ret);
    assert_eq!(instr.operand, Operand::None);
}

#[test]
fn with_u8_stores_byte_operand() {
    let instr = Instruction::with_u8(Opcode::LdLoc, 42);
    assert_eq!(instr.opcode, Opcode::LdLoc);
    assert_eq!(instr.operand, Operand::U8(42));
}

#[test]
fn with_u16_stores_wide_index() {
    let instr = Instruction::with_u16(Opcode::LdConst, 1000);
    assert_eq!(instr.opcode, Opcode::LdConst);
    assert_eq!(instr.operand, Operand::U16(1000));
}

#[test]
fn with_i16_stores_signed_offset() {
    let instr = Instruction::with_i16(Opcode::BrJmp, -128);
    assert_eq!(instr.opcode, Opcode::BrJmp);
    assert_eq!(instr.operand, Operand::I16(-128));
}

#[test]
fn with_wide_stores_fn_ref_and_upval_count() {
    let instr = Instruction::with_wide(Opcode::ClsNew, 500, 3);
    assert_eq!(instr.opcode, Opcode::ClsNew);
    assert_eq!(instr.operand, Operand::Wide(500, 3));
}

#[test]
fn with_type_tagged_stores_type_tag_and_length() {
    let instr = Instruction::with_type_tagged(Opcode::ArrNewT, 7, 1, 2);
    assert_eq!(instr.opcode, Opcode::ArrNewT);
    assert_eq!(instr.operand, Operand::TypeTagged(7, 1, 2));
}

#[test]
fn with_table_stores_offset_vec() {
    let offsets = vec![10, -20, 30];
    let instr = Instruction::with_table(Opcode::BrTbl, offsets.clone());
    assert_eq!(instr.opcode, Opcode::BrTbl);
    assert_eq!(instr.operand, Operand::Table(offsets));
}

#[test]
fn with_indexed_jump_stores_both_values() {
    let instr = Instruction::with_indexed_jump(Opcode::HdlPush, 7, -42);
    assert_eq!(instr.opcode, Opcode::HdlPush);
    assert_eq!(instr.operand, Operand::IndexedJump(7, -42));
}

#[test]
fn instruction_clone_is_equal() {
    let instr = Instruction::with_u16(Opcode::ArrNew, 64);
    let cloned = instr.clone();
    assert_eq!(instr, cloned);
}

#[test]
fn operand_none_debug_format() {
    let operand = Operand::None;
    let debug = format!("{operand:?}");
    assert_eq!(debug, "None");
}
