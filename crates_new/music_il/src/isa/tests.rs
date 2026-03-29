use std::collections::HashSet;

use super::*;

#[test]
fn test_opcode_inventory_has_unique_bytes() {
    let bytes: HashSet<u8> = (0..=u8::MAX)
        .filter(|byte| Opcode::from_byte(*byte).is_some())
        .collect();
    assert_eq!(bytes.len(), ALL_OPCODES.len());
}

#[test]
fn test_opcode_round_trips_by_byte() {
    for byte in 0..=u8::MAX {
        if let Some(opcode) = Opcode::from_byte(byte) {
            assert!(ALL_OPCODES.contains(&opcode));
        }
    }
}

#[test]
fn test_opcode_round_trips_by_mnemonic() {
    for &opcode in ALL_OPCODES {
        assert_eq!(Opcode::from_mnemonic(opcode.mnemonic()), Some(opcode));
    }
}

#[test]
fn test_opcode_families_match_runtime_domains() {
    assert_eq!(Opcode::LdLoc.family(), OpcodeFamily::Data);
    assert_eq!(Opcode::Dup.family(), OpcodeFamily::Stack);
    assert_eq!(Opcode::IAdd.family(), OpcodeFamily::Scalar);
    assert_eq!(Opcode::And.family(), OpcodeFamily::Logic);
    assert_eq!(Opcode::CmpEq.family(), OpcodeFamily::Compare);
    assert_eq!(Opcode::BrJmp.family(), OpcodeFamily::Branch);
    assert_eq!(Opcode::Call.family(), OpcodeFamily::Call);
    assert_eq!(Opcode::SeqNew.family(), OpcodeFamily::Sequence);
    assert_eq!(Opcode::DataNew.family(), OpcodeFamily::Aggregate);
    assert_eq!(Opcode::TyId.family(), OpcodeFamily::Type);
    assert_eq!(Opcode::EffInvk.family(), OpcodeFamily::Effect);
    assert_eq!(Opcode::FfiCall.family(), OpcodeFamily::Runtime);
}

#[test]
fn test_removed_opcodes_are_not_decodable() {
    for mnemonic in [
        "ld.nil",
        "ld.one",
        "br.back",
        "seq.get.i",
        "seq.set.i",
        "seq.tag",
        "seq.new.tag",
        "ty.tag",
        "tycl.dict",
        "tycl.call",
        "gc.pin",
        "gc.unpin",
        "nop",
        "panic",
        "halt",
    ] {
        assert_eq!(Opcode::from_mnemonic(mnemonic), None);
    }
}

#[test]
fn test_instruction_builders_store_expected_operands() {
    assert_eq!(Instruction::basic(Opcode::Ret).operand, Operand::None);
    assert_eq!(
        Instruction::with_u8(Opcode::LdLoc, 7).operand,
        Operand::U8(7)
    );
    assert_eq!(
        Instruction::with_u16(Opcode::LdConst, 12).operand,
        Operand::U16(12)
    );
    assert_eq!(
        Instruction::with_i16(Opcode::BrJmp, -4).operand,
        Operand::I16(-4)
    );
    assert_eq!(
        Instruction::with_wide(Opcode::ClsNew, 9, 2).operand,
        Operand::Wide(9, 2)
    );
    assert_eq!(
        Instruction::with_type_len(Opcode::SeqNew, 3, 4).operand,
        Operand::TypeLen(3, 4)
    );
    assert_eq!(
        Instruction::with_effect(Opcode::EffInvk, 1, 2).operand,
        Operand::Effect(1, 2)
    );
    assert_eq!(
        Instruction::with_effect_jump(Opcode::HdlPush, 1, 2, -8).operand,
        Operand::EffectJump(1, 2, -8)
    );
    assert_eq!(
        Instruction::with_table(Opcode::BrTbl, vec![3, -2]).operand,
        Operand::BranchTable(vec![3, -2])
    );
}
