use std::collections::HashSet;

use super::{Opcode, encode_i32, encode_i8, pack_id_arity, pack_tag_arity_u16, unpack_id_arity, unpack_tag_arity_u16};

/// All no-operand opcodes (range 0x00–0x3F).
const NO_OPERAND_OPCODES: &[Opcode] = &[
    // §CTL
    Opcode::NOP,
    Opcode::HLT,
    Opcode::RET,
    Opcode::RET_UT,
    Opcode::UNR,
    Opcode::BRK,
    // §STK
    Opcode::DUP,
    Opcode::POP,
    Opcode::SWP,
    Opcode::LD_UT,
    // §DAT
    Opcode::LD_TAG,
    Opcode::LD_LEN,
    Opcode::LD_IDX,
    Opcode::ST_IDX,
    Opcode::TSK_AWT,
    // §INT
    Opcode::INT_ADD,
    Opcode::INT_SUB,
    Opcode::INT_MUL,
    Opcode::INT_DIV,
    Opcode::INT_REM,
    Opcode::INT_NEG,
    // §NAT
    Opcode::NAT_ADD,
    Opcode::NAT_SUB,
    Opcode::NAT_MUL,
    Opcode::NAT_DIV,
    Opcode::NAT_REM,
    // §FLT
    Opcode::FLT_ADD,
    Opcode::FLT_SUB,
    Opcode::FLT_MUL,
    Opcode::FLT_DIV,
    Opcode::FLT_REM,
    Opcode::FLT_NEG,
    // §BIT
    Opcode::BIT_AND,
    Opcode::BIT_OR,
    Opcode::BIT_XOR,
    Opcode::BIT_NOT,
    Opcode::BIT_SHL,
    Opcode::BIT_SHR,
    Opcode::BIT_SRU,
    // §CMP
    Opcode::CMP_EQ,
    Opcode::CMP_NE,
    Opcode::CMP_LT,
    Opcode::CMP_LE,
    Opcode::CMP_GT,
    Opcode::CMP_GE,
    Opcode::CMP_LTU,
    Opcode::CMP_LEU,
    Opcode::CMP_GTU,
    Opcode::CMP_GEU,
    Opcode::CMP_FLT,
    Opcode::CMP_FLE,
    Opcode::CMP_FGT,
    Opcode::CMP_FGE,
    // §CNV
    Opcode::CNV_ITF,
    Opcode::CNV_FTI,
];

/// All u8-operand opcodes (range 0x40–0x7F).
const U8_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::LD_LOC,
    Opcode::ST_LOC,
    Opcode::LD_CST,
    Opcode::LD_FLD,
    Opcode::ST_FLD,
    Opcode::LD_UPV,
    Opcode::LD_PAY,
    Opcode::MK_PRD,
    Opcode::CMP_TAG,
    Opcode::INV_DYN,
    Opcode::JMP_SH,
    Opcode::JIF_SH,
    Opcode::JNF_SH,
    Opcode::CNT_MRK,
    Opcode::CNT_UMK,
];

/// All u16-operand opcodes (range 0x80–0xBF).
const U16_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::MK_VAR,
];

/// All u32-operand opcodes (range 0xC0–0xFD).
const U32_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::INV,
    Opcode::INV_TAL,
    Opcode::INV_FFI,
    Opcode::JMP,
    Opcode::JIF,
    Opcode::JNF,
    Opcode::LD_GLB,
    Opcode::ST_GLB,
    Opcode::MK_ARR,
    Opcode::ALC_REF,
    Opcode::ALC_ARN,
    Opcode::MK_CLO,
    Opcode::CNT_SAV,
    Opcode::CNT_RSM,
    Opcode::TSK_SPN,
    Opcode::TSK_CHS,
    Opcode::TSK_CHR,
    Opcode::TSK_CMK,
    Opcode::TYP_CHK,
];

#[test]
fn test_all_no_operand_opcodes_in_range_0x00_0x3f() {
    for &op in NO_OPERAND_OPCODES {
        assert_eq!(
            op.0 >> 6,
            0,
            "no-operand opcode {:#04x} must be in range 0x00–0x3F",
            op.0
        );
    }
}

#[test]
fn test_all_u8_operand_opcodes_in_range_0x40_0x7f() {
    for &op in U8_OPERAND_OPCODES {
        assert_eq!(
            op.0 >> 6,
            1,
            "u8-operand opcode {:#04x} must be in range 0x40–0x7F",
            op.0
        );
    }
}

#[test]
fn test_all_u16_operand_opcodes_in_range_0x80_0xbf() {
    for &op in U16_OPERAND_OPCODES {
        assert_eq!(
            op.0 >> 6,
            2,
            "u16-operand opcode {:#04x} must be in range 0x80–0xBF",
            op.0
        );
    }
}

#[test]
fn test_all_u32_operand_opcodes_in_range_0xc0_0xfd() {
    for &op in U32_OPERAND_OPCODES {
        assert!(
            op.0 >= 0xC0 && op.0 <= 0xFD,
            "u32-operand opcode {:#04x} must be in range 0xC0–0xFD",
            op.0
        );
    }
}

#[test]
fn test_encode_i32_produces_five_bytes_le_signed() {
    let mut buf = vec![];
    encode_i32(&mut buf, Opcode::JMP, -5);
    assert_eq!(buf.len(), 5);
    assert_eq!(buf[0], Opcode::JMP.0);
    let operand = i32::from_le_bytes([buf[1], buf[2], buf[3], buf[4]]);
    assert_eq!(operand, -5);
}

#[test]
fn test_encode_i8_produces_two_bytes() {
    let mut buf = vec![];
    encode_i8(&mut buf, Opcode::JMP_SH, -10);
    assert_eq!(buf.len(), 2);
    assert_eq!(buf[0], Opcode::JMP_SH.0);
    assert_eq!(buf[1] as i8, -10);
}

#[test]
fn test_no_duplicate_opcode_values() {
    let all: Vec<u8> = NO_OPERAND_OPCODES
        .iter()
        .chain(U8_OPERAND_OPCODES)
        .chain(U16_OPERAND_OPCODES)
        .chain(U32_OPERAND_OPCODES)
        .map(|op| op.0)
        .collect();

    let unique: HashSet<u8> = all.iter().copied().collect();
    assert_eq!(
        all.len(),
        unique.len(),
        "duplicate opcode values found among {} opcodes",
        all.len()
    );
}

#[test]
fn test_display_known_opcodes() {
    assert_eq!(format!("{}", Opcode::NOP), "nop");
    assert_eq!(format!("{}", Opcode::LD_LOC), "ld.loc");
    assert_eq!(format!("{}", Opcode::CMP_EQ), "cmp.eq");
    assert_eq!(format!("{}", Opcode::JMP), "jmp");
    assert_eq!(format!("{}", Opcode::INV_FFI), "inv.ffi");
    assert_eq!(format!("{}", Opcode::INT_ADD), "int.add");
    assert_eq!(format!("{}", Opcode::NAT_ADD), "nat.add");
    assert_eq!(format!("{}", Opcode::FLT_ADD), "flt.add");
    assert_eq!(format!("{}", Opcode::BIT_AND), "bit.and");
    assert_eq!(format!("{}", Opcode::MK_VAR), "mk.var");
    assert_eq!(format!("{}", Opcode::JMP_SH), "jmp.sh");
    assert_eq!(format!("{}", Opcode::LD_UT), "ld.ut");
    assert_eq!(format!("{}", Opcode::RET_UT), "ret.ut");
}

#[test]
fn test_display_unknown_opcode() {
    assert_eq!(format!("{}", Opcode(0x0F)), "0x0F");
}

#[test]
fn test_pack_unpack_id_arity() {
    let packed = pack_id_arity(0x123456, 42);
    let (id, arity) = unpack_id_arity(packed);
    assert_eq!(id, 0x123456);
    assert_eq!(arity, 42);
}

#[test]
fn test_pack_unpack_tag_arity_u16() {
    let packed = pack_tag_arity_u16(7, 3);
    let (tag, arity) = unpack_tag_arity_u16(packed);
    assert_eq!(tag, 7);
    assert_eq!(arity, 3);
}

#[test]
fn test_opcode_count() {
    let total = NO_OPERAND_OPCODES.len()
        + U8_OPERAND_OPCODES.len()
        + U16_OPERAND_OPCODES.len()
        + U32_OPERAND_OPCODES.len();
    assert_eq!(total, 90, "expected 90 assigned opcodes, got {total}");
}
