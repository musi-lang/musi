use std::collections::HashSet;

use super::{Opcode, encode_i32};

/// All no-operand opcodes (range 0x00–0x3F).
const NO_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::NOP,
    Opcode::HLT,
    Opcode::RET,
    Opcode::RET_U,
    Opcode::UNR,
    Opcode::BRK,
    Opcode::DUP,
    Opcode::POP,
    Opcode::SWP,
    Opcode::LD_TAG,
    Opcode::LD_LEN,
    Opcode::LD_IDX,
    Opcode::ST_IDX,
    Opcode::FRE,
    Opcode::EFF_RES_C,
    Opcode::EFF_ABT,
    Opcode::I_ADD,
    Opcode::I_ADD_UN,
    Opcode::I_SUB,
    Opcode::I_SUB_UN,
    Opcode::I_MUL,
    Opcode::I_MUL_UN,
    Opcode::I_DIV,
    Opcode::I_DIV_UN,
    Opcode::I_REM,
    Opcode::I_REM_UN,
    Opcode::I_NEG,
    Opcode::TSK_AWT,
    Opcode::F_ADD,
    Opcode::F_SUB,
    Opcode::F_MUL,
    Opcode::F_DIV,
    Opcode::F_REM,
    Opcode::F_NEG,
    Opcode::B_AND,
    Opcode::B_OR,
    Opcode::B_XOR,
    Opcode::B_NOT,
    Opcode::B_SHL,
    Opcode::B_SHR,
    Opcode::B_SHR_UN,
    Opcode::CMP_EQ,
    Opcode::CMP_NE,
    Opcode::CMP_LT,
    Opcode::CMP_LT_UN,
    Opcode::CMP_LE,
    Opcode::CMP_LE_UN,
    Opcode::CMP_GT,
    Opcode::CMP_GT_UN,
    Opcode::CMP_GE,
    Opcode::CMP_GE_UN,
    Opcode::CMP_F_EQ,
    Opcode::CMP_F_NE,
    Opcode::CMP_F_LT,
    Opcode::CMP_F_LE,
    Opcode::CMP_F_GT,
    Opcode::CMP_F_GE,
    Opcode::CNV_ITF,
    Opcode::CNV_FTI,
    Opcode::CNV_TRM,
];

/// All u8-operand opcodes (range 0x40–0x7F).
const U8_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::LD_LOC,
    Opcode::ST_LOC,
    Opcode::LD_CST,
    Opcode::MK_PRD,
    Opcode::LD_FLD,
    Opcode::MK_VAR,
    Opcode::LD_PAY,
    Opcode::CMP_TAG,
    Opcode::EFF_PSH,
    Opcode::EFF_POP,
    Opcode::INV_DYN,
];

/// All u16-operand opcodes (range 0x80–0xBF).
const U16_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::LD_LOC_W,
    Opcode::ST_LOC_W,
    Opcode::LD_CST_W,
    Opcode::MK_VAR_W,
    Opcode::CMP_TAG_W,
];

/// All u32-operand opcodes (range 0xC0–0xFF).
const U32_OPERAND_OPCODES: &[Opcode] = &[
    Opcode::INV,
    Opcode::INV_EFF,
    Opcode::INV_TAL,
    Opcode::INV_TAL_EFF,
    Opcode::LD_GLB,
    Opcode::ST_GLB,
    Opcode::MK_ARR,
    Opcode::ALC_REF,
    Opcode::ALC_ARN,
    Opcode::EFF_DO,
    Opcode::EFF_RES,
    Opcode::TSK_SPN,
    Opcode::TSK_CHS,
    Opcode::TSK_CHR,
    Opcode::TSK_CMK,
    Opcode::INV_FFI,
    Opcode::JMP_W,
    Opcode::JMP_T_W,
    Opcode::JMP_F_W,
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
fn test_all_u32_operand_opcodes_in_range_0xc0_0xff() {
    for &op in U32_OPERAND_OPCODES {
        assert_eq!(
            op.0 >> 6,
            3,
            "u32-operand opcode {:#04x} must be in range 0xC0–0xFF",
            op.0
        );
    }
}

#[test]
fn test_encode_i32_produces_five_bytes_le_signed() {
    let mut buf = vec![];
    encode_i32(&mut buf, Opcode::JMP_W, -5);
    assert_eq!(buf.len(), 5);
    assert_eq!(buf[0], Opcode::JMP_W.0);
    let operand = i32::from_le_bytes([buf[1], buf[2], buf[3], buf[4]]);
    assert_eq!(operand, -5);
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
    assert_eq!(format!("{}", Opcode::JMP_W), "jmp.w");
    assert_eq!(format!("{}", Opcode::INV_FFI), "inv.ffi");
}

#[test]
fn test_display_unknown_opcode() {
    assert_eq!(format!("{}", Opcode(0xFF)), "0xFF");
}
