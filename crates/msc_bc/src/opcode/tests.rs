use std::collections::HashSet;

use super::{Format, Opcode, format, instr_len};

// All 81 defined opcodes with their expected formats.
const OPCODE_FORMAT_TABLE: &[(Opcode, Format)] = &[
    // §4.1 Data Movement
    (Opcode::LD_CONST, Format::FI16),
    (Opcode::LD_SMI, Format::FI16),
    (Opcode::LD_TRUE, Format::F0),
    (Opcode::LD_FALSE, Format::F0),
    (Opcode::LD_UNIT, Format::F0),
    (Opcode::LD_NONE, Format::F0),
    (Opcode::LD_LOC, Format::FI8),
    (Opcode::LD_UPV, Format::FI8),
    (Opcode::LD_GLB, Format::FI8),
    (Opcode::LD_ADDR, Format::FI8),
    (Opcode::LD_IND, Format::F0),
    (Opcode::ST_LOC, Format::FI8),
    (Opcode::ST_UPV, Format::FI8),
    (Opcode::ST_GLB, Format::FI8),
    (Opcode::ST_IND, Format::F0),
    // §4.2 Stack
    (Opcode::POP, Format::F0),
    (Opcode::DUP, Format::F0),
    (Opcode::SWAP, Format::F0),
    // §4.3 Arithmetic
    (Opcode::ADD, Format::F0),
    (Opcode::SUB, Format::F0),
    (Opcode::MUL, Format::F0),
    (Opcode::DIV, Format::F0),
    (Opcode::REM, Format::F0),
    (Opcode::NEG, Format::F0),
    // §4.4 Bitwise
    (Opcode::BAND, Format::F0),
    (Opcode::BOR, Format::F0),
    (Opcode::BXOR, Format::F0),
    (Opcode::BNOT, Format::F0),
    // §4.5 Class Dispatch
    (Opcode::CLS_DICT, Format::FI16),
    (Opcode::CLS_DISP, Format::FI8x2),
    // §4.6 Comparison
    (Opcode::CMP_EQ, Format::F0),
    (Opcode::CMP_NE, Format::F0),
    (Opcode::CMP_LT, Format::F0),
    (Opcode::CMP_GT, Format::F0),
    (Opcode::CMP_LE, Format::F0),
    (Opcode::CMP_GE, Format::F0),
    // §4.6 Branch
    (Opcode::BR, Format::FI16),
    (Opcode::BR_TRUE, Format::FI16),
    (Opcode::BR_FALSE, Format::FI16),
    (Opcode::BR_LONG, Format::FI24),
    // §4.7 Call/Return
    (Opcode::CALL, Format::FI8),
    (Opcode::CALL_TAIL, Format::FI8),
    (Opcode::RET, Format::F0),
    (Opcode::RET_UNIT, Format::F0),
    // §4.8 Closure
    (Opcode::CLS_NEW, Format::FI16),
    (Opcode::CLS_UPV, Format::FI8x2),
    // §4.9 Record
    (Opcode::REC_NEW, Format::FI8x2),
    (Opcode::REC_GET, Format::FI8),
    (Opcode::REC_SET, Format::FI8),
    (Opcode::REC_ADDR, Format::FI8),
    // §4.10 Array
    (Opcode::ARR_NEW, Format::F0),
    (Opcode::ARR_GET, Format::F0),
    (Opcode::ARR_SET, Format::F0),
    (Opcode::ARR_LEN, Format::F0),
    // §4.11 Tuple
    (Opcode::TUP_NEW, Format::FI8),
    (Opcode::TUP_GET, Format::FI8),
    // §4.12 Type
    (Opcode::TY_OF, Format::F0),
    (Opcode::TY_EQ, Format::F0),
    (Opcode::TY_TEST, Format::F0),
    (Opcode::TY_CAST, Format::F0),
    (Opcode::TY_DESC, Format::FI16),
    // §4.13 Effect
    (Opcode::EFF_NEED, Format::FI8x2),
    (Opcode::EFF_HDL, Format::FI16),
    (Opcode::EFF_RES, Format::F0),
    (Opcode::EFF_POP, Format::F0),
    // §4.14 Match
    (Opcode::MAT_TAG, Format::FI16),
    (Opcode::MAT_DATA, Format::F0),
    // §4.15 Optional
    (Opcode::OPT_SOME, Format::F0),
    (Opcode::OPT_NONE, Format::F0),
    (Opcode::OPT_GET, Format::F0),
    (Opcode::OPT_IS, Format::F0),
    // §4.16 String
    (Opcode::STR_CAT, Format::F0),
    (Opcode::STR_LEN, Format::F0),
    // §4.17 Arena
    (Opcode::AR_NEW, Format::F0),
    (Opcode::AR_ALLOC, Format::F0),
    (Opcode::AR_FREE, Format::F0),
    // §4.18 GC
    (Opcode::GC_PIN, Format::F0),
    (Opcode::GC_UNPIN, Format::F0),
    // §4.19 Foreign
    (Opcode::FFI_CALL, Format::FI8x2),
    // §4.21 Misc
    (Opcode::NOP, Format::F0),
    (Opcode::PANIC, Format::F0),
];

#[test]
fn test_opcode_count() {
    assert_eq!(
        OPCODE_FORMAT_TABLE.len(),
        81,
        "expected 81 opcodes, got {}",
        OPCODE_FORMAT_TABLE.len()
    );
}

#[test]
fn test_no_duplicate_opcode_values() {
    let values: Vec<u8> = OPCODE_FORMAT_TABLE.iter().map(|(op, _)| op.0).collect();
    let unique: HashSet<u8> = values.iter().copied().collect();
    assert_eq!(
        values.len(),
        unique.len(),
        "duplicate opcode byte values found"
    );
}

#[test]
fn test_format_matches_table() {
    for (op, expected_fmt) in OPCODE_FORMAT_TABLE {
        let actual_fmt = format(op.0);
        assert_eq!(
            actual_fmt, *expected_fmt,
            "opcode {op} (0x{:02X}): expected format {expected_fmt:?}, got {actual_fmt:?}",
            op.0
        );
    }
}

#[test]
fn test_instr_len_matches_format() {
    for (op, fmt) in OPCODE_FORMAT_TABLE {
        let expected_len = match fmt {
            Format::F0 => 1,
            Format::FI8 => 2,
            Format::FI16 | Format::FI8x2 => 3,
            Format::FI24 => 4,
        };
        let actual_len = instr_len(op.0);
        assert_eq!(
            actual_len, expected_len,
            "opcode {op} (0x{:02X}): expected len {expected_len}, got {actual_len}",
            op.0
        );
    }
}

#[test]
fn test_unassigned_opcodes_are_f0() {
    let assigned: HashSet<u8> = OPCODE_FORMAT_TABLE.iter().map(|(op, _)| op.0).collect();
    for byte in 0x51u8..=0xFFu8 {
        if !assigned.contains(&byte) {
            assert_eq!(
                format(byte),
                Format::F0,
                "unassigned opcode 0x{byte:02X} should map to F0"
            );
            assert_eq!(
                instr_len(byte),
                1,
                "unassigned opcode 0x{byte:02X} should have len 1"
            );
        }
    }
}

#[test]
fn test_display_known_opcodes() {
    assert_eq!(format!("{}", Opcode::NOP), "nop");
    assert_eq!(format!("{}", Opcode::ADD), "add");
    assert_eq!(format!("{}", Opcode::CMP_EQ), "cmp.eq");
    assert_eq!(format!("{}", Opcode::BR), "br");
    assert_eq!(format!("{}", Opcode::BR_LONG), "br.long");
    assert_eq!(format!("{}", Opcode::CALL), "call");
    assert_eq!(format!("{}", Opcode::RET), "ret");
    assert_eq!(format!("{}", Opcode::LD_CONST), "ld.const");
    assert_eq!(format!("{}", Opcode::LD_LOC), "ld.loc");
    assert_eq!(format!("{}", Opcode::FFI_CALL), "ffi.call");
    assert_eq!(format!("{}", Opcode::REC_NEW), "rec.new");
    assert_eq!(format!("{}", Opcode::EFF_NEED), "eff.need");
}

#[test]
fn test_display_unknown_opcode() {
    assert_eq!(format!("{}", Opcode(0x51)), "0x51");
    assert_eq!(format!("{}", Opcode(0xFF)), "0xFF");
}

#[test]
fn test_all_assigned_opcodes_have_names() {
    for (op, _) in OPCODE_FORMAT_TABLE {
        assert!(
            op.name().is_some(),
            "opcode {op:?} (0x{:02X}) has no name in OPCODE_NAMES",
            op.0
        );
    }
}
