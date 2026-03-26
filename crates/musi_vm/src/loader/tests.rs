#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use music_il::opcode::Opcode;

use super::load;
use crate::errors::LoadError;
use crate::module::ConstantEntry;

const fn op(o: Opcode) -> u8 {
    o as u8
}

fn minimal_seam(instr_bytes: &[u8], instr_count: u16) -> Vec<u8> {
    let mut content = Vec::new();
    content.extend_from_slice(&1u16.to_le_bytes());
    content.extend_from_slice(&u32::MAX.to_le_bytes());
    content.extend_from_slice(&0u16.to_le_bytes());
    content.extend_from_slice(&instr_count.to_le_bytes());
    content.extend_from_slice(instr_bytes);

    let content_len = u32::try_from(content.len()).unwrap();
    let total_size = u32::try_from(16 + 8 + content.len()).unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&1u32.to_le_bytes());
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());
    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&content_len.to_le_bytes());
    buf.extend_from_slice(&content);

    buf
}

/// Build a .seam binary with STRT, CNST, and METH sections.
fn seam_with_strt_cnst(strt_data: &[u8], cnst_content: &[u8]) -> Vec<u8> {
    let meth_content: Vec<u8> = {
        let mut v = Vec::new();
        v.extend_from_slice(&1u16.to_le_bytes());
        v.extend_from_slice(&u32::MAX.to_le_bytes());
        v.extend_from_slice(&0u16.to_le_bytes());
        v.extend_from_slice(&1u16.to_le_bytes());
        v.push(op(Opcode::Halt));
        v
    };

    let strt_len = u32::try_from(strt_data.len()).unwrap();
    let cnst_len = u32::try_from(cnst_content.len()).unwrap();
    let meth_len = u32::try_from(meth_content.len()).unwrap();
    let total_size =
        u32::try_from(16 + 8 + strt_data.len() + 8 + cnst_content.len() + 8 + meth_content.len())
            .unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&3u32.to_le_bytes());
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());

    buf.extend_from_slice(b"STRT");
    buf.extend_from_slice(&strt_len.to_le_bytes());
    buf.extend_from_slice(strt_data);

    buf.extend_from_slice(b"CNST");
    buf.extend_from_slice(&cnst_len.to_le_bytes());
    buf.extend_from_slice(cnst_content);

    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&meth_len.to_le_bytes());
    buf.extend_from_slice(&meth_content);

    buf
}

#[test]
fn load_halt_only() {
    let seam = minimal_seam(&[op(Opcode::Halt)], 1);
    let module = load(&seam).unwrap();
    assert_eq!(module.methods.len(), 1);
    assert_eq!(module.methods[0].name, u32::MAX);
    assert_eq!(module.methods[0].locals_count, 0);
    assert_eq!(module.methods[0].code, &[op(Opcode::Halt)]);
}

#[test]
fn load_ldsmi_halt() {
    let seam = minimal_seam(&[op(Opcode::LdSmi), 42, 0, op(Opcode::Halt)], 2);
    let module = load(&seam).unwrap();
    assert_eq!(
        module.methods[0].code,
        &[op(Opcode::LdSmi), 42, 0, op(Opcode::Halt)]
    );
}

#[test]
fn load_constants_int() {
    let mut cnst_content = Vec::new();
    cnst_content.extend_from_slice(&1u16.to_le_bytes());
    cnst_content.push(0x01); // Int tag
    cnst_content.extend_from_slice(&42i64.to_le_bytes());

    let cnst_len = u32::try_from(cnst_content.len()).unwrap();

    let meth_content: Vec<u8> = {
        let mut v = Vec::new();
        v.extend_from_slice(&1u16.to_le_bytes());
        v.extend_from_slice(&u32::MAX.to_le_bytes());
        v.extend_from_slice(&0u16.to_le_bytes());
        v.extend_from_slice(&1u16.to_le_bytes());
        v.push(op(Opcode::Halt));
        v
    };
    let meth_len = u32::try_from(meth_content.len()).unwrap();
    let total_size = u32::try_from(16 + 8 + cnst_content.len() + 8 + meth_content.len()).unwrap();

    let mut buf = vec![0u8; 16];
    buf[0..4].copy_from_slice(b"SEAM");
    buf[4] = 0;
    buf[5] = 1;
    buf[8..12].copy_from_slice(&2u32.to_le_bytes());
    buf[12..16].copy_from_slice(&total_size.to_le_bytes());
    buf.extend_from_slice(b"CNST");
    buf.extend_from_slice(&cnst_len.to_le_bytes());
    buf.extend_from_slice(&cnst_content);
    buf.extend_from_slice(b"METH");
    buf.extend_from_slice(&meth_len.to_le_bytes());
    buf.extend_from_slice(&meth_content);

    let module = load(&buf).unwrap();
    assert_eq!(module.constants.len(), 1);
    let ConstantEntry::Value(v) = &module.constants[0] else {
        panic!("expected Value");
    };
    assert!(v.is_int());
    assert_eq!(v.as_int(), 42);
}

#[test]
fn load_constants_str_as_string_ref() {
    // STRT: "hello\0" (6 bytes), CNST: 1 entry, tag=0x03, offset=0
    let strt_data = b"hello\0";
    let mut cnst_content = Vec::new();
    cnst_content.extend_from_slice(&1u16.to_le_bytes());
    cnst_content.push(0x03);
    cnst_content.extend_from_slice(&0u16.to_le_bytes()); // byte offset 0

    let buf = seam_with_strt_cnst(strt_data, &cnst_content);
    let module = load(&buf).unwrap();

    assert_eq!(module.constants.len(), 1);
    assert!(
        matches!(module.constants[0], ConstantEntry::StringRef(0)),
        "expected StringRef(0), got {:?}",
        module.constants[0]
    );
    assert_eq!(module.strings[0], "hello");
}

#[test]
fn load_two_strings_correct_indices() {
    // STRT: "hello\0world\0" → strings[0]="hello" at offset 0, strings[1]="world" at offset 6
    let strt_data = b"hello\0world\0";
    let mut cnst_content = Vec::new();
    cnst_content.extend_from_slice(&2u16.to_le_bytes());
    // First string: byte offset 0 → index 0
    cnst_content.push(0x03);
    cnst_content.extend_from_slice(&0u16.to_le_bytes());
    // Second string: byte offset 6 → index 1
    cnst_content.push(0x03);
    cnst_content.extend_from_slice(&6u16.to_le_bytes());

    let buf = seam_with_strt_cnst(strt_data, &cnst_content);
    let module = load(&buf).unwrap();

    assert_eq!(module.constants.len(), 2);
    assert!(matches!(module.constants[0], ConstantEntry::StringRef(0)));
    assert!(matches!(module.constants[1], ConstantEntry::StringRef(1)));
    assert_eq!(module.strings[0], "hello");
    assert_eq!(module.strings[1], "world");
}

#[test]
fn bad_magic() {
    let mut data = vec![0u8; 16];
    data[0..4].copy_from_slice(b"BADM");
    assert!(matches!(load(&data), Err(LoadError::InvalidMagic)));
}

#[test]
fn truncated_header() {
    assert!(matches!(
        load(&[0x53, 0x45]),
        Err(LoadError::TruncatedHeader)
    ));
}

#[test]
fn wrong_version() {
    let mut data = vec![0u8; 16];
    data[0..4].copy_from_slice(b"SEAM");
    data[4] = 9;
    data[5] = 9;
    data[8..12].copy_from_slice(&0u32.to_le_bytes());
    data[12..16].copy_from_slice(&16u32.to_le_bytes());
    assert!(matches!(
        load(&data),
        Err(LoadError::UnsupportedVersion { major: 9, minor: 9 })
    ));
}

#[test]
fn invalid_opcode_in_method() {
    let seam = minimal_seam(&[0xFF], 1);
    assert!(matches!(
        load(&seam),
        Err(LoadError::InvalidOpcode { byte: 0xFF, .. })
    ));
}

#[test]
fn arrtag_tytag_have_no_operand() {
    let bytes = [op(Opcode::ArrTag), op(Opcode::TyTag), op(Opcode::Halt)];
    let seam = minimal_seam(&bytes, 3);
    let module = load(&seam).unwrap();
    assert_eq!(module.methods[0].code, &bytes);
}

#[test]
fn arrgeti_arrseti_are_one_byte_operand() {
    let bytes = [
        op(Opcode::ArrGetI),
        5,
        op(Opcode::ArrSetI),
        3,
        op(Opcode::Halt),
    ];
    let seam = minimal_seam(&bytes, 3);
    let module = load(&seam).unwrap();
    assert_eq!(module.methods[0].code, &bytes);
}

#[test]
fn tychk_tycast_have_u16_operand() {
    let bytes = [
        op(Opcode::TyChk),
        0xF6,
        0xFF, // type_id = 0xFFF6 (Int)
        op(Opcode::TyCast),
        0xF5,
        0xFF, // type_id = 0xFFF5 (Bool)
        op(Opcode::Halt),
    ];
    let seam = minimal_seam(&bytes, 3);
    let module = load(&seam).unwrap();
    assert_eq!(module.methods[0].code, &bytes);
}
