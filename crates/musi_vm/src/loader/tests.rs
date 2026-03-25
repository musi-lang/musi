#![allow(clippy::unwrap_used, clippy::panic, clippy::as_conversions)]

use music_il::opcode::Opcode;

use super::load;
use crate::error::LoadError;

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
    assert!(module.constants[0].is_int());
    assert_eq!(module.constants[0].as_int(), 42);
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
