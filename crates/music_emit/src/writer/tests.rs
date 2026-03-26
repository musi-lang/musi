use music_il::format;

use crate::emitter::SeamModule;
use crate::pool::{ConstantEntry, ConstantPool};
use crate::writer::write_seam;

fn empty_module() -> SeamModule {
    SeamModule {
        constants: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
    }
}

#[test]
fn header_starts_with_magic() {
    let module = empty_module();
    let bytes = write_seam(&module);
    assert!(bytes.len() >= format::HEADER_SIZE);
    assert_eq!(&bytes[0..4], &format::MAGIC);
}

#[test]
fn header_version() {
    let module = empty_module();
    let bytes = write_seam(&module);
    assert_eq!(bytes[4], format::VERSION_MAJOR);
    assert_eq!(bytes[5], format::VERSION_MINOR);
}

#[test]
fn header_section_count_zero_for_empty() {
    let module = empty_module();
    let bytes = write_seam(&module);
    let section_count = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    assert_eq!(section_count, 0);
}

#[test]
fn header_total_size_matches_buffer() {
    let module = empty_module();
    let bytes = write_seam(&module);
    let total_size = u32::from_le_bytes([bytes[12], bytes[13], bytes[14], bytes[15]]);
    assert_eq!(usize::try_from(total_size).unwrap(), bytes.len());
}

#[test]
fn constant_pool_section_present_when_nonempty() {
    let mut pool = ConstantPool::new();
    let _idx = pool.add(ConstantEntry::Int(42));

    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
    };
    let bytes = write_seam(&module);

    let section_count = u32::from_le_bytes([bytes[8], bytes[9], bytes[10], bytes[11]]);
    assert!(section_count >= 1);

    let cnst_tag = &format::section::CNST;
    let found = bytes
        .windows(4)
        .skip(format::HEADER_SIZE)
        .any(|w| w == cnst_tag);
    assert!(found, "CNST section tag not found in output");
}

#[test]
fn roundtrip_string_in_string_table() {
    let mut pool = ConstantPool::new();
    let _idx = pool.add(ConstantEntry::Str(String::from("hello")));

    let module = SeamModule {
        constants: pool,
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
    };
    let bytes = write_seam(&module);

    let strt_tag = &format::section::STRT;
    let tag_pos = bytes
        .windows(4)
        .position(|w| w == strt_tag)
        .expect("STRT section not found");
    let data_start = tag_pos + 4 + 4;
    let expected = b"hello\0";
    assert_eq!(&bytes[data_start..data_start + expected.len()], expected);
}
