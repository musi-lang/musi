use super::*;

#[test]
fn magic_bytes_are_seam() {
    assert_eq!(&MAGIC, b"SEAM");
}

#[test]
fn header_size_is_16() {
    assert_eq!(HEADER_SIZE, 16);
}

#[test]
fn version_is_0_1() {
    assert_eq!(VERSION_MAJOR, 0);
    assert_eq!(VERSION_MINOR, 1);
}

#[test]
fn section_tags_are_four_bytes() {
    assert_eq!(section::STRT.len(), 4);
    assert_eq!(section::TYPE.len(), 4);
    assert_eq!(section::CNST.len(), 4);
    assert_eq!(section::DEPS.len(), 4);
    assert_eq!(section::GLOB.len(), 4);
    assert_eq!(section::METH.len(), 4);
    assert_eq!(section::EFCT.len(), 4);
    assert_eq!(section::CLSS.len(), 4);
    assert_eq!(section::FRGN.len(), 4);
    assert_eq!(section::DBUG.len(), 4);
}

#[test]
fn section_tags_are_distinct() {
    let tags: Vec<[u8; 4]> = vec![
        section::STRT,
        section::TYPE,
        section::CNST,
        section::DEPS,
        section::GLOB,
        section::METH,
        section::EFCT,
        section::CLSS,
        section::FRGN,
        section::DBUG,
    ];
    for (i, a) in tags.iter().enumerate() {
        for b in &tags[i + 1..] {
            assert_ne!(a, b, "duplicate section tag");
        }
    }
}

#[test]
fn nan_box_tags_fit_in_3_bits() {
    const { assert!(NAN_BOX_PTR < 8) };
    const { assert!(NAN_BOX_SMI < 8) };
    const { assert!(NAN_BOX_BOOL < 8) };
    const { assert!(NAN_BOX_UNIT < 8) };
    const { assert!(NAN_BOX_TAG < 8) };
    const { assert!(NAN_BOX_CHAR < 8) };
}

#[test]
fn builtin_type_ids_are_in_reserved_range() {
    let ids = [
        BUILTIN_TYPE_TYPE,
        BUILTIN_TYPE_ANY,
        BUILTIN_TYPE_UNKNOWN,
        BUILTIN_TYPE_NEVER,
        BUILTIN_TYPE_UNIT,
        BUILTIN_TYPE_BOOL,
        BUILTIN_TYPE_INT,
        BUILTIN_TYPE_FLOAT,
        BUILTIN_TYPE_STRING,
    ];
    for id in ids {
        assert!(
            id >= 0xFFF0,
            "builtin type ID {id:#06x} below reserved range"
        );
    }
}
