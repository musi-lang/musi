use super::*;

#[test]
fn test_nan_box_tags_fit_in_three_bits() {
    for tag in [
        NAN_BOX_PTR,
        NAN_BOX_SMI,
        NAN_BOX_BOOL,
        NAN_BOX_UNIT,
        NAN_BOX_TAG,
        NAN_BOX_CHAR,
    ] {
        assert!(tag < 8);
    }
}

#[test]
fn test_builtin_type_ids_stay_reserved() {
    for id in [
        BUILTIN_TYPE_TYPE,
        BUILTIN_TYPE_ANY,
        BUILTIN_TYPE_UNKNOWN,
        BUILTIN_TYPE_NEVER,
        BUILTIN_TYPE_UNIT,
        BUILTIN_TYPE_BOOL,
        BUILTIN_TYPE_INT,
        BUILTIN_TYPE_FLOAT,
        BUILTIN_TYPE_STRING,
    ] {
        assert!(id >= 0xFFF0);
    }
}
