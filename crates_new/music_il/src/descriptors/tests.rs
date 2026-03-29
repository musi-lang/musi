#[test]
fn test_nan_box_tags_fit_in_three_bits() {
    for tag in [
        super::NAN_BOX_PTR,
        super::NAN_BOX_SMI,
        super::NAN_BOX_BOOL,
        super::NAN_BOX_UNIT,
        super::NAN_BOX_TAG,
        super::NAN_BOX_CHAR,
    ] {
        assert!(tag < 8);
    }
}

#[test]
fn test_builtin_type_ids_stay_reserved() {
    for id in [
        super::BUILTIN_TYPE_TYPE,
        super::BUILTIN_TYPE_ANY,
        super::BUILTIN_TYPE_UNKNOWN,
        super::BUILTIN_TYPE_NEVER,
        super::BUILTIN_TYPE_UNIT,
        super::BUILTIN_TYPE_BOOL,
        super::BUILTIN_TYPE_INT,
        super::BUILTIN_TYPE_FLOAT,
        super::BUILTIN_TYPE_STRING,
    ] {
        assert!(id >= 0xFFF0);
    }
}
