use music_names::Interner;

use super::IrDataLayout;

#[test]
fn test_record_field_index() {
    let mut interner = Interner::new();
    let a = interner.intern("a");
    let b = interner.intern("b");
    let layout = IrDataLayout {
        record_fields: Some(Box::new([a, b])),
        choice_variants: None,
    };
    assert_eq!(layout.record_field_index(a), Some(0));
    assert_eq!(layout.record_field_index(b), Some(1));
}

#[test]
fn test_choice_variant_tag() {
    let mut interner = Interner::new();
    let some = interner.intern("Some");
    let none = interner.intern("None");
    let layout = IrDataLayout {
        record_fields: None,
        choice_variants: Some(Box::new([none, some])),
    };
    assert_eq!(layout.choice_variant_tag(none), Some(0));
    assert_eq!(layout.choice_variant_tag(some), Some(1));
}
