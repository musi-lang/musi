use super::Symbol;

#[test]
fn raw_round_trip() {
    let sym = Symbol::from_raw(42);
    assert_eq!(sym.raw(), 42);
}

#[test]
fn ordering_is_by_raw_value() {
    assert!(Symbol::from_raw(1) < Symbol::from_raw(2));
}

#[test]
fn debug_and_display_are_stable() {
    let sym = Symbol::from_raw(7);
    assert_eq!(format!("{sym:?}"), "Symbol(7)");
    assert_eq!(format!("{sym}"), "7");
}
