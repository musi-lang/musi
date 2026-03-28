use super::*;

#[test]
fn prelude_classes_count() {
    assert_eq!(PRELUDE_CLASSES.len(), 5);
}

#[test]
fn eq_class_has_two_methods() {
    let eq = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Eq")
        .expect("Eq class missing");
    assert_eq!(eq.methods.len(), 2);
}

#[test]
fn ord_class_has_four_methods() {
    let ord = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Ord")
        .expect("Ord class missing");
    assert_eq!(ord.methods.len(), 4);
}

#[test]
fn num_class_has_six_methods() {
    let num = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Num")
        .expect("Num class missing");
    assert_eq!(num.methods.len(), 6);
}

#[test]
fn bits_class_has_two_methods() {
    let bits = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Bits")
        .expect("Bits class missing");
    assert_eq!(bits.methods.len(), 2);
}

#[test]
fn show_class_has_no_methods() {
    let show = PRELUDE_CLASSES
        .iter()
        .find(|c| c.name == "Show")
        .expect("Show class missing");
    assert_eq!(show.methods.len(), 0);
}

#[test]
fn all_methods_have_nonempty_fields() {
    for class in PRELUDE_CLASSES {
        assert!(!class.name.is_empty(), "class name is empty");
        for method in class.methods {
            assert!(
                !method.name.is_empty(),
                "method name is empty in {}",
                class.name
            );
            assert!(
                !method.op_name.is_empty(),
                "op_name is empty for {}.{}",
                class.name,
                method.name
            );
        }
    }
}

#[test]
fn prelude_source_exports_builtin_surface() {
    assert!(
        PRELUDE_SOURCE.contains("export let Bool"),
        "expected builtin Bool declaration in prelude source"
    );
    assert!(
        PRELUDE_SOURCE.contains("export let Eq [T] := class"),
        "expected Eq class declaration in prelude source"
    );
    assert!(
        PRELUDE_SOURCE.contains("export opaque let CString"),
        "expected builtin CString declaration in prelude source"
    );
    assert!(
        PRELUDE_SOURCE.contains("export opaque let CPtr"),
        "expected builtin CPtr declaration in prelude source"
    );
}
