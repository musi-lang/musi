use super::*;

#[test]
fn test_int_equality() {
    assert_eq!(Literal::Int(42), Literal::Int(42));
    assert_ne!(Literal::Int(1), Literal::Int(2));
}

#[test]
fn test_float_equality() {
    assert_eq!(Literal::Float(2.72), Literal::Float(2.72));
    assert_ne!(Literal::Float(1.0), Literal::Float(2.0));
}

#[test]
fn test_str_equality() {
    assert_eq!(
        Literal::Str(String::from("hello")),
        Literal::Str(String::from("hello"))
    );
    assert_ne!(
        Literal::Str(String::from("a")),
        Literal::Str(String::from("b"))
    );
}

#[test]
fn test_rune_equality() {
    assert_eq!(Literal::Rune('x'), Literal::Rune('x'));
    assert_ne!(Literal::Rune('a'), Literal::Rune('b'));
}

#[test]
fn test_different_variants_not_equal() {
    assert_ne!(Literal::Int(1), Literal::Float(1.0));
    assert_ne!(Literal::Str(String::from("1")), Literal::Int(1));
}

#[test]
fn test_clone_produces_equal_value() {
    let original = Literal::Str(String::from("test"));
    let cloned = original.clone();
    assert_eq!(original, cloned);
}

#[test]
fn test_display_format() {
    assert_eq!(format!("{}", Literal::Int(42)), "42");
    assert_eq!(format!("{}", Literal::Int(-7)), "-7");
    assert_eq!(format!("{}", Literal::Float(2.72)), "2.72");
    assert_eq!(
        format!("{}", Literal::Str(String::from("hello"))),
        "\"hello\""
    );
    assert_eq!(format!("{}", Literal::Rune('a')), "'a'");
}

#[test]
fn test_nan_float_not_equal_to_itself() {
    let nan = Literal::Float(f64::NAN);
    assert_ne!(nan, nan);
}
