use super::*;

#[test]
fn test_new() {
    let pos = SourcePosition::new(42);
    assert_eq!(pos.position, 42);
}

#[test]
fn test_new_zero() {
    let pos = SourcePosition::new(0);
    assert_eq!(pos.position, 0);
}

#[test]
fn test_new_max() {
    let pos = SourcePosition::new(u32::MAX);
    assert_eq!(pos.position, u32::MAX);
}

#[test]
fn test_copy_semantics() {
    let pos1 = SourcePosition::new(100);
    let pos2 = pos1;
    assert_eq!(pos1.position, pos2.position);
}

#[test]
fn test_equality() {
    assert_eq!(SourcePosition::new(42), SourcePosition::new(42));
    assert_ne!(SourcePosition::new(42), SourcePosition::new(43));
}
