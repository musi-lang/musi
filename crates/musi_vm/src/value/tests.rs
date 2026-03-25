#![allow(clippy::unwrap_used, clippy::panic)]

use super::Value;

#[test]
fn int_roundtrip_positive() {
    let v = Value::from_int(42);
    assert!(v.is_int());
    assert!(!v.is_float());
    assert!(!v.is_bool());
    assert!(!v.is_unit());
    assert_eq!(v.as_int(), 42);
}

#[test]
fn int_roundtrip_negative() {
    let v = Value::from_int(-1);
    assert!(v.is_int());
    assert_eq!(v.as_int(), -1);
}

#[test]
fn int_roundtrip_zero() {
    let v = Value::from_int(0);
    assert!(v.is_int());
    assert_eq!(v.as_int(), 0);
    assert_eq!(v, Value::ZERO);
}

#[test]
fn int_roundtrip_one() {
    let v = Value::from_int(1);
    assert_eq!(v, Value::ONE);
    assert_eq!(v.as_int(), 1);
}

#[test]
fn int_max_48bit() {
    // 2^47 - 1 is the largest positive 48-bit signed integer
    let max = (1i64 << 47).wrapping_sub(1);
    let v = Value::from_int(max);
    assert!(v.is_int());
    assert_eq!(v.as_int(), max);
}

#[test]
fn int_min_48bit() {
    // -(2^47) is the most negative 48-bit signed integer
    let min = -(1i64 << 47);
    let v = Value::from_int(min);
    assert!(v.is_int());
    assert_eq!(v.as_int(), min);
}

#[test]
fn float_roundtrip_zero() {
    let v = Value::from_float(0.0);
    assert!(v.is_float());
    assert!(!v.is_int());
    assert_eq!(v.as_float().to_bits(), 0.0_f64.to_bits());
}

#[test]
fn float_roundtrip_negative() {
    let v = Value::from_float(-1.5);
    assert!(v.is_float());
    assert_eq!(v.as_float().to_bits(), (-1.5_f64).to_bits());
}

#[test]
fn float_roundtrip_infinity() {
    let v = Value::from_float(f64::INFINITY);
    assert!(v.is_float());
    assert!(v.as_float().is_infinite());
}

#[test]
fn bool_true() {
    assert!(Value::TRUE.is_bool());
    assert!(Value::TRUE.as_bool());
    assert!(!Value::TRUE.is_int());
}

#[test]
fn bool_false() {
    assert!(Value::FALSE.is_bool());
    assert!(!Value::FALSE.as_bool());
}

#[test]
fn bool_from() {
    assert_eq!(Value::from_bool(true), Value::TRUE);
    assert_eq!(Value::from_bool(false), Value::FALSE);
}

#[test]
fn unit() {
    assert!(Value::UNIT.is_unit());
    assert!(!Value::UNIT.is_int());
    assert!(!Value::UNIT.is_bool());
    assert!(!Value::UNIT.is_float());
}

#[test]
fn ptr_roundtrip_zero() {
    // idx=0 exercises NAN_BOX_PTR tag (0b000) - payload all zeros, most collision-prone
    let v = Value::from_ptr(0);
    assert!(v.is_ptr());
    assert!(!v.is_int());
    assert!(!v.is_bool());
    assert!(!v.is_float());
    assert!(!v.is_unit());
    assert_eq!(v.as_ptr_idx(), 0);
}

#[test]
fn ptr_roundtrip_nonzero() {
    let v = Value::from_ptr(42);
    assert!(v.is_ptr());
    assert_eq!(v.as_ptr_idx(), 42);
}

#[test]
fn ptr_roundtrip_max_payload() {
    // 48-bit max index
    let max_idx: usize = (1 << 48) - 1;
    let v = Value::from_ptr(max_idx);
    assert!(v.is_ptr());
    assert_eq!(v.as_ptr_idx(), max_idx);
}

#[test]
fn tag_roundtrip() {
    let v = Value::from_tag(42);
    assert!(v.is_tag());
    assert!(!v.is_int());
    assert!(!v.is_bool());
    assert!(!v.is_float());
    assert!(!v.is_unit());
    assert!(!v.is_ptr());
    assert_eq!(v.as_tag_idx(), 42);
}

#[test]
fn tag_zero() {
    let v = Value::from_tag(0);
    assert!(v.is_tag());
    assert_eq!(v.as_tag_idx(), 0);
}

#[test]
fn tag_max() {
    let v = Value::from_tag(u16::MAX);
    assert!(v.is_tag());
    assert_eq!(v.as_tag_idx(), u16::MAX);
}

#[test]
fn tag_equality() {
    // Two values with the same index are bit-identical — CmpEq works at u64 level
    assert_eq!(Value::from_tag(7), Value::from_tag(7));
    assert_ne!(Value::from_tag(7), Value::from_tag(8));
}

#[test]
#[cfg(debug_assertions)]
#[should_panic]
fn from_int_debug_panic_overflow() {
    let _ = Value::from_int(1i64 << 47);
}

#[test]
#[cfg(debug_assertions)]
#[should_panic]
fn from_int_debug_panic_underflow() {
    let _ = Value::from_int(-(1i64 << 47) - 1);
}

#[test]
#[cfg(debug_assertions)]
#[should_panic]
fn as_tag_idx_debug_panic() {
    // from_ptr with payload > u16::MAX; calling as_tag_idx on it triggers the guard
    let v = Value::from_ptr(0x1_0000);
    let _ = v.as_tag_idx();
}
