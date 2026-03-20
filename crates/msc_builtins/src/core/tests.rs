use std::f64::consts;

use msc_vm::{Heap, Value};

use crate::core::*;

fn make_string(heap: &mut Heap, s: &str) -> Value {
    let ptr = heap.alloc_string(0, Box::from(s));
    Value::from_ref(ptr)
}

fn make_array(heap: &mut Heap, elems: Vec<Value>) -> Value {
    let ptr = heap.alloc_array(0, elems);
    Value::from_ref(ptr)
}

fn extract_str(val: Value, heap: &Heap) -> &str {
    let ptr = val.as_ref().unwrap();
    heap.get_string(ptr).unwrap()
}

fn extract_elems(val: Value, heap: &Heap) -> Vec<Value> {
    let ptr = val.as_ref().unwrap();
    heap.get_array(ptr).unwrap().to_vec()
}

#[test]
fn test_str_len() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello");
    let result = str_len(&[s], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 5);
}

#[test]
fn test_str_len_unicode() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "héllo");
    let result = str_len(&[s], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 5);
}

#[test]
fn test_str_byte_len() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "héllo");
    let result = str_byte_len(&[s], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 6); // é is 2 bytes
}

#[test]
fn test_str_at() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "abc");
    let idx = Value::from_int(1);
    let result = str_at(&[s, idx], &mut heap).unwrap();
    assert_eq!(get_rune(result).unwrap(), 'b');
}

#[test]
fn test_str_slice() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let result = str_slice(&[s, Value::from_int(0), Value::from_int(5)], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hello");
}

#[test]
fn test_str_contains() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let needle = make_string(&mut heap, "world");
    let result = str_contains(&[s, needle], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_str_contains_false() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello");
    let needle = make_string(&mut heap, "xyz");
    let result = str_contains(&[s, needle], &mut heap).unwrap();
    assert!(!result.as_bool().unwrap());
}

#[test]
fn test_str_starts_with() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let prefix = make_string(&mut heap, "hello");
    let result = str_starts_with(&[s, prefix], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_str_ends_with() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let suffix = make_string(&mut heap, "world");
    let result = str_ends_with(&[s, suffix], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_str_index_of() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let needle = make_string(&mut heap, "world");
    let result = str_index_of(&[s, needle], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 6);
}

#[test]
fn test_str_index_of_not_found() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello");
    let needle = make_string(&mut heap, "xyz");
    let result = str_index_of(&[s, needle], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), -1);
}

#[test]
fn test_str_to_upper() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello");
    let result = str_to_upper(&[s], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "HELLO");
}

#[test]
fn test_str_to_lower() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "HELLO");
    let result = str_to_lower(&[s], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hello");
}

#[test]
fn test_str_trim() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "  hello  ");
    let result = str_trim(&[s], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hello");
}

#[test]
fn test_str_trim_start() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "  hello  ");
    let result = str_trim_start(&[s], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hello  ");
}

#[test]
fn test_str_trim_end() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "  hello  ");
    let result = str_trim_end(&[s], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "  hello");
}

#[test]
fn test_str_split() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "a,b,c");
    let delim = make_string(&mut heap, ",");
    let result = str_split(&[s, delim], &mut heap).unwrap();
    let elems = extract_elems(result, &heap);
    assert_eq!(elems.len(), 3);
    assert_eq!(extract_str(elems[0], &heap), "a");
    assert_eq!(extract_str(elems[1], &heap), "b");
    assert_eq!(extract_str(elems[2], &heap), "c");
}

#[test]
fn test_str_join() {
    let mut heap = Heap::new();
    let a = make_string(&mut heap, "a");
    let b = make_string(&mut heap, "b");
    let c = make_string(&mut heap, "c");
    let arr = make_array(&mut heap, vec![a, b, c]);
    let sep = make_string(&mut heap, ",");
    let result = str_join(&[arr, sep], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "a,b,c");
}

#[test]
fn test_str_join_empty() {
    let mut heap = Heap::new();
    let arr = make_array(&mut heap, vec![]);
    let sep = make_string(&mut heap, ",");
    let result = str_join(&[arr, sep], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "");
}

#[test]
fn test_str_replace() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "hello world");
    let from = make_string(&mut heap, "world");
    let to = make_string(&mut heap, "rust");
    let result = str_replace(&[s, from, to], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hello rust");
}

#[test]
fn test_str_repeat() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "ab");
    let result = str_repeat(&[s, Value::from_int(3)], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "ababab");
}

#[test]
fn test_str_chars() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "abc");
    let result = str_chars(&[s], &mut heap).unwrap();
    let elems = extract_elems(result, &heap);
    assert_eq!(elems.len(), 3);
    assert_eq!(get_rune(elems[0]).unwrap(), 'a');
    assert_eq!(get_rune(elems[1]).unwrap(), 'b');
    assert_eq!(get_rune(elems[2]).unwrap(), 'c');
}

#[test]
fn test_str_from_chars() {
    let mut heap = Heap::new();
    let runes = vec![Value::from_rune('h'), Value::from_rune('i')];
    let arr = make_array(&mut heap, runes);
    let result = str_from_chars(&[arr], &mut heap).unwrap();
    assert_eq!(extract_str(result, &heap), "hi");
}

#[test]
fn test_str_parse_int() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "42");
    let result = str_parse_int(&[s], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 42);
}

#[test]
fn test_str_parse_int_invalid() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "abc");
    let result = str_parse_int(&[s], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 0);
}

#[test]
#[expect(
    clippy::approx_constant,
    reason = "intentionally testing near-pi values"
)]
fn test_str_parse_float() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "3.14");
    let result = str_parse_float(&[s], &mut heap).unwrap();
    let f = result.as_float().unwrap();
    assert!((f - 3.14_f64).abs() < 1e-10);
}

#[test]
fn test_str_parse_float_invalid() {
    let mut heap = Heap::new();
    let s = make_string(&mut heap, "abc");
    let result = str_parse_float(&[s], &mut heap).unwrap();
    assert!(result.as_float().unwrap().is_nan());
}

#[test]
fn test_arr_len() {
    let mut heap = Heap::new();
    let arr = make_array(&mut heap, vec![Value::from_int(1), Value::from_int(2)]);
    let result = arr_len(&[arr], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 2);
}

#[test]
fn test_arr_push_pop() {
    let mut heap = Heap::new();
    let arr = make_array(&mut heap, vec![Value::from_int(10)]);
    let _ = arr_push(&[arr, Value::from_int(20)], &mut heap).unwrap();
    let len = arr_len(&[arr], &mut heap).unwrap();
    assert_eq!(len.as_int().unwrap(), 2);

    let popped = arr_pop(&[arr], &mut heap).unwrap();
    assert_eq!(popped.as_int().unwrap(), 20);
    let len = arr_len(&[arr], &mut heap).unwrap();
    assert_eq!(len.as_int().unwrap(), 1);
}

#[test]
fn test_arr_slice() {
    let mut heap = Heap::new();
    let arr = make_array(
        &mut heap,
        vec![
            Value::from_int(1),
            Value::from_int(2),
            Value::from_int(3),
            Value::from_int(4),
        ],
    );
    let result = arr_slice(&[arr, Value::from_int(1), Value::from_int(3)], &mut heap).unwrap();
    let elems = extract_elems(result, &heap);
    assert_eq!(elems.len(), 2);
    assert_eq!(elems[0].as_int().unwrap(), 2);
    assert_eq!(elems[1].as_int().unwrap(), 3);
}

#[test]
fn test_arr_concat() {
    let mut heap = Heap::new();
    let a = make_array(&mut heap, vec![Value::from_int(1), Value::from_int(2)]);
    let b = make_array(&mut heap, vec![Value::from_int(3)]);
    let result = arr_concat(&[a, b], &mut heap).unwrap();
    let elems = extract_elems(result, &heap);
    assert_eq!(elems.len(), 3);
    assert_eq!(elems[2].as_int().unwrap(), 3);
}

#[test]
fn test_arr_reverse() {
    let mut heap = Heap::new();
    let arr = make_array(
        &mut heap,
        vec![Value::from_int(1), Value::from_int(2), Value::from_int(3)],
    );
    let _ = arr_reverse(&[arr], &mut heap).unwrap();
    let elems = extract_elems(arr, &heap);
    assert_eq!(elems[0].as_int().unwrap(), 3);
    assert_eq!(elems[1].as_int().unwrap(), 2);
    assert_eq!(elems[2].as_int().unwrap(), 1);
}

#[test]
fn test_arr_contains() {
    let mut heap = Heap::new();
    let arr = make_array(&mut heap, vec![Value::from_int(10), Value::from_int(20)]);
    let found = arr_contains(&[arr, Value::from_int(10)], &mut heap).unwrap();
    assert!(found.as_bool().unwrap());
    let not_found = arr_contains(&[arr, Value::from_int(99)], &mut heap).unwrap();
    assert!(!not_found.as_bool().unwrap());
}

#[test]
fn test_arr_index_of() {
    let mut heap = Heap::new();
    let arr = make_array(
        &mut heap,
        vec![
            Value::from_int(10),
            Value::from_int(20),
            Value::from_int(30),
        ],
    );
    let idx = arr_index_of(&[arr, Value::from_int(20)], &mut heap).unwrap();
    assert_eq!(idx.as_int().unwrap(), 1);
    let not_found = arr_index_of(&[arr, Value::from_int(99)], &mut heap).unwrap();
    assert_eq!(not_found.as_int().unwrap(), -1);
}

#[test]
fn test_arr_sort() {
    let mut heap = Heap::new();
    let arr = make_array(
        &mut heap,
        vec![Value::from_int(3), Value::from_int(1), Value::from_int(2)],
    );
    let _ = arr_sort(&[arr], &mut heap).unwrap();
    let elems = extract_elems(arr, &heap);
    assert_eq!(elems[0].as_int().unwrap(), 1);
    assert_eq!(elems[1].as_int().unwrap(), 2);
    assert_eq!(elems[2].as_int().unwrap(), 3);
}

#[test]
fn test_int_abs() {
    let mut heap = Heap::new();
    let result = int_abs(&[Value::from_int(-42)], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 42);
}

#[test]
fn test_int_min_max() {
    let mut heap = Heap::new();
    let min = int_min(&[Value::from_int(3), Value::from_int(7)], &mut heap).unwrap();
    assert_eq!(min.as_int().unwrap(), 3);
    let max = int_max(&[Value::from_int(3), Value::from_int(7)], &mut heap).unwrap();
    assert_eq!(max.as_int().unwrap(), 7);
}

#[test]
fn test_int_clamp() {
    let mut heap = Heap::new();
    let result = int_clamp(
        &[Value::from_int(15), Value::from_int(0), Value::from_int(10)],
        &mut heap,
    )
    .unwrap();
    assert_eq!(result.as_int().unwrap(), 10);
}

#[test]
fn test_int_pow() {
    let mut heap = Heap::new();
    let result = int_pow(&[Value::from_int(2), Value::from_int(10)], &mut heap).unwrap();
    assert_eq!(result.as_int().unwrap(), 1024);
}

#[test]
fn test_float_sqrt() {
    let mut heap = Heap::new();
    let result = float_sqrt(&[Value::from_float(4.0)], &mut heap).unwrap();
    assert!((result.as_float().unwrap() - 2.0).abs() < 1e-10);
}

#[test]
fn test_float_trig() {
    let mut heap = Heap::new();
    let result = float_sin(&[Value::from_float(0.0)], &mut heap).unwrap();
    assert!((result.as_float().unwrap()).abs() < 1e-10);

    let result = float_cos(&[Value::from_float(0.0)], &mut heap).unwrap();
    assert!((result.as_float().unwrap() - 1.0).abs() < 1e-10);
}

#[test]
fn test_float_is_nan() {
    let mut heap = Heap::new();
    let result = float_is_nan(&[Value::from_float(f64::NAN)], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
    let result = float_is_nan(&[Value::from_float(1.0)], &mut heap).unwrap();
    assert!(!result.as_bool().unwrap());
}

#[test]
fn test_float_is_infinite() {
    let mut heap = Heap::new();
    let result = float_is_infinite(&[Value::from_float(f64::INFINITY)], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
}

#[test]
fn test_float_constants() {
    let mut heap = Heap::new();
    let pi = float_pi(&[], &mut heap).unwrap().as_float().unwrap();
    assert!((pi - consts::PI).abs() < 1e-15);
    let e = float_e(&[], &mut heap).unwrap().as_float().unwrap();
    assert!((e - consts::E).abs() < 1e-15);
}

#[test]
fn test_int_bounds() {
    let mut heap = Heap::new();
    let min_val = int_min_val(&[], &mut heap).unwrap();
    let max_val = int_max_val(&[], &mut heap).unwrap();
    assert_eq!(min_val.as_int_wide(&heap).unwrap(), i64::MIN);
    assert_eq!(max_val.as_int_wide(&heap).unwrap(), i64::MAX);
}

#[test]
fn test_rune_is_alpha() {
    let mut heap = Heap::new();
    let result = rune_is_alpha(&[Value::from_rune('A')], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
    let result = rune_is_alpha(&[Value::from_rune('3')], &mut heap).unwrap();
    assert!(!result.as_bool().unwrap());
}

#[test]
fn test_rune_is_digit() {
    let mut heap = Heap::new();
    let result = rune_is_digit(&[Value::from_rune('9')], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
    let result = rune_is_digit(&[Value::from_rune('a')], &mut heap).unwrap();
    assert!(!result.as_bool().unwrap());
}

#[test]
fn test_rune_is_whitespace() {
    let mut heap = Heap::new();
    let result = rune_is_whitespace(&[Value::from_rune(' ')], &mut heap).unwrap();
    assert!(result.as_bool().unwrap());
    let result = rune_is_whitespace(&[Value::from_rune('a')], &mut heap).unwrap();
    assert!(!result.as_bool().unwrap());
}

#[test]
fn test_rune_case_conversion() {
    let mut heap = Heap::new();
    let upper = rune_to_upper(&[Value::from_rune('a')], &mut heap).unwrap();
    assert_eq!(get_rune(upper).unwrap(), 'A');
    let lower = rune_to_lower(&[Value::from_rune('A')], &mut heap).unwrap();
    assert_eq!(get_rune(lower).unwrap(), 'a');
}

#[test]
fn test_rune_int_roundtrip() {
    let mut heap = Heap::new();
    let n = rune_to_int(&[Value::from_rune('A')], &mut heap).unwrap();
    assert_eq!(n.as_int().unwrap(), 65);
    let c = rune_from_int(&[Value::from_int(65)], &mut heap).unwrap();
    assert_eq!(get_rune(c).unwrap(), 'A');
}

#[test]
fn test_rune_from_int_invalid() {
    let mut heap = Heap::new();
    // 0xD800 is a surrogate - not a valid Unicode scalar, must error
    assert!(rune_from_int(&[Value::from_int(0xD800)], &mut heap).is_err());
}
