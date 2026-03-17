//! Tests for sorted array operations.

import "@std/testing" as t;
import "@std/collections/sorted_array" as sa;

t.runner.report(t.runner.run_suite([
  ("new - starts empty", () => (
    let s := sa.new();
    t.assertions.assert_eq(sa.len(s), 0)
  )),
  ("contains - empty array returns false", () => (
    let s := sa.new();
    t.assertions.assert_false(sa.contains(s, 5))
  )),
  ("insert - len increments", () => (
    let s := sa.new();
    sa.insert(s, 10);
    sa.insert(s, 5);
    t.assertions.assert_eq(sa.len(s), 2)
  )),
  ("insert - maintains sorted order", () => (
    let s := sa.new();
    sa.insert(s, 3);
    sa.insert(s, 1);
    sa.insert(s, 2);
    let arr := sa.to_array(s);
    t.assertions.assert_true(arr.[0] = 1 and arr.[1] = 2 and arr.[2] = 3)
  )),
  ("contains - finds inserted element", () => (
    let s := sa.new();
    sa.insert(s, 42);
    t.assertions.assert_true(sa.contains(s, 42))
  )),
  ("contains - returns false for absent element", () => (
    let s := sa.new();
    sa.insert(s, 10);
    sa.insert(s, 20);
    t.assertions.assert_false(sa.contains(s, 15))
  )),
  ("remove - returns True when element exists", () => (
    let s := sa.new();
    sa.insert(s, 7);
    t.assertions.assert_eq(sa.remove(s, 7), .True)
  )),
  ("remove - returns False when element absent", () => (
    let s := sa.new();
    sa.insert(s, 7);
    t.assertions.assert_eq(sa.remove(s, 99), .False)
  )),
  ("remove - element no longer contained after removal", () => (
    let s := sa.new();
    sa.insert(s, 5);
    sa.remove(s, 5);
    t.assertions.assert_false(sa.contains(s, 5))
  )),
  ("remove - len decrements", () => (
    let s := sa.new();
    sa.insert(s, 1);
    sa.insert(s, 2);
    sa.remove(s, 1);
    t.assertions.assert_eq(sa.len(s), 1)
  )),
  ("insert - duplicates both present after insert", () => (
    let s := sa.new();
    sa.insert(s, 5);
    sa.insert(s, 5);
    t.assertions.assert_eq(sa.len(s), 2)
  )),
  ("to_array - empty array returns empty", () => (
    let s := sa.new();
    let arr := sa.to_array(s);
    t.assertions.assert_eq(arr, [])
  )),
  ("to_array - returns sorted snapshot", () => (
    let s := sa.new();
    sa.insert(s, 9);
    sa.insert(s, 3);
    sa.insert(s, 6);
    let arr := sa.to_array(s);
    t.assertions.assert_true(arr.[0] = 3 and arr.[1] = 6 and arr.[2] = 9)
  )),
]));
