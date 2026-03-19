//! Tests for sorted array operations.

import "@std/testing" as t;
import "@std/collections/sorted_array" as sa;

t.describe("SortedArray", () => (
  t.it("new - starts empty", () => (
    let s := sa.new();
    t.expect(sa.len(s)).toBe(0)
  ));
  t.it("contains - empty array returns false", () => (
    let s := sa.new();
    t.expect(sa.contains(s, 5)).toBe(.False)
  ));
  t.it("insert - len increments", () => (
    let s := sa.new();
    sa.insert(s, 10);
    sa.insert(s, 5);
    t.expect(sa.len(s)).toBe(2)
  ));
  t.it("insert - maintains sorted order", () => (
    let s := sa.new();
    sa.insert(s, 3);
    sa.insert(s, 1);
    sa.insert(s, 2);
    let arr := sa.to_array(s);
    t.expect(arr.[0] = 1 and arr.[1] = 2 and arr.[2] = 3).toBe(.True)
  ));
  t.it("contains - finds inserted element", () => (
    let s := sa.new();
    sa.insert(s, 42);
    t.expect(sa.contains(s, 42)).toBe(.True)
  ));
  t.it("contains - returns false for absent element", () => (
    let s := sa.new();
    sa.insert(s, 10);
    sa.insert(s, 20);
    t.expect(sa.contains(s, 15)).toBe(.False)
  ));
  t.it("remove - returns True when element exists", () => (
    let s := sa.new();
    sa.insert(s, 7);
    t.expect(sa.remove(s, 7)).toBe(.True)
  ));
  t.it("remove - returns False when element absent", () => (
    let s := sa.new();
    sa.insert(s, 7);
    t.expect(sa.remove(s, 99)).toBe(.False)
  ));
  t.it("remove - element no longer contained after removal", () => (
    let s := sa.new();
    sa.insert(s, 5);
    sa.remove(s, 5);
    t.expect(sa.contains(s, 5)).toBe(.False)
  ));
  t.it("remove - len decrements", () => (
    let s := sa.new();
    sa.insert(s, 1);
    sa.insert(s, 2);
    sa.remove(s, 1);
    t.expect(sa.len(s)).toBe(1)
  ));
  t.it("insert - duplicates both present after insert", () => (
    let s := sa.new();
    sa.insert(s, 5);
    sa.insert(s, 5);
    t.expect(sa.len(s)).toBe(2)
  ));
  t.it("to_array - empty array returns empty", () => (
    let s := sa.new();
    let arr := sa.to_array(s);
    t.expect(arr).toBe([])
  ));
  t.it("to_array - returns sorted snapshot", () => (
    let s := sa.new();
    sa.insert(s, 9);
    sa.insert(s, 3);
    sa.insert(s, 6);
    let arr := sa.to_array(s);
    t.expect(arr.[0] = 3 and arr.[1] = 6 and arr.[2] = 9).toBe(.True)
  ))
));

t.run();
