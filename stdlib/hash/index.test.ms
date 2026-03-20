//! Tests for FNV-1a hash functions.

import "@std/testing" as t;
import "@std/hash/index" as m;

t.describe("Hash", () => (
  t.it("hash_int - returns non-negative", () => (
    t.expect(m.hash_int(42) >= 0).toBe(.True)
  ));
  t.it("hash_int - zero input", () => (
    t.expect(m.hash_int(0) >= 0).toBe(.True)
  ));
  t.it("hash_int - negative input", () => (
    t.expect(m.hash_int(-99) >= 0).toBe(.True)
  ));
  t.it("hash_int - same input same output", () => (
    t.expect(m.hash_int(7)).toBe(m.hash_int(7))
  ));
  t.it("hash_int - different inputs differ", () => (
    t.expect(m.hash_int(1)).not.toBe(m.hash_int(2))
  ));
  t.it("hash_str - returns non-negative", () => (
    t.expect(m.hash_str("hello") >= 0).toBe(.True)
  ));
  t.it("hash_str - empty string", () => (
    t.expect(m.hash_str("") >= 0).toBe(.True)
  ));
  t.it("hash_str - same input same output", () => (
    t.expect(m.hash_str("abc")).toBe(m.hash_str("abc"))
  ));
  t.it("hash_str - different strings differ", () => (
    t.expect(m.hash_str("abc")).not.toBe(m.hash_str("xyz"))
  ));
  t.it("hash_bool - true and false differ", () => (
    t.expect(m.hash_bool(.True)).not.toBe(m.hash_bool(.False))
  ));
  t.it("hash_bool - stable for true", () => (
    t.expect(m.hash_bool(.True)).toBe(m.hash_bool(.True))
  ));
  t.it("combine - returns non-negative", () => (
    t.expect(m.combine(m.hash_int(1), m.hash_int(2)) >= 0).toBe(.True)
  ));
  t.it("combine - same inputs same output", () => (
    let h1 := m.hash_int(10);
    let h2 := m.hash_int(20);
    t.expect(m.combine(h1, h2)).toBe(m.combine(h1, h2))
  ))
));

t.run();
