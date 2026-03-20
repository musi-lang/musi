//! Tests for function combinators.

import "@std/testing" as t;
import "@std/func/index" as m;

t.describe("Func", () => (
  t.it("identity - returns input unchanged", () => (
    t.expect(m.identity(42)).toBe(42)
  ));
  t.it("identity - string value", () => (
    t.expect(m.identity("hello")).toBe("hello")
  ));
  t.it("constant - ignores second argument", () => (
    let always5 := m.constant(5);
    t.expect(always5("ignored")).toBe(5)
  ));
  t.it("constant - different ignored values", () => (
    let alwaysHi := m.constant("hi");
    t.expect(alwaysHi(99)).toBe("hi")
  ));
  t.it("compose - applies g then f", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let double_then_add1 := m.compose(add1, double);
    t.expect(double_then_add1(3)).toBe(7)
  ));
  t.it("compose - order matters", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let add1_then_double := m.compose(double, add1);
    t.expect(add1_then_double(3)).toBe(8)
  ));
  t.it("flip - reverses argument order", () => (
    let subtract := (a, b) => a - b;
    let flipped := m.flip(subtract);
    t.expect(flipped(3, 10)).toBe(7)
  ));
  t.it("apply - calls function with argument", () => (
    let double := (x) => x * 2;
    t.expect(m.apply(double, 5)).toBe(10)
  ));
  t.it("on - applies g to both args then f", () => (
    let strlen := (_) => 3;
    let add := (a, b) => a + b;
    let add_lengths := m.on(add, strlen);
    t.expect(add_lengths("abc", "de")).toBe(6)
  ));
  t.it("negate - inverts predicate result", () => (
    let is_positive := (x) => x > 0;
    let is_not_positive := m.negate(is_positive);
    t.expect(is_not_positive(-1)).toBe(.True)
  ));
  t.it("negate - true becomes false", () => (
    let always_true := (_) => .True;
    let always_false := m.negate(always_true);
    t.expect(always_false(0)).toBe(.False)
  ));
  t.it("pipe2 - applies f then g", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    t.expect(m.pipe2(3, add1, double)).toBe(8)
  ));
  t.it("pipe3 - applies f, g, h in order", () => (
    let add1 := (x) => x + 1;
    let double := (x) => x * 2;
    let negate := (x) => 0 - x;
    t.expect(m.pipe3(3, add1, double, negate)).toBe(-8)
  ))
));

t.run();
