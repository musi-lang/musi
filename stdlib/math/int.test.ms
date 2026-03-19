//! Tests for integer math operations.

import "@std/testing" as t;
import "@std/math/int" as m;

t.describe("Math.int", () => (
  t.it("abs - positive stays positive", () => (
    t.expect(m.abs(5)).toBe(5)
  ));
  t.it("abs - negative becomes positive", () => (
    t.expect(m.abs(-7)).toBe(7)
  ));
  t.it("abs - zero stays zero", () => (
    t.expect(m.abs(0)).toBe(0)
  ));
  t.it("min_int - returns smaller", () => (
    t.expect(m.min_int(3, 8)).toBe(3)
  ));
  t.it("min_int - equal values", () => (
    t.expect(m.min_int(4, 4)).toBe(4)
  ));
  t.it("max_int - returns larger", () => (
    t.expect(m.max_int(3, 8)).toBe(8)
  ));
  t.it("max_int - equal values", () => (
    t.expect(m.max_int(4, 4)).toBe(4)
  ));
  t.it("clamp_int - value below range", () => (
    t.expect(m.clamp_int(1, 5, 10)).toBe(5)
  ));
  t.it("clamp_int - value above range", () => (
    t.expect(m.clamp_int(15, 5, 10)).toBe(10)
  ));
  t.it("clamp_int - value within range", () => (
    t.expect(m.clamp_int(7, 5, 10)).toBe(7)
  ));
  t.it("pow - base to positive exponent", () => (
    t.expect(m.pow(2, 10)).toBe(1024)
  ));
  t.it("pow - any base to zero", () => (
    t.expect(m.pow(5, 0)).toBe(1)
  ));
  t.it("pow - base one", () => (
    t.expect(m.pow(1, 100)).toBe(1)
  ));
  t.it("sign_int - negative", () => (
    t.expect(m.sign_int(-42)).toBe(-1)
  ));
  t.it("sign_int - zero", () => (
    t.expect(m.sign_int(0)).toBe(0)
  ));
  t.it("sign_int - positive", () => (
    t.expect(m.sign_int(99)).toBe(1)
  ));
  t.it("is_even - even number", () => (
    t.expect(m.is_even(4)).toBe(.True)
  ));
  t.it("is_even - odd number", () => (
    t.expect(m.is_even(7)).toBe(.False)
  ));
  t.it("is_odd - odd number", () => (
    t.expect(m.is_odd(3)).toBe(.True)
  ));
  t.it("is_odd - even number", () => (
    t.expect(m.is_odd(6)).toBe(.False)
  ));
  t.it("gcd - two positive numbers", () => (
    t.expect(m.gcd(12, 8)).toBe(4)
  ));
  t.it("gcd - coprime numbers", () => (
    t.expect(m.gcd(7, 13)).toBe(1)
  ));
  t.it("gcd - one is zero", () => (
    t.expect(m.gcd(0, 5)).toBe(5)
  ));
  t.it("gcd - negatives handled", () => (
    t.expect(m.gcd(-12, 8)).toBe(4)
  ));
  t.it("lcm - two positive numbers", () => (
    t.expect(m.lcm(4, 6)).toBe(12)
  ));
  t.it("lcm - one is zero", () => (
    t.expect(m.lcm(0, 5)).toBe(0)
  ));
  t.it("lcm - coprime numbers", () => (
    t.expect(m.lcm(3, 7)).toBe(21)
  ))
));

t.run();
