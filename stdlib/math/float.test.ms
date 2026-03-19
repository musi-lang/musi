//! Tests for floating-point math operations.

import "@std/testing" as t;
import "@std/math/float" as m;

t.describe("Math.float", () => (
  t.it("floor - positive fractional", () => (
    t.expect(m.floor(3.7)).toBe(3.0)
  ));
  t.it("floor - negative fractional", () => (
    t.expect(m.floor(-2.3)).toBe(-3.0)
  ));
  t.it("ceil - positive fractional", () => (
    t.expect(m.ceil(3.2)).toBe(4.0)
  ));
  t.it("ceil - negative fractional", () => (
    t.expect(m.ceil(-2.7)).toBe(-2.0)
  ));
  t.it("round - rounds half up", () => (
    t.expect(m.round(2.5)).toBe(3.0)
  ));
  t.it("round - rounds down", () => (
    t.expect(m.round(2.3)).toBe(2.0)
  ));
  t.it("sqrt - perfect square", () => (
    t.expect(m.sqrt(4.0)).toBe(2.0)
  ));
  t.it("sqrt - zero", () => (
    t.expect(m.sqrt(0.0)).toBe(0.0)
  ));
  t.it("sqrt - one", () => (
    t.expect(m.sqrt(1.0)).toBe(1.0)
  ));
  t.it("sin - zero", () => (
    t.expect(m.sin(0.0)).toBe(0.0)
  ));
  t.it("cos - zero", () => (
    t.expect(m.cos(0.0)).toBe(1.0)
  ));
  t.it("log - one gives zero", () => (
    t.expect(m.log(1.0)).toBe(0.0)
  ));
  t.it("exp - zero gives one", () => (
    t.expect(m.exp(0.0)).toBe(1.0)
  ));
  t.it("is_nan - NaN value", () => (
    t.expect(m.is_nan(m.sqrt(-1.0))).toBe(.True)
  ));
  t.it("is_nan - normal value", () => (
    t.expect(m.is_nan(1.0)).toBe(.False)
  ));
  t.it("is_infinite - normal value", () => (
    t.expect(m.is_infinite(1.0)).toBe(.False)
  ));
  t.it("abs_float - negative", () => (
    t.expect(m.abs_float(-3.5)).toBe(3.5)
  ));
  t.it("abs_float - positive", () => (
    t.expect(m.abs_float(3.5)).toBe(3.5)
  ));
  t.it("abs_float - zero", () => (
    t.expect(m.abs_float(0.0)).toBe(0.0)
  ));
  t.it("clamp_float - below range", () => (
    t.expect(m.clamp_float(0.5, 1.0, 3.0)).toBe(1.0)
  ));
  t.it("clamp_float - above range", () => (
    t.expect(m.clamp_float(5.0, 1.0, 3.0)).toBe(3.0)
  ));
  t.it("clamp_float - within range", () => (
    t.expect(m.clamp_float(2.0, 1.0, 3.0)).toBe(2.0)
  ));
  t.it("min_float - returns smaller", () => (
    t.expect(m.min_float(1.5, 2.5)).toBe(1.5)
  ));
  t.it("max_float - returns larger", () => (
    t.expect(m.max_float(1.5, 2.5)).toBe(2.5)
  ));
  t.it("sign_float - negative", () => (
    t.expect(m.sign_float(-3.0)).toBe(-1.0)
  ));
  t.it("sign_float - zero", () => (
    t.expect(m.sign_float(0.0)).toBe(0.0)
  ));
  t.it("sign_float - positive", () => (
    t.expect(m.sign_float(5.0)).toBe(1.0)
  ));
  t.it("lerp - at t=0 gives a", () => (
    t.expect(m.lerp(0.0, 10.0, 0.0)).toBe(0.0)
  ));
  t.it("lerp - at t=1 gives b", () => (
    t.expect(m.lerp(0.0, 10.0, 1.0)).toBe(10.0)
  ));
  t.it("lerp - at t=0.5 gives midpoint", () => (
    t.expect(m.lerp(0.0, 10.0, 0.5)).toBe(5.0)
  ))
));

t.run();
