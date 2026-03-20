//! Tests for numeric constants.

import "@std/testing" as t;
import "@std/math/constants" as m;
import "@std/math/float" as f;

t.describe("Math.constants", () => (
  t.it("PI - greater than 3.14", () => (
    t.expect(m.PI > 3.14).toBe(.True)
  ));
  t.it("PI - less than 3.15", () => (
    t.expect(m.PI < 3.15).toBe(.True)
  ));
  t.it("E - greater than 2.71", () => (
    t.expect(m.E > 2.71).toBe(.True)
  ));
  t.it("E - less than 2.72", () => (
    t.expect(m.E < 2.72).toBe(.True)
  ));
  t.it("INFINITY - is infinite", () => (
    t.expect(f.is_infinite(m.INFINITY)).toBe(.True)
  ));
  t.it("INFINITY - greater than large number", () => (
    t.expect(m.INFINITY > 1000000.0).toBe(.True)
  ));
  t.it("NAN - is NaN", () => (
    t.expect(f.is_nan(m.NAN)).toBe(.True)
  ));
  t.it("INT_MIN - less than zero", () => (
    t.expect(m.INT_MIN < 0).toBe(.True)
  ));
  t.it("INT_MAX - greater than zero", () => (
    t.expect(m.INT_MAX > 0).toBe(.True)
  ));
  t.it("INT_MIN and INT_MAX - min is less than max", () => (
    t.expect(m.INT_MIN < m.INT_MAX).toBe(.True)
  ))
));

t.run();
