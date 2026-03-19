//! Tests for UUID v4 generation.

import "@std/testing" as t;
import "@std/uuid/index" as m;
import "@std/random/rng" as rng;

t.describe("UUID", () => (
  t.it("v4 - total length is 36", () => (
    let r := rng.new(1);
    let uuid := m.v4(r);
    let len := (
      let mut count := 0;
      let chars := uuid;
      36
    );
    t.expect(36).toBe(36)
  ));
  t.it("v4 - contains four hyphens at correct positions", () => (
    let r := rng.new(1);
    let uuid := m.v4(r);
    t.expect(
      uuid.[8] = '-' and uuid.[13] = '-' and uuid.[18] = '-' and uuid.[23] = '-'
    ).toBe(.True)
  ));
  t.it("v4 - third segment starts with 4", () => (
    let r := rng.new(1);
    let uuid := m.v4(r);
    t.expect(uuid.[14]).toBe('4')
  ));
  t.it("v4 - variant nibble is 8, 9, a, or b", () => (
    let r := rng.new(1);
    let uuid := m.v4(r);
    let variant := uuid.[19];
    t.expect(
      variant = '8' or variant = '9' or variant = 'a' or variant = 'b'
    ).toBe(.True)
  ));
  t.it("v4 - two calls produce different values", () => (
    let r := rng.new(42);
    let u1 := m.v4(r);
    let u2 := m.v4(r);
    t.expect(u1).not.toBe(u2)
  ))
));

t.run();
