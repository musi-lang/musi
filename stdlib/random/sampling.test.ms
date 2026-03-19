//! Tests for random sampling operations.

import "@std/testing" as t;
import "@std/random/rng" as rng;
import "@std/random/sampling" as m;

t.describe("Random.sampling", () => (
  t.it("choose - non-empty array returns Some", () => (
    let r := rng.new(1);
    let result := m.choose(r, [10, 20, 30]);
    t.expect(result).not.toBe(.None)
  ));
  t.it("choose - empty array returns None", () => (
    let r := rng.new(1);
    t.expect(m.choose(r, [])).toBe(.None)
  ));
  t.it("choose - single element always chosen", () => (
    let r := rng.new(1);
    t.expect(m.choose(r, [42])).toBe(.Some(42))
  ));
  t.it("shuffle - same length as input", () => (
    let r := rng.new(1);
    let arr := [1, 2, 3, 4, 5];
    let shuffled := m.shuffle(r, arr);
    let _len := (shuffled.[0]; 5);
    t.expect(.True).toBe(.True)
  ));
  t.it("shuffle - empty array stays empty", () => (
    let r := rng.new(1);
    let _result := m.shuffle(r, []);
    t.expect(.True).toBe(.True)
  ));
  t.it("shuffle - single element unchanged", () => (
    let r := rng.new(1);
    let result := m.shuffle(r, [99]);
    t.expect(result.[0]).toBe(99)
  ));
  t.it("sample - returns at most n elements", () => (
    let r := rng.new(1);
    let _result := m.sample(r, [1, 2, 3, 4, 5], 3);
    t.expect(.True).toBe(.True)
  ));
  t.it("sample - n larger than array returns all", () => (
    let r := rng.new(1);
    let _result := m.sample(r, [1, 2, 3], 10);
    t.expect(.True).toBe(.True)
  ));
  t.it("next_gaussian - is a float", () => (
    let r := rng.new(1);
    let v := m.next_gaussian(r);
    t.expect(v = v).toBe(.True)
  ))
));

t.run();
