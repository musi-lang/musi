//! Tests for option type and combinators.

import "@std/testing" as t;
import "@std/option/index" as opt;

t.describe("Option", () => (
  t.it("is_some - Some returns true", () => (
    t.expect(opt.is_some(.Some(1))).toBe(.True)
  ));
  t.it("is_some - None returns false", () => (
    t.expect(opt.is_some(.None)).toBe(.False)
  ));
  t.it("is_none - None returns true", () => (
    t.expect(opt.is_none(.None)).toBe(.True)
  ));
  t.it("is_none - Some returns false", () => (
    t.expect(opt.is_none(.Some(42))).toBe(.False)
  ));
  t.it("contains - Some with matching value", () => (
    t.expect(opt.contains(.Some(5), 5)).toBe(.True)
  ));
  t.it("contains - Some with non-matching value", () => (
    t.expect(opt.contains(.Some(5), 6)).toBe(.False)
  ));
  t.it("contains - None", () => (
    t.expect(opt.contains(.None, 5)).toBe(.False)
  ));
  t.it("unwrap_or - Some returns inner", () => (
    let result := opt.unwrap_or(.Some(10), 99);
    t.expect(result).toBe(10)
  ));
  t.it("unwrap_or - None returns default", () => (
    let result := opt.unwrap_or(.None, 99);
    t.expect(result).toBe(99)
  ));
  t.it("unwrap_or_else - Some returns inner", () => (
    let result := opt.unwrap_or_else(.Some(7), () => 0);
    t.expect(result).toBe(7)
  ));
  t.it("unwrap_or_else - None calls function", () => (
    let result := opt.unwrap_or_else(.None, () => 42);
    t.expect(result).toBe(42)
  ));
  t.it("map - Some applies function", () => (
    let result := opt.map(.Some(3), (x) => x * 2);
    t.expect(result).toBe(.Some(6))
  ));
  t.it("map - None stays None", () => (
    let result := opt.map(.None, (x) => x * 2);
    t.expect(result).toBe(.None)
  ));
  t.it("and_then - Some returns function result", () => (
    let result := opt.and_then(.Some(4), (x) => .Some(x + 1));
    t.expect(result).toBe(.Some(5))
  ));
  t.it("and_then - Some with function returning None", () => (
    let result := opt.and_then(.Some(4), (_) => .None);
    t.expect(result).toBe(.None)
  ));
  t.it("and_then - None stays None", () => (
    let result := opt.and_then(.None, (x) => .Some(x));
    t.expect(result).toBe(.None)
  ));
  t.it("filter - predicate passes", () => (
    let result := opt.filter(.Some(6), (x) => x > 5);
    t.expect(result).toBe(.Some(6))
  ));
  t.it("filter - predicate fails", () => (
    let result := opt.filter(.Some(3), (x) => x > 5);
    t.expect(result).toBe(.None)
  ));
  t.it("filter - None stays None", () => (
    let result := opt.filter(.None, (x) => x > 5);
    t.expect(result).toBe(.None)
  ));
  t.it("map_or - Some applies function", () => (
    let result := opt.map_or(.Some(3), 0, (x) => x * 10);
    t.expect(result).toBe(30)
  ));
  t.it("map_or - None returns default", () => (
    let result := opt.map_or(.None, 0, (x) => x * 10);
    t.expect(result).toBe(0)
  ));
  t.it("flatten - Some(Some(v)) unwraps to Some(v)", () => (
    let result := opt.flatten(.Some(.Some(8)));
    t.expect(result).toBe(.Some(8))
  ));
  t.it("flatten - Some(None) unwraps to None", () => (
    let result := opt.flatten(.Some(.None));
    t.expect(result).toBe(.None)
  ));
  t.it("flatten - None stays None", () => (
    let result := opt.flatten(.None);
    t.expect(result).toBe(.None)
  ));
  t.it("or_else - Some returns itself", () => (
    let result := opt.or_else(.Some(1), () => .Some(99));
    t.expect(result).toBe(.Some(1))
  ));
  t.it("or_else - None calls function", () => (
    let result := opt.or_else(.None, () => .Some(99));
    t.expect(result).toBe(.Some(99))
  ));
  t.it("and_opt - both Some returns second", () => (
    let result := opt.and_opt(.Some(1), .Some(2));
    t.expect(result).toBe(.Some(2))
  ));
  t.it("and_opt - first None returns None", () => (
    let result := opt.and_opt(.None, .Some(2));
    t.expect(result).toBe(.None)
  ));
  t.it("or_opt - first Some returns first", () => (
    let result := opt.or_opt(.Some(1), .Some(2));
    t.expect(result).toBe(.Some(1))
  ));
  t.it("or_opt - first None returns second", () => (
    let result := opt.or_opt(.None, .Some(2));
    t.expect(result).toBe(.Some(2))
  ));
  t.it("xor_opt - only first Some", () => (
    let result := opt.xor_opt(.Some(1), .None);
    t.expect(result).toBe(.Some(1))
  ));
  t.it("xor_opt - only second Some", () => (
    let result := opt.xor_opt(.None, .Some(2));
    t.expect(result).toBe(.Some(2))
  ));
  t.it("xor_opt - both Some returns None", () => (
    let result := opt.xor_opt(.Some(1), .Some(2));
    t.expect(result).toBe(.None)
  ));
  t.it("xor_opt - both None returns None", () => (
    let result := opt.xor_opt(.None, .None);
    t.expect(result).toBe(.None)
  ));
  t.it("zip - both Some returns Some of tuple", () => (
    let result := opt.zip(.Some(1), .Some("a"));
    t.expect(result).toBe(.Some((1, "a")))
  ));
  t.it("zip - first None returns None", () => (
    let result := opt.zip(.None, .Some("a"));
    t.expect(result).toBe(.None)
  ));
  t.it("zip - second None returns None", () => (
    let result := opt.zip(.Some(1), .None);
    t.expect(result).toBe(.None)
  ));
  t.it("zip_with - both Some applies function", () => (
    let result := opt.zip_with(.Some(3), .Some(4), (a, b) => a + b);
    t.expect(result).toBe(.Some(7))
  ));
  t.it("zip_with - first None returns None", () => (
    let result := opt.zip_with(.None, .Some(4), (a, b) => a + b);
    t.expect(result).toBe(.None)
  ));
  t.it("to_result - Some becomes Ok", () => (
    let result := opt.to_result(.Some(10), "missing");
    t.expect(result).toBe(.Ok(10))
  ));
  t.it("to_result - None becomes Err", () => (
    let result := opt.to_result(.None, "missing");
    t.expect(result).toBe(.Err("missing"))
  ));
  t.it("to_array - Some produces single-element array", () => (
    let result := opt.to_array(.Some(5));
    t.expect(result).toBe([5])
  ));
  t.it("to_array - None produces empty array", () => (
    let result := opt.to_array(.None);
    t.expect(result).toBe([])
  ))
));

t.run();
