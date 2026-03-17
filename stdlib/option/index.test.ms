//! Tests for option type and combinators.

import "@std/testing" as t;
import "@std/option/index" as opt;

t.runner.report(t.runner.run_suite([
  ("is_some - Some returns true", () => (
    t.assertions.assert_true(opt.is_some(.Some(1)))
  )),
  ("is_some - None returns false", () => (
    t.assertions.assert_false(opt.is_some(.None))
  )),
  ("is_none - None returns true", () => (
    t.assertions.assert_true(opt.is_none(.None))
  )),
  ("is_none - Some returns false", () => (
    t.assertions.assert_false(opt.is_none(.Some(42)))
  )),
  ("contains - Some with matching value", () => (
    t.assertions.assert_true(opt.contains(.Some(5), 5))
  )),
  ("contains - Some with non-matching value", () => (
    t.assertions.assert_false(opt.contains(.Some(5), 6))
  )),
  ("contains - None", () => (
    t.assertions.assert_false(opt.contains(.None, 5))
  )),
  ("unwrap_or - Some returns inner", () => (
    let result := opt.unwrap_or(.Some(10), 99);
    t.assertions.assert_eq(result, 10)
  )),
  ("unwrap_or - None returns default", () => (
    let result := opt.unwrap_or(.None, 99);
    t.assertions.assert_eq(result, 99)
  )),
  ("unwrap_or_else - Some returns inner", () => (
    let result := opt.unwrap_or_else(.Some(7), () => 0);
    t.assertions.assert_eq(result, 7)
  )),
  ("unwrap_or_else - None calls function", () => (
    let result := opt.unwrap_or_else(.None, () => 42);
    t.assertions.assert_eq(result, 42)
  )),
  ("map - Some applies function", () => (
    let result := opt.map(.Some(3), (x) => x * 2);
    t.assertions.assert_eq(result, .Some(6))
  )),
  ("map - None stays None", () => (
    let result := opt.map(.None, (x) => x * 2);
    t.assertions.assert_eq(result, .None)
  )),
  ("and_then - Some returns function result", () => (
    let result := opt.and_then(.Some(4), (x) => .Some(x + 1));
    t.assertions.assert_eq(result, .Some(5))
  )),
  ("and_then - Some with function returning None", () => (
    let result := opt.and_then(.Some(4), (_) => .None);
    t.assertions.assert_eq(result, .None)
  )),
  ("and_then - None stays None", () => (
    let result := opt.and_then(.None, (x) => .Some(x));
    t.assertions.assert_eq(result, .None)
  )),
  ("filter - predicate passes", () => (
    let result := opt.filter(.Some(6), (x) => x > 5);
    t.assertions.assert_eq(result, .Some(6))
  )),
  ("filter - predicate fails", () => (
    let result := opt.filter(.Some(3), (x) => x > 5);
    t.assertions.assert_eq(result, .None)
  )),
  ("filter - None stays None", () => (
    let result := opt.filter(.None, (x) => x > 5);
    t.assertions.assert_eq(result, .None)
  )),
  ("map_or - Some applies function", () => (
    let result := opt.map_or(.Some(3), 0, (x) => x * 10);
    t.assertions.assert_eq(result, 30)
  )),
  ("map_or - None returns default", () => (
    let result := opt.map_or(.None, 0, (x) => x * 10);
    t.assertions.assert_eq(result, 0)
  )),
  ("flatten - Some(Some(v)) unwraps to Some(v)", () => (
    let result := opt.flatten(.Some(.Some(8)));
    t.assertions.assert_eq(result, .Some(8))
  )),
  ("flatten - Some(None) unwraps to None", () => (
    let result := opt.flatten(.Some(.None));
    t.assertions.assert_eq(result, .None)
  )),
  ("flatten - None stays None", () => (
    let result := opt.flatten(.None);
    t.assertions.assert_eq(result, .None)
  )),
  ("or_else - Some returns itself", () => (
    let result := opt.or_else(.Some(1), () => .Some(99));
    t.assertions.assert_eq(result, .Some(1))
  )),
  ("or_else - None calls function", () => (
    let result := opt.or_else(.None, () => .Some(99));
    t.assertions.assert_eq(result, .Some(99))
  )),
  ("and_opt - both Some returns second", () => (
    let result := opt.and_opt(.Some(1), .Some(2));
    t.assertions.assert_eq(result, .Some(2))
  )),
  ("and_opt - first None returns None", () => (
    let result := opt.and_opt(.None, .Some(2));
    t.assertions.assert_eq(result, .None)
  )),
  ("or_opt - first Some returns first", () => (
    let result := opt.or_opt(.Some(1), .Some(2));
    t.assertions.assert_eq(result, .Some(1))
  )),
  ("or_opt - first None returns second", () => (
    let result := opt.or_opt(.None, .Some(2));
    t.assertions.assert_eq(result, .Some(2))
  )),
  ("xor_opt - only first Some", () => (
    let result := opt.xor_opt(.Some(1), .None);
    t.assertions.assert_eq(result, .Some(1))
  )),
  ("xor_opt - only second Some", () => (
    let result := opt.xor_opt(.None, .Some(2));
    t.assertions.assert_eq(result, .Some(2))
  )),
  ("xor_opt - both Some returns None", () => (
    let result := opt.xor_opt(.Some(1), .Some(2));
    t.assertions.assert_eq(result, .None)
  )),
  ("xor_opt - both None returns None", () => (
    let result := opt.xor_opt(.None, .None);
    t.assertions.assert_eq(result, .None)
  )),
  ("zip - both Some returns Some of tuple", () => (
    let result := opt.zip(.Some(1), .Some("a"));
    t.assertions.assert_eq(result, .Some((1, "a")))
  )),
  ("zip - first None returns None", () => (
    let result := opt.zip(.None, .Some("a"));
    t.assertions.assert_eq(result, .None)
  )),
  ("zip - second None returns None", () => (
    let result := opt.zip(.Some(1), .None);
    t.assertions.assert_eq(result, .None)
  )),
  ("zip_with - both Some applies function", () => (
    let result := opt.zip_with(.Some(3), .Some(4), (a, b) => a + b);
    t.assertions.assert_eq(result, .Some(7))
  )),
  ("zip_with - first None returns None", () => (
    let result := opt.zip_with(.None, .Some(4), (a, b) => a + b);
    t.assertions.assert_eq(result, .None)
  )),
  ("to_result - Some becomes Ok", () => (
    let result := opt.to_result(.Some(10), "missing");
    t.assertions.assert_eq(result, .Ok(10))
  )),
  ("to_result - None becomes Err", () => (
    let result := opt.to_result(.None, "missing");
    t.assertions.assert_eq(result, .Err("missing"))
  )),
  ("to_array - Some produces single-element array", () => (
    let result := opt.to_array(.Some(5));
    t.assertions.assert_eq(result, [5])
  )),
  ("to_array - None produces empty array", () => (
    let result := opt.to_array(.None);
    t.assertions.assert_eq(result, [])
  )),
]));
