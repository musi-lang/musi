//! Tests for path manipulation operations.

import "@std/testing" as t;
import "@std/path/index" as path;

t.runner.report(t.runner.run_suite([
  ("join — basic join", () => (
    t.assertions.assert_eq(path.join("foo", "bar"), "foo/bar")
  )),
  ("join — a has trailing slash", () => (
    t.assertions.assert_eq(path.join("foo/", "bar"), "foo/bar")
  )),
  ("join — b is absolute, ignores a", () => (
    t.assertions.assert_eq(path.join("foo", "/bar"), "/bar")
  )),
  ("join — empty a returns b", () => (
    t.assertions.assert_eq(path.join("", "bar"), "bar")
  )),
  ("join — empty b returns empty", () => (
    t.assertions.assert_eq(path.join("foo", ""), "")
  )),
  ("dirname — standard path", () => (
    t.assertions.assert_eq(path.dirname("/foo/bar/baz.txt"), "/foo/bar")
  )),
  ("dirname — single segment returns dot", () => (
    t.assertions.assert_eq(path.dirname("file.txt"), ".")
  )),
  ("dirname — root file returns slash", () => (
    t.assertions.assert_eq(path.dirname("/file.txt"), "/")
  )),
  ("dirname — trailing slash stripped before compute", () => (
    t.assertions.assert_eq(path.dirname("/foo/bar/"), "/foo")
  )),
  ("basename — standard path", () => (
    t.assertions.assert_eq(path.basename("/foo/bar/baz.txt"), "baz.txt")
  )),
  ("basename — no slash returns full string", () => (
    t.assertions.assert_eq(path.basename("file.txt"), "file.txt")
  )),
  ("basename — trailing slash stripped", () => (
    t.assertions.assert_eq(path.basename("/foo/bar/"), "bar")
  )),
  ("extension — returns extension without dot", () => (
    t.assertions.assert_eq(path.extension("/foo/bar.txt"), .Some("txt"))
  )),
  ("extension — no extension returns None", () => (
    t.assertions.assert_eq(path.extension("/foo/bar"), .None)
  )),
  ("extension — dot at start returns None", () => (
    t.assertions.assert_eq(path.extension(".hidden"), .None)
  )),
  ("with_extension — replaces existing extension", () => (
    t.assertions.assert_eq(path.with_extension("/foo/bar.txt", "ms"), "/foo/bar.ms")
  )),
  ("with_extension — adds extension when none", () => (
    t.assertions.assert_eq(path.with_extension("/foo/bar", "txt"), "/foo/bar.txt")
  )),
  ("with_extension — empty ext removes extension", () => (
    t.assertions.assert_eq(path.with_extension("/foo/bar.txt", ""), "/foo/bar")
  )),
  ("is_absolute — true for slash-prefixed", () => (
    t.assertions.assert_true(path.is_absolute("/foo/bar"))
  )),
  ("is_absolute — false for relative path", () => (
    t.assertions.assert_false(path.is_absolute("foo/bar"))
  )),
  ("normalize — resolves dot-dot segments", () => (
    t.assertions.assert_eq(path.normalize("/foo/bar/../baz"), "/foo/baz")
  )),
  ("normalize — resolves dot segments", () => (
    t.assertions.assert_eq(path.normalize("/foo/./bar"), "/foo/bar")
  )),
  ("normalize — empty result from relative becomes dot", () => (
    t.assertions.assert_eq(path.normalize("foo/.."), ".")
  )),
  ("normalize — absolute path stays absolute", () => (
    t.assertions.assert_true(path.is_absolute(path.normalize("/foo/../bar")))
  )),
]));
