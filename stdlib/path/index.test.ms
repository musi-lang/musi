//! Tests for path manipulation operations.

import "@std/testing" as t;
import "@std/path/index" as path;

t.describe("Path", () => (
  t.it("join - basic join", () => (
    t.expect(path.join("foo", "bar")).toBe("foo/bar")
  ));
  t.it("join - a has trailing slash", () => (
    t.expect(path.join("foo/", "bar")).toBe("foo/bar")
  ));
  t.it("join - b is absolute, ignores a", () => (
    t.expect(path.join("foo", "/bar")).toBe("/bar")
  ));
  t.it("join - empty a returns b", () => (
    t.expect(path.join("", "bar")).toBe("bar")
  ));
  t.it("join - empty b returns empty", () => (
    t.expect(path.join("foo", "")).toBe("")
  ));
  t.it("dirname - standard path", () => (
    t.expect(path.dirname("/foo/bar/baz.txt")).toBe("/foo/bar")
  ));
  t.it("dirname - single segment returns dot", () => (
    t.expect(path.dirname("file.txt")).toBe(".")
  ));
  t.it("dirname - root file returns slash", () => (
    t.expect(path.dirname("/file.txt")).toBe("/")
  ));
  t.it("dirname - trailing slash stripped before compute", () => (
    t.expect(path.dirname("/foo/bar/")).toBe("/foo")
  ));
  t.it("basename - standard path", () => (
    t.expect(path.basename("/foo/bar/baz.txt")).toBe("baz.txt")
  ));
  t.it("basename - no slash returns full string", () => (
    t.expect(path.basename("file.txt")).toBe("file.txt")
  ));
  t.it("basename - trailing slash stripped", () => (
    t.expect(path.basename("/foo/bar/")).toBe("bar")
  ));
  t.it("extension - returns extension without dot", () => (
    t.expect(path.extension("/foo/bar.txt")).toBe(.Some("txt"))
  ));
  t.it("extension - no extension returns None", () => (
    t.expect(path.extension("/foo/bar")).toBe(.None)
  ));
  t.it("extension - dot at start returns None", () => (
    t.expect(path.extension(".hidden")).toBe(.None)
  ));
  t.it("with_extension - replaces existing extension", () => (
    t.expect(path.with_extension("/foo/bar.txt", "ms")).toBe("/foo/bar.ms")
  ));
  t.it("with_extension - adds extension when none", () => (
    t.expect(path.with_extension("/foo/bar", "txt")).toBe("/foo/bar.txt")
  ));
  t.it("with_extension - empty ext removes extension", () => (
    t.expect(path.with_extension("/foo/bar.txt", "")).toBe("/foo/bar")
  ));
  t.it("is_absolute - true for slash-prefixed", () => (
    t.expect(path.is_absolute("/foo/bar")).toBe(.True)
  ));
  t.it("is_absolute - false for relative path", () => (
    t.expect(path.is_absolute("foo/bar")).toBe(.False)
  ));
  t.it("normalize - resolves dot-dot segments", () => (
    t.expect(path.normalize("/foo/bar/../baz")).toBe("/foo/baz")
  ));
  t.it("normalize - resolves dot segments", () => (
    t.expect(path.normalize("/foo/./bar")).toBe("/foo/bar")
  ));
  t.it("normalize - empty result from relative becomes dot", () => (
    t.expect(path.normalize("foo/..")).toBe(".")
  ));
  t.it("normalize - absolute path stays absolute", () => (
    t.expect(path.is_absolute(path.normalize("/foo/../bar"))).toBe(.True)
  ))
));

t.run();
