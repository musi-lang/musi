//! Tests for string truncation with suffix.

import "@std/testing" as t;
import "@std/fmt/truncate" as m;

t.describe("Fmt.truncate", () => (
  t.it("truncate - short string unchanged", () => (
    t.expect(m.truncate("hello", 10, "...")).toBe("hello")
  ));
  t.it("truncate - exact length unchanged", () => (
    t.expect(m.truncate("hello", 5, "...")).toBe("hello")
  ));
  t.it("truncate - long string gets suffix", () => (
    t.expect(m.truncate("hello world", 8, "...")).toBe("hello...")
  ));
  t.it("truncate - empty string unchanged", () => (
    t.expect(m.truncate("", 5, "...")).toBe("")
  ));
  t.it("truncate - empty suffix", () => (
    t.expect(m.truncate("hello world", 5, "")).toBe("hello")
  ));
  t.it("truncate - suffix fills max_len", () => (
    t.expect(m.truncate("abcdef", 3, "...")).toBe("...")
  ));
  t.it("truncate - single char suffix", () => (
    t.expect(m.truncate("hello world", 6, "~")).toBe("hello~")
  ))
));

t.run();
