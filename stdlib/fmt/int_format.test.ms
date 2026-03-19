//! Tests for integer formatting utilities.

import "@std/testing" as t;
import "@std/fmt/int_format" as m;

t.describe("Fmt.int_format", () => (
  t.it("int_to_hex - zero", () => (
    t.expect(m.int_to_hex(0)).toBe("0")
  ));
  t.it("int_to_hex - single digit", () => (
    t.expect(m.int_to_hex(10)).toBe("a")
  ));
  t.it("int_to_hex - 255", () => (
    t.expect(m.int_to_hex(255)).toBe("ff")
  ));
  t.it("int_to_hex - 256", () => (
    t.expect(m.int_to_hex(256)).toBe("100")
  ));
  t.it("int_to_hex - negative", () => (
    t.expect(m.int_to_hex(-1)).toBe("-1")
  ));
  t.it("int_to_hex - 15 is f", () => (
    t.expect(m.int_to_hex(15)).toBe("f")
  ));
  t.it("int_to_hex - 16 is 10", () => (
    t.expect(m.int_to_hex(16)).toBe("10")
  ));
  t.it("int_to_bin - zero", () => (
    t.expect(m.int_to_bin(0)).toBe("0")
  ));
  t.it("int_to_bin - one", () => (
    t.expect(m.int_to_bin(1)).toBe("1")
  ));
  t.it("int_to_bin - two", () => (
    t.expect(m.int_to_bin(2)).toBe("10")
  ));
  t.it("int_to_bin - eight", () => (
    t.expect(m.int_to_bin(8)).toBe("1000")
  ));
  t.it("int_to_bin - negative", () => (
    t.expect(m.int_to_bin(-2)).toBe("-10")
  ));
  t.it("int_to_bin - 255", () => (
    t.expect(m.int_to_bin(255)).toBe("11111111")
  ))
));

t.run();
