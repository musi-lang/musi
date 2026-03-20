//! Tests for hex encode/decode operations.

import "@std/testing" as t;
import "@std/encoding/hex" as hex;

t.describe("Encoding.hex", () => (
  t.it("encode - empty bytes produces empty string", () => (
    t.expect(hex.encode([])).toBe("")
  ));
  t.it("encode - single byte zero", () => (
    t.expect(hex.encode([0])).toBe("00")
  ));
  t.it("encode - single byte 255", () => (
    t.expect(hex.encode([255])).toBe("ff")
  ));
  t.it("encode - lowercase output", () => (
    t.expect(hex.encode([171])).toBe("ab")
  ));
  t.it("encode - multiple bytes", () => (
    t.expect(hex.encode([0, 16, 255])).toBe("0010ff")
  ));
  t.it("encode_upper - uppercase output", () => (
    t.expect(hex.encode_upper([171])).toBe("AB")
  ));
  t.it("encode_upper - multiple bytes uppercase", () => (
    t.expect(hex.encode_upper([0, 16, 255])).toBe("0010FF")
  ));
  t.it("decode - empty string returns Ok empty", () => (
    t.expect(hex.decode("")).toBe(.Ok([]))
  ));
  t.it("decode - odd length returns OddLength error", () => (
    t.expect(hex.decode("abc")).toBe(.Err(.OddLength))
  ));
  t.it("decode - invalid char returns InvalidChar error", () => (
    match hex.decode("zz") (
      .Err(.InvalidChar(_)) => t.expect(.True).toBe(.True)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("decode - single byte zero", () => (
    t.expect(hex.decode("00")).toBe(.Ok([0]))
  ));
  t.it("decode - single byte ff", () => (
    t.expect(hex.decode("ff")).toBe(.Ok([255]))
  ));
  t.it("decode - uppercase hex digits", () => (
    t.expect(hex.decode("FF")).toBe(.Ok([255]))
  ));
  t.it("encode/decode - roundtrip", () => (
    let bytes := [0, 1, 127, 128, 255];
    t.expect(hex.decode(hex.encode(bytes))).toBe(.Ok(bytes))
  ));
  t.it("encode_upper/decode - roundtrip", () => (
    let bytes := [16, 32, 48, 64];
    t.expect(hex.decode(hex.encode_upper(bytes))).toBe(.Ok(bytes))
  ))
));

t.run();
