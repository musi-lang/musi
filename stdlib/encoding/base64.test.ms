//! Tests for base64 encode/decode operations.

import "@std/testing" as t;
import "@std/encoding/base64" as base64;

t.describe("Encoding.base64", () => (
  t.it("encode - empty bytes produces empty string", () => (
    t.expect(base64.encode([])).toBe("")
  ));
  t.it("encode - single byte with double padding", () => (
    // 0x4D = 77 = 'M'; base64: "TQ=="
    t.expect(base64.encode([77])).toBe("TQ==")
  ));
  t.it("encode - two bytes with single padding", () => (
    // [77, 97] = 'Ma'; base64: "TWE="
    t.expect(base64.encode([77, 97])).toBe("TWE=")
  ));
  t.it("encode - three bytes no padding", () => (
    // [77, 97, 110] = 'Man'; base64: "TWFu"
    t.expect(base64.encode([77, 97, 110])).toBe("TWFu")
  ));
  t.it("decode - empty string returns InvalidLength error", () => (
    t.expect(base64.decode("")).toBe(.Err(.InvalidLength))
  ));
  t.it("decode - non-multiple-of-4 length returns InvalidLength", () => (
    t.expect(base64.decode("TWF")).toBe(.Err(.InvalidLength))
  ));
  t.it("decode - valid double-padded string", () => (
    t.expect(base64.decode("TQ==")).toBe(.Ok([77]))
  ));
  t.it("decode - valid single-padded string", () => (
    t.expect(base64.decode("TWE=")).toBe(.Ok([77, 97]))
  ));
  t.it("decode - valid no-padding string", () => (
    t.expect(base64.decode("TWFu")).toBe(.Ok([77, 97, 110]))
  ));
  t.it("encode/decode - roundtrip for arbitrary bytes", () => (
    let bytes := [0, 1, 127, 128, 255];
    match base64.decode(base64.encode(bytes)) (
      .Ok(result) => t.expect(result).toBe(bytes)
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("encode_url_safe - uses - and _ instead of + and /", () => (
    // Bytes [251, 255] → 6-bit groups include indices 62 and 63,
    // which use +/ in standard but -_ in url-safe encoding.
    let encoded := base64.encode_url_safe([251, 255]);
    let standard := base64.encode([251, 255]);
    t.expect(encoded).not.toBe(standard)
  ));
  t.it("decode_url_safe - roundtrip", () => (
    let bytes := [100, 200, 150];
    match base64.decode_url_safe(base64.encode_url_safe(bytes)) (
      .Ok(result) => t.expect(result).toBe(bytes)
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ))
));

t.run();
