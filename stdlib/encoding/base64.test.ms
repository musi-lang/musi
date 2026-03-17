//! Tests for base64 encode/decode operations.

import "@std/testing" as t;
import "@std/encoding/base64" as base64;

t.runner.report(t.runner.run_suite([
  ("encode - empty bytes produces empty string", () => (
    t.assertions.assert_eq(base64.encode([]), "")
  )),
  ("encode - single byte with double padding", () => (
    // 0x4D = 77 = 'M'; base64: "TQ=="
    t.assertions.assert_eq(base64.encode([77]), "TQ==")
  )),
  ("encode - two bytes with single padding", () => (
    // [77, 97] = 'Ma'; base64: "TWE="
    t.assertions.assert_eq(base64.encode([77, 97]), "TWE=")
  )),
  ("encode - three bytes no padding", () => (
    // [77, 97, 110] = 'Man'; base64: "TWFu"
    t.assertions.assert_eq(base64.encode([77, 97, 110]), "TWFu")
  )),
  ("decode - empty string returns InvalidLength error", () => (
    t.assertions.assert_eq(base64.decode(""), .Err(.InvalidLength))
  )),
  ("decode - non-multiple-of-4 length returns InvalidLength", () => (
    t.assertions.assert_eq(base64.decode("TWF"), .Err(.InvalidLength))
  )),
  ("decode - valid double-padded string", () => (
    t.assertions.assert_eq(base64.decode("TQ=="), .Ok([77]))
  )),
  ("decode - valid single-padded string", () => (
    t.assertions.assert_eq(base64.decode("TWE="), .Ok([77, 97]))
  )),
  ("decode - valid no-padding string", () => (
    t.assertions.assert_eq(base64.decode("TWFu"), .Ok([77, 97, 110]))
  )),
  ("encode/decode - roundtrip for arbitrary bytes", () => (
    let bytes := [0, 1, 127, 128, 255];
    match base64.decode(base64.encode(bytes)) (
      .Ok(result) => t.assertions.assert_eq(result, bytes)
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("encode_url_safe - uses - and _ instead of + and /", () => (
    // Bytes [251, 255] → 6-bit groups include indices 62 and 63,
    // which use +/ in standard but -_ in url-safe encoding.
    let encoded := base64.encode_url_safe([251, 255]);
    let standard := base64.encode([251, 255]);
    t.assertions.assert_ne(encoded, standard)
  )),
  ("decode_url_safe - roundtrip", () => (
    let bytes := [100, 200, 150];
    match base64.decode_url_safe(base64.encode_url_safe(bytes)) (
      .Ok(result) => t.assertions.assert_eq(result, bytes)
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
]));
