//! Tests for hex encode/decode operations.

import "@std/testing" as t;
import "@std/encoding/hex" as hex;

t.runner.report(t.runner.run_suite([
  ("encode - empty bytes produces empty string", () => (
    t.assertions.assert_eq(hex.encode([]), "")
  )),
  ("encode - single byte zero", () => (
    t.assertions.assert_eq(hex.encode([0]), "00")
  )),
  ("encode - single byte 255", () => (
    t.assertions.assert_eq(hex.encode([255]), "ff")
  )),
  ("encode - lowercase output", () => (
    t.assertions.assert_eq(hex.encode([171]), "ab")
  )),
  ("encode - multiple bytes", () => (
    t.assertions.assert_eq(hex.encode([0, 16, 255]), "0010ff")
  )),
  ("encode_upper - uppercase output", () => (
    t.assertions.assert_eq(hex.encode_upper([171]), "AB")
  )),
  ("encode_upper - multiple bytes uppercase", () => (
    t.assertions.assert_eq(hex.encode_upper([0, 16, 255]), "0010FF")
  )),
  ("decode - empty string returns Ok empty", () => (
    t.assertions.assert_eq(hex.decode(""), .Ok([]))
  )),
  ("decode - odd length returns OddLength error", () => (
    t.assertions.assert_eq(hex.decode("abc"), .Err(.OddLength))
  )),
  ("decode - invalid char returns InvalidChar error", () => (
    match hex.decode("zz") (
      .Err(.InvalidChar(_)) => t.assertions.assert_true(.True)
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("decode - single byte zero", () => (
    t.assertions.assert_eq(hex.decode("00"), .Ok([0]))
  )),
  ("decode - single byte ff", () => (
    t.assertions.assert_eq(hex.decode("ff"), .Ok([255]))
  )),
  ("decode - uppercase hex digits", () => (
    t.assertions.assert_eq(hex.decode("FF"), .Ok([255]))
  )),
  ("encode/decode - roundtrip", () => (
    let bytes := [0, 1, 127, 128, 255];
    t.assertions.assert_eq(hex.decode(hex.encode(bytes)), .Ok(bytes))
  )),
  ("encode_upper/decode - roundtrip", () => (
    let bytes := [16, 32, 48, 64];
    t.assertions.assert_eq(hex.decode(hex.encode_upper(bytes)), .Ok(bytes))
  )),
]));
