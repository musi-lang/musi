/// Standard library version.
export let version := "0.1.0";
/// Assertion helpers module.
export let assert := import "@std/assert";
/// Array helpers module.
export let array := import "@std/array";
/// Byte sequence helpers module.
export let bytes := import "@std/bytes";
/// Comparison helpers module.
export let cmp := import "@std/cmp";
/// Text and byte encoding module namespace.
export let encoding := {
  base64 := import "@std/encoding/base64",
  hex := import "@std/encoding/hex",
  utf8 := import "@std/encoding/utf8"
};
/// Environment variable helpers module.
export let env := import "@std/env";
/// Filesystem helpers module.
export let fs := import "@std/fs";
/// Foreign function interface helpers module.
export let ffi := import "@std/ffi";
/// Terminal I/O helpers module.
export let io := import "@std/io";
/// Iterator-style sequence helpers module.
export let iter := import "@std/iter";
/// JSON helpers module.
export let json := import "@std/json";
/// Logging helpers module.
export let log := import "@std/log";
/// List helpers module.
export let list := import "@std/list";
/// Math helpers module.
export let math := import "@std/math";
/// Optional value helpers module.
export let option := import "@std/option";
/// Typed operating system target helpers module.
export let os := import "@std/os";
/// Path helpers module.
export let path := import "@std/path";
/// Commonly imported standard library surface.
export let prelude := import "@std/prelude";
/// Process helpers module.
export let process := import "@std/process";
/// Random number helpers module.
export let random := import "@std/random";
/// Recoverable result helpers module.
export let result := import "@std/result";
/// Slice helpers module.
export let slice := import "@std/slice";
/// Raw system target facts module.
export let sys := import "@std/sys";
/// Testing helpers module.
export let testing := import "@std/testing";
/// Text helpers module.
export let text := import "@std/text";
/// Time helpers module.
export let time := import "@std/time";
