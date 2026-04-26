--- Standard library version.
export let version := "0.1.0";

--- Commonly imported standard library surface.
export let prelude := import "@std/prelude";
--- Optional value helpers surface.
export let option := import "@std/option";
--- Recoverable result helpers surface.
export let result := import "@std/result";
--- Comparison helpers surface.
export let cmp := import "@std/cmp";
--- Collections namespace.
export let collections := import "@std/collections";
--- Byte sequence helpers surface.
export let bytes := import "@std/bytes";
--- Text helpers surface.
export let text := import "@std/text";
--- Text and byte encoding namespace.
export let encoding := import "@std/encoding";
--- JSON helpers surface.
export let json := import "@std/json";
--- Math helpers surface.
export let math := import "@std/math";
--- Fixed-width bit-vector helpers surface.
export let bits := import "@std/bits";
--- Formatting helpers surface.
export let fmt := import "@std/fmt";
--- Error value helpers surface.
export let errors := import "@std/errors";

--- Date and time helpers surface.
export let datetime := import "@std/datetime";
--- Random number helpers surface.
export let random := import "@std/random";
--- Cryptographic helpers surface.
export let crypto := import "@std/crypto";
--- Filesystem helpers surface.
export let fs := import "@std/fs";
--- Path helpers surface.
export let path := import "@std/path";
--- Terminal I/O helpers surface.
export let io := import "@std/io";
--- Environment variable helpers surface.
export let env := import "@std/env";
--- Process helpers surface.
export let process := import "@std/process";
let osRecord := import "@std/os";
--- Typed operating system target helpers surface.
export let os := {
  current := osRecord.current,
  currentJit := osRecord.currentJit,
  targetOs := osRecord.targetOs,
  targetArch := osRecord.targetArch,
  targetFamily := osRecord.targetFamily,
  targetPointerWidth := osRecord.targetPointerWidth,
  targetEndian := osRecord.targetEndian,
  targetJit := osRecord.targetJit,
  matchesOs := osRecord.matchesOs,
  matchesArch := osRecord.matchesArch,
  matchesFamily := osRecord.matchesFamily,
  supportsJit := osRecord.supportsJit,
};
--- Logging helpers surface.
export let log := import "@std/log";
--- Foreign function interface helpers surface.
export let ffi := import "@std/ffi";

--- Command-line helpers surface.
export let cli := import "@std/cli";
--- UUID helpers surface.
export let uuid := import "@std/uuid";
--- Semantic version helpers surface.
export let semver := import "@std/semver";

--- Assertion helpers surface.
export let assert := import "@std/assert";
--- Testing helpers surface.
export let testing := import "@std/testing";
