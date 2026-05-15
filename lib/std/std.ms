--! Standard library root module.

--- Standard library version.
export let version := "0.1.0";

--- Commonly imported standard library values.
export let prelude := import "@std/prelude";
--- ASCII byte classification and conversion tools.
export let ascii := import "@std/ascii";
--- Maybe value tools.
export let maybe := import "@std/maybe";
--- Expected success or failure tools.
export let expect := import "@std/expect";
--- Comparison tools.
export let cmp := import "@std/cmp";
--- Collection types and functions.
export let collections := import "@std/collections";
--- Byte sequence tools.
export let bytes := import "@std/bytes";
--- Text tools.
export let text := import "@std/text";
--- Text and byte encoding tools.
export let encoding := import "@std/encoding";
--- JSON tools.
export let json := import "@std/json";
--- Math tools.
export let math := import "@std/math";
--- C math library tools.
export let libm := import "@std/libm";
--- C runtime library tools.
export let libc := import "@std/libc";
--- Fixed-width bit-vector tools.
export let bits := import "@std/bits";
--- Machine word tools.
export let word := import "@std/word";
--- Formatting tools.
export let fmt := import "@std/fmt";
--- Error value tools.
export let errors := import "@std/errors";

--- Date and time tools.
export let datetime := import "@std/datetime";
--- Random number tools.
export let random := import "@std/random";
--- Cryptographic tools.
export let crypto := import "@std/crypto";
--- Filesystem tools.
export let fs := import "@std/fs";
--- Path tools.
export let path := import "@std/path";
--- Terminal I/O tools.
export let io := import "@std/io";
--- Environment variable tools.
export let env := import "@std/env";
--- Process tools.
export let process := import "@std/process";
--- Typed operating system target tools.
export let os := import "@std/os";
--- Logging tools.
export let log := import "@std/log";
--- Foreign function interface tools.
export let ffi := import "@std/ffi";

--- Command-line tools.
export let cli := import "@std/cli";
--- UUID tools.
export let uuid := import "@std/uuid";
--- Semantic version tools.
export let semver := import "@std/semver";

--- Assertion tools.
export let assert := import "@std/assert";
--- Testing tools.
export let testing := import "@std/testing";
