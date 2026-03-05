// std/prelude.ms — standard definitions always in scope

// Platform-sized numerics
#[intrinsic("int")]   export native opaque record Int {};
#[intrinsic("nat")]   export native opaque record Nat {};
#[intrinsic("float")] export native opaque record Float {};

// Fixed-size signed integers
#[intrinsic("int8")]  export native opaque record Int8 {};
#[intrinsic("int16")] export native opaque record Int16 {};
#[intrinsic("int32")] export native opaque record Int32 {};
#[intrinsic("int64")] export native opaque record Int64 {};

// Fixed-size unsigned integers
#[intrinsic("nat8")]  export native opaque record Nat8 {};
#[intrinsic("nat16")] export native opaque record Nat16 {};
#[intrinsic("nat32")] export native opaque record Nat32 {};
#[intrinsic("nat64")] export native opaque record Nat64 {};

// Fixed-size floats
#[intrinsic("float32")] export native opaque record Float32 {};
#[intrinsic("float64")] export native opaque record Float64 {};

// Text primitives
#[intrinsic("rune")]   export native opaque record Rune {};   // Unicode scalar value
#[intrinsic("string")] export native opaque record String {};

// Unit type (single value)
#[intrinsic("unit")] export native opaque record Unit {};

// Boolean type, matches VM native representation
#[intrinsic("bool")]
export const Bool := choice { False := 0 | True := 1 };

// Option type: None or Some(value); no null-pointers
export choice Option['T] {
    None
  | Some('T)
};

// Expect type: Pass(success) or Fail(error), for explicit error handling
export choice Expect['T, 'E] {
    Fail('E)
  | Pass('T)
};

// Basic I/O intrinsics
#[intrinsic("writeln")]       export native fn writeln(s: String): Unit;
#[intrinsic("write")]         export native fn write(s: String): Unit;
#[intrinsic("int_to_string")] export native fn int_to_string(n: Int): String;
