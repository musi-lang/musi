// std/prelude.ms — Musi standard prelude
//
// Automatically in scope for every Musi module.
// Contains primitive types, core sum types, and I/O intrinsics.

// ── Platform-sized numeric types ─────────────────────────────────────────────

#[intrinsic("int")]
export native opaque record Int {};

#[intrinsic("nat")]
export native opaque record Nat {};

#[intrinsic("float")]
export native opaque record Float {};

// ── Fixed-size signed integers ────────────────────────────────────────────────

#[intrinsic("int8")]
export native opaque record Int8 {};

#[intrinsic("int16")]
export native opaque record Int16 {};

#[intrinsic("int32")]
export native opaque record Int32 {};

#[intrinsic("int64")]
export native opaque record Int64 {};

// ── Fixed-size unsigned integers (natural numbers) ───────────────────────────

#[intrinsic("nat8")]
export native opaque record Nat8 {};

#[intrinsic("nat16")]
export native opaque record Nat16 {};

#[intrinsic("nat32")]
export native opaque record Nat32 {};

#[intrinsic("nat64")]
export native opaque record Nat64 {};

// ── Fixed-size floats ────────────────────────────────────────────────────────

#[intrinsic("float32")]
export native opaque record Float32 {};

#[intrinsic("float64")]
export native opaque record Float64 {};

// ── Text primitives ───────────────────────────────────────────────────────────

/// A Unicode scalar value (U+0000 to U+D7FF and U+E000 to U+10FFFF).
#[intrinsic("rune")]
export native opaque record Rune {};

#[intrinsic("string")]
export native opaque record String {};

// ── Unit ──────────────────────────────────────────────────────────────────────

#[intrinsic("unit")]
export native opaque record Unit {};

// ── Bool ─────────────────────────────────────────────────────────────────────
// Boolean type with explicit integer discriminants (False = 0, True = 1).
// Backed by the VM's native bool representation.

#[intrinsic("bool")]
export const Bool := choice { False := 0 | True := 1 };

// ── Option ───────────────────────────────────────────────────────────────────
// Generic option type. Use `None` for the absent case, `Some['T]` for a value.
// Type applications at use sites: Option[Int], Option[String], etc.
// Nullables in Musi are simply Option — there is no null pointer.

export choice Option['T] {
    None
  | Some('T)
};

// ── Expect ───────────────────────────────────────────────────────────────────
/// Explicit error handling type. Use Pass for success, Fail for errors.
/// Inspired by C++ std::expected. No exceptions — errors are values.
/// Pass/Fail follows the 4-letter convention of Some/None.

export choice Expect['T, 'E] {
    Fail('E)
  | Pass('T)
};

// ── I/O intrinsics ───────────────────────────────────────────────────────────
// Native functions wired directly to the VM's I/O subsystem.

#[intrinsic("writeln")] export native fn writeln(s: String): Unit;
#[intrinsic("write")]   export native fn write(s: String): Unit;
#[intrinsic("int_to_string")] export native fn int_to_string(n: Int): String;
