// std/semver.ms — Semantic Versioning (SemVer 2.0.0)
// import { SemVer, semver_parse, semver_compare, semver_satisfies } from "std/semver"

/// A semantic version: major.minor.patch[-prerelease][+build]
export record SemVer {
    major: Int,
    minor: Int,
    patch: Int,
    prerelease: String,
    build: String
};

/// Create a simple version (major.minor.patch).
export fn semver(major: Int, minor: Int, patch: Int): SemVer =>
    SemVer.{ major := major, minor := minor, patch := patch, prerelease := "", build := "" };

/// Create a version with prerelease tag.
export fn semver_pre(major: Int, minor: Int, patch: Int, prerelease: String): SemVer =>
    SemVer.{ major := major, minor := minor, patch := patch, prerelease := prerelease, build := "" };

/// Format a SemVer as a string.
export fn semver_to_string(v: SemVer): String => (
    var result := string_concat(int_to_string(v.major), string_concat(".", string_concat(
        int_to_string(v.minor), string_concat(".", int_to_string(v.patch)))));
    if string_length(v.prerelease) > 0 then result <- string_concat(result, string_concat("-", v.prerelease));
    if string_length(v.build) > 0 then result <- string_concat(result, string_concat("+", v.build));
    result
);

/// Parse a SemVer string. Returns `.None` on invalid input.
export fn semver_parse(s: String): Option[SemVer] => (
    const input := string_trim(s);
    // Remove leading 'v' if present
    const ver := if string_length(input) > 0 and string_slice(input, 0, 1) = "v"
                 then string_slice(input, 1, string_length(input))
                 else input;

    // Split off build metadata first
    const build_parts := string_split(ver, "+");
    const build := if array_length(build_parts) > 1 then array_get(build_parts, 1) else "";
    const without_build := array_get(build_parts, 0);

    // Split off prerelease
    const pre_parts := string_split(without_build, "-");
    const prerelease := if array_length(pre_parts) > 1 then array_get(pre_parts, 1) else "";
    const core := array_get(pre_parts, 0);

    // Parse major.minor.patch
    const parts := string_split(core, ".");
    if array_length(parts) /= 3 then .None
    else match string_to_int(array_get(parts, 0)) with (
        .None => .None
      | .Some(major) => match string_to_int(array_get(parts, 1)) with (
            .None => .None
          | .Some(minor) => match string_to_int(array_get(parts, 2)) with (
                .None => .None
              | .Some(patch) => .Some(SemVer.{
                    major := major,
                    minor := minor,
                    patch := patch,
                    prerelease := prerelease,
                    build := build
                })
            )
        )
    )
);

/// Compare two SemVers. Returns -1 if a < b, 0 if equal, 1 if a > b.
export fn semver_compare(a: SemVer, b: SemVer): Int => (
    if a.major /= b.major then (if a.major < b.major then 0 - 1 else 1)
    else if a.minor /= b.minor then (if a.minor < b.minor then 0 - 1 else 1)
    else if a.patch /= b.patch then (if a.patch < b.patch then 0 - 1 else 1)
    else compare_prerelease(a.prerelease, b.prerelease)
);

fn compare_prerelease(a: String, b: String): Int => (
    // No prerelease > prerelease (1.0.0 > 1.0.0-alpha)
    const a_empty := string_length(a) = 0;
    const b_empty := string_length(b) = 0;
    if a_empty and b_empty then 0
    else if a_empty then 1
    else if b_empty then 0 - 1
    else string_compare(a, b)
);

fn string_compare(a: String, b: String): Int => (
    if a = b then 0
    else if a < b then 0 - 1
    else 1
);

/// Returns `.True` if `a` is less than `b`.
export fn semver_lt(a: SemVer, b: SemVer): Bool => semver_compare(a, b) < 0;

/// Returns `.True` if `a` is less than or equal to `b`.
export fn semver_lte(a: SemVer, b: SemVer): Bool => semver_compare(a, b) <= 0;

/// Returns `.True` if `a` is greater than `b`.
export fn semver_gt(a: SemVer, b: SemVer): Bool => semver_compare(a, b) > 0;

/// Returns `.True` if `a` is greater than or equal to `b`.
export fn semver_gte(a: SemVer, b: SemVer): Bool => semver_compare(a, b) >= 0;

/// Returns `.True` if `a` equals `b` (ignoring build metadata).
export fn semver_eq(a: SemVer, b: SemVer): Bool => semver_compare(a, b) = 0;

/// Increment the major version (resets minor and patch to 0).
export fn semver_inc_major(v: SemVer): SemVer =>
    SemVer.{ major := v.major + 1, minor := 0, patch := 0, prerelease := "", build := "" };

/// Increment the minor version (resets patch to 0).
export fn semver_inc_minor(v: SemVer): SemVer =>
    SemVer.{ major := v.major, minor := v.minor + 1, patch := 0, prerelease := "", build := "" };

/// Increment the patch version.
export fn semver_inc_patch(v: SemVer): SemVer =>
    SemVer.{ major := v.major, minor := v.minor, patch := v.patch + 1, prerelease := "", build := "" };

/// Check if version is a prerelease.
export fn semver_is_prerelease(v: SemVer): Bool => string_length(v.prerelease) > 0;

/// Check if version satisfies a simple range (^major.minor.patch).
/// Caret range: allows changes that do not modify the left-most non-zero digit.
export fn semver_satisfies_caret(v: SemVer, range: SemVer): Bool => (
    if v.major /= range.major then .False
    else if range.major = 0 then (
        if range.minor = 0 then v.minor = 0 and v.patch >= range.patch
        else v.minor = range.minor and v.patch >= range.patch
    )
    else semver_gte(v, range)
);

/// Check if version satisfies a tilde range (~major.minor.patch).
/// Tilde range: allows patch-level changes.
export fn semver_satisfies_tilde(v: SemVer, range: SemVer): Bool =>
    v.major = range.major and v.minor = range.minor and v.patch >= range.patch;
