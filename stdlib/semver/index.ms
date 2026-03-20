//! Semantic versioning - parse, format, compare, and bump version strings.

import "@std/rt" as rt;
import "@std/result/result" as res;

/// A parsed semantic version with major, minor, patch, pre-release, and build metadata.
export let Semver := record {
  major : Int;
  minor : Int;
  patch : Int;
  pre   : String;
  build : String;
};

/// Error returned when a version string cannot be parsed.
export let ParseError := choice { InvalidFormat of String };

/// Parses a semver string (e.g. "1.2.3-alpha+build") into a Semver record.
export let parse := (s : String) : res.Result of (Semver, ParseError) => (
  let plus_idx := rt.str_index_of(s, "+");
  let build := (
    "" if plus_idx = -1
    | rt.str_slice(s, plus_idx + 1, rt.str_len(s)) if _
  );
  let core_pre := (
    s if plus_idx = -1
    | rt.str_slice(s, 0, plus_idx) if _
  );
  let dash_idx := rt.str_index_of(core_pre, "-");
  let pre := (
    "" if dash_idx = -1
    | rt.str_slice(core_pre, dash_idx + 1, rt.str_len(core_pre)) if _
  );
  let core_str := (
    core_pre if dash_idx = -1
    | rt.str_slice(core_pre, 0, dash_idx) if _
  );
  let parts := rt.str_split(core_str, ".");
  let part_count := rt.arr_len(parts);
  (
    .Err(.InvalidFormat(f"expected major.minor.patch, got `{core_str}`")) if part_count /= 3
    | (
      let major_str := parts.[0];
      let minor_str := parts.[1];
      let patch_str := parts.[2];
      (
        .Err(.InvalidFormat("major component is empty")) if rt.str_len(major_str) = 0
        | .Err(.InvalidFormat("minor component is empty")) if rt.str_len(minor_str) = 0
        | .Err(.InvalidFormat("patch component is empty")) if rt.str_len(patch_str) = 0
        | .Ok(.{
            major := rt.str_parse_int(major_str),
            minor := rt.str_parse_int(minor_str),
            patch := rt.str_parse_int(patch_str),
            pre   := pre,
            build := build,
          }) if _
      )
    ) if _
  )
);

/// Formats a Semver record back to its canonical string representation.
export let to_string := (v : Semver) : String => (
  let base := f"{v.major}.{v.minor}.{v.patch}";
  let with_pre := (
    base if rt.str_len(v.pre) = 0
    | f"{base}-{v.pre}" if _
  );
  (
    with_pre if rt.str_len(v.build) = 0
    | f"{with_pre}+{v.build}" if _
  )
);

let Ordering := choice { Less + Equal + Greater };

let compare_int := (a : Int, b : Int) => (
  .Less    if a < b
  | .Greater if a > b
  | .Equal   if _
);

let compare_pre_result := (a, b) => (
  .Equal   if a = b
  | .Greater if rt.str_len(a) = 0
  | .Less    if rt.str_len(b) = 0
  | (.Less if a < b | .Greater if _) if _
);

/// Compares two Semver values, returning .Less, .Equal, or .Greater.
export let compare := (a : Semver, b : Semver) : Ordering => (
  let maj : Ordering := compare_int(a.major, b.major);
  match maj (
    .Equal => (
      let min : Ordering := compare_int(a.minor, b.minor);
      match min (
        .Equal => (
          let pat : Ordering := compare_int(a.patch, b.patch);
          match pat (
            .Equal => compare_pre_result(a.pre, b.pre)
            | _ => pat
          )
        )
        | _ => min
      )
    )
    | _ => maj
  )
);

/// Returns .True if a and b are compatible under semver rules.
export let is_compatible := (a : Semver, b : Semver) => (
  (a.major = b.major and a.minor = b.minor) if a.major = 0
  | a.major = b.major if _
);

/// Returns v with the major version incremented and minor/patch reset to zero.
export let inc_major := (v : Semver) => .{
  major := v.major + 1,
  minor := 0,
  patch := 0,
  pre   := "",
  build := "",
};

/// Returns v with the minor version incremented and patch reset to zero.
export let inc_minor := (v : Semver) => .{
  major := v.major,
  minor := v.minor + 1,
  patch := 0,
  pre   := "",
  build := "",
};

/// Returns v with the patch version incremented.
export let inc_patch := (v : Semver) => .{
  major := v.major,
  minor := v.minor,
  patch := v.patch + 1,
  pre   := "",
  build := "",
};
