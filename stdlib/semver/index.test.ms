//! Tests for semantic versioning parse, compare, and bump operations.

import "@std/testing" as t;
import "@std/semver/index" as semver;

t.runner.report(t.runner.run_suite([
  ("parse - valid version", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => t.assertions.assert_true(v.major = 1 and v.minor = 2 and v.patch = 3)
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("parse - version with pre-release", () => (
    match semver.parse("1.0.0-alpha") (
      .Ok(v) => t.assertions.assert_eq(v.pre, "alpha")
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("parse - version with build metadata", () => (
    match semver.parse("1.0.0+build.1") (
      .Ok(v) => t.assertions.assert_eq(v.build, "build.1")
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("parse - version with pre-release and build", () => (
    match semver.parse("1.0.0-beta+exp.sha") (
      .Ok(v) => t.assertions.assert_true(v.pre = "beta" and v.build = "exp.sha")
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("parse - invalid format returns Err", () => (
    match semver.parse("1.2") (
      .Err(.InvalidFormat(_)) => t.assertions.assert_true(.True)
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("to_string - reconstructs basic version", () => (
    match semver.parse("2.3.4") (
      .Ok(v) => t.assertions.assert_eq(semver.to_string(v), "2.3.4")
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("to_string - includes pre-release", () => (
    match semver.parse("1.0.0-rc.1") (
      .Ok(v) => t.assertions.assert_eq(semver.to_string(v), "1.0.0-rc.1")
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("compare - equal versions", () => (
    let a := semver.parse("1.2.3");
    let b := semver.parse("1.2.3");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.assertions.assert_eq(semver.compare(av, bv), .Equal)
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("compare - major version difference", () => (
    let a := semver.parse("2.0.0");
    let b := semver.parse("1.0.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.assertions.assert_eq(semver.compare(av, bv), .Greater)
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("compare - pre-release is lower precedence than release", () => (
    let a := semver.parse("1.0.0");
    let b := semver.parse("1.0.0-alpha");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.assertions.assert_eq(semver.compare(av, bv), .Greater)
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("inc_major - resets minor and patch", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_major(v);
        t.assertions.assert_true(bumped.major = 2 and bumped.minor = 0 and bumped.patch = 0)
      )
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("inc_minor - resets patch, preserves major", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_minor(v);
        t.assertions.assert_true(bumped.major = 1 and bumped.minor = 3 and bumped.patch = 0)
      )
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("inc_patch - increments patch only", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_patch(v);
        t.assertions.assert_true(bumped.major = 1 and bumped.minor = 2 and bumped.patch = 4)
      )
      | .Err(_) => t.assertions.assert_false(.True)
    )
  )),
  ("is_compatible - same major is compatible", () => (
    let a := semver.parse("1.2.3");
    let b := semver.parse("1.5.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.assertions.assert_true(semver.is_compatible(av, bv))
      | _ => t.assertions.assert_false(.True)
    )
  )),
  ("is_compatible - different major is not compatible", () => (
    let a := semver.parse("1.0.0");
    let b := semver.parse("2.0.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.assertions.assert_false(semver.is_compatible(av, bv))
      | _ => t.assertions.assert_false(.True)
    )
  )),
]));
