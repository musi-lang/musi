//! Tests for semantic versioning parse, compare, and bump operations.

import "@std/testing" as t;
import "@std/semver/index" as semver;

t.describe("Semver", () => (
  t.it("parse - valid version", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => t.expect(v.major = 1 and v.minor = 2 and v.patch = 3).toBe(.True)
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("parse - version with pre-release", () => (
    match semver.parse("1.0.0-alpha") (
      .Ok(v) => t.expect(v.pre).toBe("alpha")
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("parse - version with build metadata", () => (
    match semver.parse("1.0.0+build.1") (
      .Ok(v) => t.expect(v.build).toBe("build.1")
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("parse - version with pre-release and build", () => (
    match semver.parse("1.0.0-beta+exp.sha") (
      .Ok(v) => t.expect(v.pre = "beta" and v.build = "exp.sha").toBe(.True)
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("parse - invalid format returns Err", () => (
    match semver.parse("1.2") (
      .Err(.InvalidFormat(_)) => t.expect(.True).toBe(.True)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("to_string - reconstructs basic version", () => (
    match semver.parse("2.3.4") (
      .Ok(v) => t.expect(semver.to_string(v)).toBe("2.3.4")
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("to_string - includes pre-release", () => (
    match semver.parse("1.0.0-rc.1") (
      .Ok(v) => t.expect(semver.to_string(v)).toBe("1.0.0-rc.1")
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("compare - equal versions", () => (
    let a := semver.parse("1.2.3");
    let b := semver.parse("1.2.3");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.expect(semver.compare(av, bv)).toBe(.Equal)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("compare - major version difference", () => (
    let a := semver.parse("2.0.0");
    let b := semver.parse("1.0.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.expect(semver.compare(av, bv)).toBe(.Greater)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("compare - pre-release is lower precedence than release", () => (
    let a := semver.parse("1.0.0");
    let b := semver.parse("1.0.0-alpha");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.expect(semver.compare(av, bv)).toBe(.Greater)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("inc_major - resets minor and patch", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_major(v);
        t.expect(bumped.major = 2 and bumped.minor = 0 and bumped.patch = 0).toBe(.True)
      )
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("inc_minor - resets patch, preserves major", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_minor(v);
        t.expect(bumped.major = 1 and bumped.minor = 3 and bumped.patch = 0).toBe(.True)
      )
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("inc_patch - increments patch only", () => (
    match semver.parse("1.2.3") (
      .Ok(v) => (
        let bumped := semver.inc_patch(v);
        t.expect(bumped.major = 1 and bumped.minor = 2 and bumped.patch = 4).toBe(.True)
      )
      | .Err(_) => t.expect(.True).toBe(.False)
    )
  ));
  t.it("is_compatible - same major is compatible", () => (
    let a := semver.parse("1.2.3");
    let b := semver.parse("1.5.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.expect(semver.is_compatible(av, bv)).toBe(.True)
      | _ => t.expect(.True).toBe(.False)
    )
  ));
  t.it("is_compatible - different major is not compatible", () => (
    let a := semver.parse("1.0.0");
    let b := semver.parse("2.0.0");
    match (a, b) (
      (.Ok(av), .Ok(bv)) => t.expect(semver.is_compatible(av, bv)).toBe(.False)
      | _ => t.expect(.True).toBe(.False)
    )
  ))
));

t.run();
