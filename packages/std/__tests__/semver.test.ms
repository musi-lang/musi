let semver := import "@std/semver";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("semver"),
    let one := semver.parse("1.0.0"),
    let two := semver.parse("2.0.0"),
    testing.it("validates version text", semver.isValid("1.2.3"),),
    testing.it("normalizes version text", one.toString() = "1.0.0",),
    testing.it("compares versions", testing.toBe(semver.compare(one, two), - 1)),
    testing.endDescribe(),
  );
