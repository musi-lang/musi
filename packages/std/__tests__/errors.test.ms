let errors := import "@std/errors";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("errors"),
    let value := errors.error("failed"),
    testing.it("error stores message", value.message() = "failed",),
    testing.it("error prints message", value.toString() = "failed",),
    testing.endDescribe(),
  );
