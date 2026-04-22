let errors := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("errors"),
    let value := errors.error("failed"),
    testing.it(
      "error stores message",
      testing.toBeTruthy(value.message() = "failed"),
    ),
    testing.it(
      "error prints message",
      testing.toBeTruthy(value.toString() = "failed"),
    ),
    testing.endDescribe(),
  );
