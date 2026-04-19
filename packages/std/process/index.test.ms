let process := import "./index.ms";
let option := import "@std/option";
let path := import "@std/path";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("process/index");
    testing.it(
      "'argsCount' returns non-negative count",
      testing.toBeTruthy(process.argsCount() >= 0),
    );
    testing.it(
      "'argAt' returns some for first arg",
      testing.toBeTruthy(process.argAt(0).isSome()),
    );
    testing.it(
      "'cwd' returns non-empty path",
      testing.toBeTruthy(process.cwd().toString() /= ""),
    );
    testing.it(
      "'exitCode' exposes integer value",
      testing.toBe(process.exitCode(7).toInt(), 7),
    );
    testing.endDescribe()
  );
