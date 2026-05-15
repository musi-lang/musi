let assert := import "@std/assert";
let maybe := import "@std/maybe";
let path := import "@std/path";
let process := import "@std/process";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("process");
    testing.it(
      "'argsCount' returns non-negative count",
      process.argsCount() >= 0,
    );
    testing.it("'argAt' returns Some for first arg", process.argAt(0).isSome(),);
    testing.it("'cwd' returns non-empty path", process.cwd().toString() /= "",);
    testing.it(
      "'exitCode' exposes integer value",
      assert.toBe(process.exitCode(7).toInt(), 7),
    );
    testing.endDescribe()
  );
