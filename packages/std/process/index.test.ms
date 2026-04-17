let testing := import "@std/testing";
let option := import "@std/option";
let process := import "./index.ms";
let path := import "@std/path";

export let test () :=
  (
    testing.describe("process/index");
    testing.it("'argsCount' returns non-negative count", testing.toBeTruthy(process.argsCount() >= 0));
    testing.it("'argAt' returns some for first arg", testing.toBeTruthy(option.isSome[String](process.argAt(0))));
    testing.it("'cwd' returns non-empty path", testing.toBeTruthy(path.toString(process.cwd()) /= ""));
    testing.it("'exitCode' exposes integer value", testing.toBe(process.toInt(process.exitCode(7)), 7));
    testing.endDescribe()
  );
