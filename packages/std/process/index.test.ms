let Testing := import "@std/testing";
let Option := import "@std/option";
let Process := import "./index.ms";

export let test () :=
  (
    Testing.describe("process/index");
    Testing.it("'argsCount' returns non-negative count", Testing.toBeTruthy(Process.argsCount() >= 0));
    Testing.it("'argAt' returns some for first arg", Testing.toBeTruthy(Option.isSome[String](Process.argAt(0))));
    Testing.it("'cwd' returns non-empty path", Testing.toBeTruthy(Process.cwd() /= ""));
    Testing.endDescribe()
  );
