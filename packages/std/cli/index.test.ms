let cli := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cli"),
    testing.it(
      "process namespace exists",
      testing.toBeTruthy(cli.process.argsCount() >= 0),
    ),
    testing.it(
      "env namespace exists",
      testing.toBeTruthy(cli.env.key("PATH").toString() = "PATH"),
    ),
    testing.endDescribe(),
  );
