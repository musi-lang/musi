let cli := import "@std/cli";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cli"),
    testing.it("process namespace exists", cli.process.argsCount() >= 0,),
    testing.it("env namespace exists", cli.env.key("PATH").toString() = "PATH",),
    testing.endDescribe(),
  );
