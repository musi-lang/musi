let cli := import "@std/cli";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cli"),
    testing.it("prompt module import resolves", 0 = 0,),
    testing.endDescribe(),
  );
