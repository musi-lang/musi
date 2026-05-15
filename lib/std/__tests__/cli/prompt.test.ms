let prompt := import "@std/cli/prompt";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("cli/prompt");
    testing.it("prompt helpers module imports without evaluating stdin", 0 = 0,);
    testing.endDescribe()
  );
