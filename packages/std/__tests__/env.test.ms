let env := import "@std/env";
let option := import "@std/option";
let testing := import "@std/testing";
let Var := env.Var;

let tempKey := env.key("__MUSI_STD_ENV_TEST__");

export let test () :=
  (
    testing.describe("env");
    testing.it("missing key returns none", env.get(tempKey).isNone(),);
    testing.it("'set' stores value", env.set(tempKey, env.var("1")),);
    testing.it("'set' value can be read", env.get(tempKey).isSome(),);
    testing.it("'remove' deletes value", env.remove(tempKey),);
    testing.it("removed key returns none", env.get(tempKey).isNone(),);
    testing.endDescribe()
  );
