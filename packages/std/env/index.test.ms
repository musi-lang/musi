let env := import "./index.ms";
let option := import "@std/option";
let testing := import "@std/testing";
let Var := env.Var;

let tempKey := env.key("__MUSI_STD_ENV_TEST__");

export let test () :=
  (
    testing.describe("env/index");
    testing.it("missing key returns none",
      testing.toBeTruthy(env.get(tempKey).isNone())
    );
    testing.it("'set' stores value",
      testing.toBeTruthy(env.set(tempKey, env.var("1")))
    );
    testing.it("'set' value can be read",
      testing.toBeTruthy(env.get(tempKey).isSome())
    );
    testing.it("'remove' deletes value",
      testing.toBeTruthy(env.remove(tempKey))
    );
    testing.it("removed key returns none",
      testing.toBeTruthy(env.get(tempKey).isNone())
    );
    testing.endDescribe()
  );
