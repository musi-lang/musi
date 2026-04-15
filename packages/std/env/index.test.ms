let Testing := import "@std/testing";
let Option := import "@std/option";
let Env := import "./index.ms";

let tempKey := "__MUSI_STD_ENV_TEST__";

export let test () :=
  (
    Testing.describe("env/index");
    Testing.it("missing key returns none", Testing.toBeTruthy(Option.isNone[String](Env.get(tempKey))));
    Testing.it("set stores value", Testing.toBeTruthy(Env.set(tempKey, "1")));
    Testing.it("set value can be read", Testing.toBeTruthy(Option.isSome[String](Env.get(tempKey))));
    Testing.it("remove deletes value", Testing.toBeTruthy(Env.remove(tempKey)));
    Testing.it("removed key returns none", Testing.toBeTruthy(Option.isNone[String](Env.get(tempKey))));
    Testing.endDescribe()
  );
