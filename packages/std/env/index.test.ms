let Testing := import "@std/testing";
let Option := import "@std/option";
let Env := import "./index.ms";

let tempKey := "__MUSI_STD_MISSING_ENV__";

export let test () :=
  (
    Testing.describe("env/index");
    Testing.it("missing key returns none", Testing.toBeTruthy(Option.isNone[String](Env.get(tempKey))));
    Testing.it("set unsupported without unsafe host mutation", Testing.toBeFalsy(Env.set(tempKey, "1")));
    Testing.it("remove unsupported without unsafe host mutation", Testing.toBeFalsy(Env.remove(tempKey)));
    Testing.endDescribe()
  );
