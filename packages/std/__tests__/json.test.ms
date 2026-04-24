let json := import "@std/json";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("json");
    testing.it(
      "'isValid' rejects invalid json",
      testing.toBeFalse(json.isValid("{")),
    );
    testing.it(
      "'stringify' keeps normalized source",
      json.parse("{\"x\":1}").stringify() = "{\"x\":1}",
    );
    testing.it("'isValid' detects valid source", json.isValid("{}"),);
    testing.endDescribe()
  );
