let Testing := import "@std/testing";
let Json := import "./index.ms";

export let test () :=
  (
    Testing.describe("json/index");
    Testing.it("'isValid' rejects invalid json", Testing.toBeFalsy(Json.isValid("{")));
    Testing.it("'stringify' keeps normalized source", Testing.toBeTruthy(Json.stringify(Json.fromString("{\"x\":1}")) = "{\"x\":1}"));
    Testing.it("'isValid' detects valid source", Testing.toBeTruthy(Json.isValid("{}")));
    Testing.endDescribe()
  );
