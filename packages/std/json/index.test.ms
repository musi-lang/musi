let testing := import "@std/testing";
let json := import "./index.ms";

export let test () :=
  (
    testing.describe("json/index");
    testing.it("'isValid' rejects invalid json", testing.toBeFalsy(json.isValid("{")));
    testing.it("'stringify' keeps normalized source", testing.toBeTruthy(json.stringify(json.parse("{\"x\":1}")) = "{\"x\":1}"));
    testing.it("'isValid' detects valid source", testing.toBeTruthy(json.isValid("{}")));
    testing.endDescribe()
  );
