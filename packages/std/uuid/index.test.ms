let uuid := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("uuid"),
    let text := "550e8400-e29b-41d4-a716-446655440000",
    testing.it("validates uuid text", testing.toBeTruthy(uuid.isValid(text))),
    testing.it(
      "parses canonical uuid",
      testing.toBeTruthy(uuid.parse(text).toString() = text),
    ),
    testing.it(
      "generates v4 uuid",
      testing.toBeTruthy(uuid.isValid(uuid.v4().toString())),
    ),
    testing.endDescribe(),
  );
