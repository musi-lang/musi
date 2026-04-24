let testing := import "@std/testing";
let uuid := import "@std/uuid";

export let test () :=
  (
    testing.describe("uuid"),
    let text := "550e8400-e29b-41d4-a716-446655440000",
    testing.it("validates uuid text", uuid.isValid(text)),
    testing.it("parses canonical uuid", uuid.parse(text).toString() = text,),
    testing.it("generates v4 uuid", uuid.isValid(uuid.v4().toString()),),
    testing.endDescribe(),
  );
