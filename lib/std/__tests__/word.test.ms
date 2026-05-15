let assert := import "@std/assert";
let testing := import "@std/testing";
let word := import "@std/word";

export let test () :=
  (
    testing.describe("word");
    testing.it("width reports target word size", word.width > 0);
    testing.it(
      "fromNat32 converts natural values",
      assert.toBe(word.toInt32(word.fromNat32(10)), 10),
    );
    testing.it(
      "fromInt32 converts integer values",
      assert.toBe(word.toInt32(word.fromInt32(7)), 7),
    );
    testing.it(
      "logicalAnd32 combines pointwise",
      assert.toBe(
        word.toInt32(word.logicalAnd32(word.fromInt32(10), word.fromInt32(12))),
        8,
      ),
    );
    testing.it(
      "equals32 detects equal words",
      word.equals32(word.zero32(), word.zero32()),
    );
    testing.endDescribe()
  );
