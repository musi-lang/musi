let slice := import "@std/collections/slice";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("slice");
    testing.it(
      "'copy' keeps values",
      slice.equalsInt(slice.copy[Int]([7, 8]), [7, 8]),
    );
    testing.it(
      "'concat' joins slices",
      slice.equalsInt(slice.concat[Int]([1], [2, 3]), [1, 2, 3]),
    );
    testing.it("'isEmpty' detects empty slice", slice.isEmpty[Int]([]),);
    testing.it("'nonEmpty' detects non-empty slice", slice.nonEmpty[Int]([1]),);
    testing.endDescribe()
  );
