let Testing := import "@std/testing";
let Slice := import "./index.ms";

export let test () :=
  (
    Testing.describe("slice");
    Testing.it("'copy' keeps values", Testing.toBeTruthy(Slice.equalsInt(Slice.copy[Int]([7, 8]), [7, 8])));
    Testing.it("'concat' joins slices", Testing.toBeTruthy(Slice.equalsInt(Slice.concat[Int]([1], [2, 3]), [1, 2, 3])));
    Testing.it("'isEmpty' detects empty slice", Testing.toBeTruthy(Slice.isEmpty[Int]([])));
    Testing.it("'nonEmpty' detects non-empty slice", Testing.toBeTruthy(Slice.nonEmpty[Int]([1])));
    Testing.endDescribe()
  );
