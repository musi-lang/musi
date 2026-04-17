let testing := import "@std/testing";
let iter := import "./index.ms";

export let test () :=
  (
    testing.describe("iter");
    testing.it("'once' creates single-value iterator", testing.toBeTruthy(iter.equalsInt(iter.once[Int](4), [4])));
    testing.it("'concat' appends iterator values", testing.toBeTruthy(iter.equalsInt(iter.concat[Int]([1, 2], [3]), [1, 2, 3])));
    testing.it("'collect' returns array copy", testing.toBeTruthy(iter.equalsInt(iter.collect[Int]([8, 9]), [8, 9])));
    testing.it("'isEmpty' detects empty iter", testing.toBeTruthy(iter.isEmpty[Int]([])));
    testing.it("'nonEmpty' detects populated iter", testing.toBeTruthy(iter.nonEmpty[Int]([4])));
    testing.endDescribe()
  );
