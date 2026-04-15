let Testing := import "@std/testing";
let Iter := import "./index.ms";

export let test () :=
  (
    Testing.describe("iter");
    Testing.it("'once' creates single-value iterator", Testing.toBeTruthy(Iter.equalsInt(Iter.once[Int](4), [4])));
    Testing.it("'concat' appends iterator values", Testing.toBeTruthy(Iter.equalsInt(Iter.concat[Int]([1, 2], [3]), [1, 2, 3])));
    Testing.it("'collect' returns array copy", Testing.toBeTruthy(Iter.equalsInt(Iter.collect[Int]([8, 9]), [8, 9])));
    Testing.it("'isEmpty' detects empty iter", Testing.toBeTruthy(Iter.isEmpty[Int]([])));
    Testing.it("'nonEmpty' detects populated iter", Testing.toBeTruthy(Iter.nonEmpty[Int]([4])));
    Testing.endDescribe()
  );
