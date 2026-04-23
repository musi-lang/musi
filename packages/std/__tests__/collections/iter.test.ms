let iter := import "@std/collections/iter";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("iter");
    testing.it(
      "'once' creates single-value iterator",
      iter.equalsInt(iter.once[Int](4), [4]),
    );
    testing.it(
      "'concat' appends iterator values",
      iter.equalsInt(iter.concat[Int]([1, 2], [3]), [1, 2, 3]),
    );
    testing.it(
      "'collect' returns array copy",
      iter.equalsInt(iter.collect[Int]([8, 9]), [8, 9]),
    );
    testing.it("'isEmpty' detects empty iter", iter.isEmpty[Int]([]),);
    testing.it("'nonEmpty' detects populated iter", iter.nonEmpty[Int]([4]),);
    testing.endDescribe()
  );
