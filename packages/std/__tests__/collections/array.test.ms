let array := import "@std/collections/array";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("array");
    testing.it(
      "'copy' clones sequence values",
      array.equalsInt(array.copy[Int]([1, 2, 3]), [1, 2, 3]),
    );
    testing.it(
      "'concat' joins two sequences",
      array.equalsInt(array.concat[Int]([1, 2], [3, 4]), [1, 2, 3, 4]),
    );
    testing.it(
      "'append' adds trailing value",
      array.equalsInt(array.append[Int]([1, 2], 3), [1, 2, 3]),
    );
    testing.it(
      "'prepend' adds leading value",
      array.equalsInt(array.prepend[Int](0, [1, 2]), [0, 1, 2]),
    );
    testing.it("'isEmpty' detects empty arrays", array.isEmpty[Int]([]),);
    testing.it("'nonEmpty' detects non-empty arrays", array.nonEmpty[Int]([1]),);
    testing.endDescribe()
  );
