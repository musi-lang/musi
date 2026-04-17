let testing := import "@std/testing";
let array := import "./index.ms";

export let test () :=
  (
    testing.describe("array");
    testing.it("'copy' clones sequence values", testing.toBeTruthy(array.equalsInt(array.copy[Int]([1, 2, 3]), [1, 2, 3])));
    testing.it("'concat' joins two sequences", testing.toBeTruthy(array.equalsInt(array.concat[Int]([1, 2], [3, 4]), [1, 2, 3, 4])));
    testing.it("'append' adds trailing value", testing.toBeTruthy(array.equalsInt(array.append[Int]([1, 2], 3), [1, 2, 3])));
    testing.it("'prepend' adds leading value", testing.toBeTruthy(array.equalsInt(array.prepend[Int](0, [1, 2]), [0, 1, 2])));
    testing.it("'isEmpty' detects empty arrays", testing.toBeTruthy(array.isEmpty[Int]([])));
    testing.it("'nonEmpty' detects non-empty arrays", testing.toBeTruthy(array.nonEmpty[Int]([1])));
    testing.endDescribe()
  );
