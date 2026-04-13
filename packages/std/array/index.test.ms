let Testing := import "@std/testing";
let Array := import "./index.ms";

export let test () :=
  (
    Testing.describe("array");
    Testing.it("'copy' clones sequence values", Testing.toBeTruthy(Array.equalsInt(Array.copy[Int]([1, 2, 3]), [1, 2, 3])));
    Testing.it("'concat' joins two sequences", Testing.toBeTruthy(Array.equalsInt(Array.concat[Int]([1, 2], [3, 4]), [1, 2, 3, 4])));
    Testing.it("'append' adds trailing value", Testing.toBeTruthy(Array.equalsInt(Array.append[Int]([1, 2], 3), [1, 2, 3])));
    Testing.it("'prepend' adds leading value", Testing.toBeTruthy(Array.equalsInt(Array.prepend[Int](0, [1, 2]), [0, 1, 2])));
    Testing.it("'isEmpty' detects empty arrays", Testing.toBeTruthy(Array.isEmpty[Int]([])));
    Testing.it("'nonEmpty' detects non-empty arrays", Testing.toBeTruthy(Array.nonEmpty[Int]([1])));
    Testing.endDescribe()
  );
