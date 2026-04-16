let testing := import "@std/testing";
let list := import "./index.ms";

export let test () :=
  (
    testing.describe("list");
    testing.it("'of' creates single-value list", testing.toBeTruthy(list.equalsInt(list.of[Int](4), [4])));
    testing.it("'concat' joins lists", testing.toBeTruthy(list.equalsInt(list.concat[Int]([1, 2], [3]), [1, 2, 3])));
    testing.it("'append' adds trailing value", testing.toBeTruthy(list.equalsInt(list.append[Int]([1, 2], 3), [1, 2, 3])));
    testing.it("'prepend' adds leading value", testing.toBeTruthy(list.equalsInt(list.prepend[Int](0, [1, 2]), [0, 1, 2])));
    testing.it("'isEmpty' detects empty lists", testing.toBeTruthy(list.isEmpty[Int]([])));
    testing.it("'nonEmpty' detects populated lists", testing.toBeTruthy(list.nonEmpty[Int]([4])));
    testing.endDescribe()
  );
