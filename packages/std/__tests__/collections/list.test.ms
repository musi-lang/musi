let list := import "@std/collections/list";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("list");
    let values := list.fromArray[Int]([1, 2]);
    testing.it(
      "'of' creates single-value list",
      list.equalsInt(list.of[Int](4), list.fromArray[Int]([4])),
    );
    testing.it(
      "'concat' joins lists",
      list.equalsInt(
        list.concat[Int](values, list.of[Int](3)),
        list.fromArray[Int]([1, 2, 3]),
      ),
    );
    testing.it(
      "'append' adds trailing value",
      list.equalsInt(
        list.append[Int](values, 3),
        list.fromArray[Int]([1, 2, 3]),
      ),
    );
    testing.it(
      "'prepend' adds leading value",
      list.equalsInt(
        list.prepend[Int](values, 0),
        list.fromArray[Int]([0, 1, 2]),
      ),
    );
    testing.it(
      "'isEmpty' detects empty lists",
      list.isEmpty[Int](list.empty[Int]()),
    );
    testing.it(
      "'nonEmpty' detects populated lists",
      list.nonEmpty[Int](list.of[Int](4)),
    );
    testing.endDescribe()
  );
