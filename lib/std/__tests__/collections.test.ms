let collections := import "@std/collections";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("collections"),
    testing.it(
      "array namespace works",
      collections.array.equalsInt([1], collections.array.single[Int](1)),
    ),
    testing.it(
      "list namespace works",
      collections.list.of[Int](2).equalsInt(
        collections.list.fromArray[Int]([2])
      ),
    ),
    testing.endDescribe(),
  );
