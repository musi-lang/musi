let collections := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("collections"),
    testing.it(
      "array namespace works",
      testing.toBeTruthy(
        collections.array.equalsInt([1], collections.array.single[Int](1))
      ),
    ),
    testing.it(
      "list namespace works",
      testing.toBeTruthy(
        collections.list.of[Int](2).equalsInt(
          collections.list.fromArray[Int]([2])
        )
      ),
    ),
    testing.it(
      "slice namespace works",
      testing.toBeTruthy(
        collections.slice.equalsInt([3], collections.slice.copy[Int]([3]))
      ),
    ),
    testing.it(
      "iter namespace works",
      testing.toBeTruthy(
        collections.iter.equalsInt([4], collections.iter.once[Int](4))
      ),
    ),
    testing.endDescribe(),
  );
