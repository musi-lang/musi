let slice := import "./index.ms";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("slice");
    testing.it("'copy' keeps values",
      testing.toBeTruthy(slice.equalsInt(slice.copy[Int]([7, 8]), [7, 8]))
    );
    testing.it("'concat' joins slices",
      testing.toBeTruthy(slice.equalsInt(slice.concat[Int]([1], [2, 3]),
          [1, 2, 3]
        ))
    );
    testing.it("'isEmpty' detects empty slice",
      testing.toBeTruthy(slice.isEmpty[Int]([]))
    );
    testing.it("'nonEmpty' detects non-empty slice",
      testing.toBeTruthy(slice.nonEmpty[Int]([1]))
    );
    testing.endDescribe()
  );
