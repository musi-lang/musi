let bytes := import "@std/bytes";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("bytes");
    let left := bytes.fromArray([1, 2]);
    let right := bytes.fromArray([3, 4]);
    let same := bytes.fromArray([1, 2]);
    testing.it("'copy' copies bytes",
      testing.toBeTruthy(bytes.equals(left.copy(), left))
    );
    testing.it("'concat' concatenates bytes",
      testing.toBeTruthy(bytes.equals(left.concat(right),
          bytes.fromArray([1, 2, 3, 4])
        ))
    );
    testing.it("'equals' returns true for equal bytes",
      testing.toBeTruthy(left.equals(same))
    );
    testing.it("'equals' returns false for different bytes",
      testing.toBeFalsy(left.equals(right))
    );
    testing.it("'append' adds trailing byte",
      testing.toBeTruthy(bytes.equals(left.append(3),
          bytes.fromArray([1, 2, 3])
        ))
    );
    testing.it("'prepend' adds leading byte",
      testing.toBeTruthy(bytes.equals(left.prepend(0),
          bytes.fromArray([0, 1, 2])
        ))
    );
    testing.it("'isEmpty' detects empty bytes",
      testing.toBeTruthy(bytes.empty().isEmpty())
    );
    testing.endDescribe()
  );
