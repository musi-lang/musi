let bytes := import "@std/bytes";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("bytes");
    let left := bytes.fromArray([1, 2]);
    let right := bytes.fromArray([3, 4]);
    let same := bytes.fromArray([1, 2]);
    let concatenated := bytes.fromArray([1, 2, 3, 4]);
    let appended := bytes.fromArray([1, 2, 3]);
    let prepended := bytes.fromArray([0, 1, 2]);
    testing.it("'copy' copies bytes", bytes.equals(left.copy(), left),);
    testing.it(
      "'concat' concatenates bytes",
      bytes.equals(left.concat(right), concatenated),
    );
    testing.it("'equals' returns true for equal bytes", left.equals(same),);
    testing.it(
      "'equals' returns false for different bytes",
      testing.toBeFalse(left.equals(right)),
    );
    testing.it(
      "'append' adds trailing byte",
      bytes.equals(left.append(3), appended),
    );
    testing.it(
      "'prepend' adds leading byte",
      bytes.equals(left.prepend(0), prepended),
    );
    testing.it("'isEmpty' detects empty bytes", bytes.empty().isEmpty(),);
    testing.endDescribe()
  );
