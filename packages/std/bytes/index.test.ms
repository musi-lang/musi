let testing := import "@std/testing";
let bytes := import "@std/bytes";

export let test () :=
  (
    testing.describe("bytes");
    let left := bytes.fromArray([1, 2]);
    let right := bytes.fromArray([3, 4]);
    let same := bytes.fromArray([1, 2]);
    testing.it("'copy' copies bytes", testing.toBeTruthy(bytes.equals(bytes.copy(left), left)));
    testing.it("'concat' concatenates bytes", testing.toBeTruthy(bytes.equals(bytes.concat(left, right), bytes.fromArray([1, 2, 3, 4]))));
    testing.it("'equals' returns true for equal bytes", testing.toBeTruthy(bytes.equals(left, same)));
    testing.it("'equals' returns false for different bytes", testing.toBeFalsy(bytes.equals(left, right)));
    testing.it("'append' adds trailing byte", testing.toBeTruthy(bytes.equals(bytes.append(left, 3), bytes.fromArray([1, 2, 3]))));
    testing.it("'prepend' adds leading byte", testing.toBeTruthy(bytes.equals(bytes.prepend(left, 0), bytes.fromArray([0, 1, 2]))));
    testing.it("'isEmpty' detects empty bytes", testing.toBeTruthy(bytes.isEmpty(bytes.empty())));
    testing.endDescribe()
  );
