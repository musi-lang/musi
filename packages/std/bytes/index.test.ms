let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
  (
    Testing.describe("bytes");
    Testing.it("'copy' copies the array", Testing.toBeTruthy(Bytes.equals(Bytes.copy([1, 2, 3]), [1, 2, 3])));
    Testing.it("'concat' concatenates arrays", Testing.toBeTruthy(Bytes.equals(Bytes.concat([1, 2], [3, 4]), [1, 2, 3, 4])));
    Testing.it("'equals' returns true for equal arrays", Testing.toBeTruthy(Bytes.equals([1, 2, 3], [1, 2, 3])));
    Testing.it("'equals' returns false for different arrays", Testing.toBeFalsy(Bytes.equals([1, 2, 3], [1, 2, 4])));
    Testing.it("'append' adds trailing byte", Testing.toBeTruthy(Bytes.equals(Bytes.append([1, 2], 3), [1, 2, 3])));
    Testing.it("'prepend' adds leading byte", Testing.toBeTruthy(Bytes.equals(Bytes.prepend(0, [1, 2]), [0, 1, 2])));
    Testing.it("'isEmpty' detects empty bytes", Testing.toBeTruthy(Bytes.isEmpty([])));
    Testing.endDescribe()
  );
