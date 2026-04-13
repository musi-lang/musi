let Testing := import "@std/testing";
let Std := import "@std";

export let test () :=
  (
    Testing.describe("std");
    Testing.it("root reexports bytes family", Testing.toBeTruthy(Std.bytes.equals([1, 2], [1, 2])));
    Testing.it("root reexports math family", Testing.toBe(Std.math.clamp(9, 0, 4), 4));
    Testing.it(
      "root reexports option family",
      Testing.toBe(Std.option.unwrapOr[Int](Std.option.none[Int](), 5), 5),
    );
    Testing.it(
      "local alias preserves module target",
      Testing.toBeTruthy(Std.bytes.equals([3, 4], [3, 4])),
    );
    Testing.endDescribe()
  );
