let Testing := import "@std/testing";
let Std := import "@std";

export let test () :=
  (
    Testing.describe("std");
    Testing.it("root reexports bytes family", Testing.toBeTruthy(Std.Bytes.equals([1, 2], [1, 2])));
    Testing.it("root reexports math family", Testing.toBe(Std.Math.clamp(9, 0, 4), 4));
    Testing.it(
      "root reexports option family",
      Testing.toBe(Std.Option.unwrapOr[Int](Std.Option.none[Int](), 5), 5),
    );
    Testing.it(
      "local alias preserves module target",
      Testing.toBeTruthy(Std.Bytes.equals([3, 4], [3, 4])),
    );
    Testing.endDescribe()
  );
