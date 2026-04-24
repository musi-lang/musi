let std := import "@std";
let testing := import "@std/testing";

export let test () :=
  (
    testing.describe("std");
    let sameBytes := std.bytes.fromArray([1, 2]);
    let oneList := std.collections.list.of[Int](1);
    let currentOs := std.os.targetOs(std.os.current);
    let noneInt := std.option.noneOf[Int]();
    testing.it(
      "root re-exports 'bytes' family",
      sameBytes.equals(std.bytes.fromArray([1, 2])),
    );
    testing.it(
      "root re-exports 'list' family",
      std.collections.list.equalsInt(
        oneList,
        std.collections.list.fromArray[Int]([1]),
      ),
    );
    testing.it(
      "root re-exports 'math' family",
      testing.toBe(std.math.clamp(9, 0, 4), 4),
    );
    testing.it(
      "root re-exports 'bits' family",
      testing.toBe(std.bits.toInt8(std.bits.fromInt8(3)), 3),
    );
    testing.it(
      "root re-exports 'encoding' family",
      std.encoding.hex.encode("a") /= "",
    );
    testing.it("root re-exports 'os' family", currentOs.toString() /= "",);
    testing.it(
      "root re-exports 'option' family",
      testing.toBe(std.option.unwrapOr[Int](noneInt, 5), 5),
    );
    testing.it(
      "local alias preserves import record target",
      std.bytes.fromArray([3, 4]).equals(std.bytes.fromArray([3, 4])),
    );
    testing.endDescribe()
  );
