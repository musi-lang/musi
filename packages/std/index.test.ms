let testing := import "@std/testing";
let std := import "@std";

export let test () :=
  (
    testing.describe("std");
    testing.it("root re-exports 'bytes' family", testing.toBeTruthy(std.bytes.equals(std.bytes.fromArray([1, 2]), std.bytes.fromArray([1, 2]))));
    testing.it("root re-exports 'list' family", testing.toBeTruthy(std.list.equalsInt(std.list.of[Int](1), std.list.fromArray[Int]([1]))));
    testing.it("root re-exports 'math' family", testing.toBe(std.math.clamp(9, 0, 4), 4));
    testing.it("root re-exports 'sys' family", testing.toBeTruthy(std.sys.os /= ""));
    testing.it("root re-exports 'os' family", testing.toBeTruthy(std.os.osName(std.os.targetOs(std.os.current)) /= ""));
    testing.it("root re-exports 'option' family", testing.toBe(std.option.unwrapOr[Int](std.option.none[Int](), 5), 5));
    testing.it("local alias preserves module target", testing.toBeTruthy(std.bytes.equals(std.bytes.fromArray([3, 4]), std.bytes.fromArray([3, 4]))));
    testing.endDescribe()
  );
