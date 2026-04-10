let Testing := import "@std/testing";
let Std := import "@std";

export let test () :=
    (
        let Bytes := Std.Bytes;
        let Math := Std.Math;
        let Option := Std.Option;
        Testing.describe("std");
        Testing.it("root reexports bytes family", Testing.to_be_truthy(Bytes.equals([1, 2], [1, 2])));
        Testing.it("root reexports math family", Testing.to_be(Math.clamp(9, 0, 4), 4));
        Testing.it("root reexports option family", Testing.to_be(Option.unwrap_or[Int](Option.none[Int](), 5), 5));
        Testing.it("local alias preserves module target", Testing.to_be_truthy(Bytes.equals([3, 4], [3, 4])));
        Testing.end_describe()
    );
