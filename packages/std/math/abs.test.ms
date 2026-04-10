let Testing := import "@std/testing";
let Math := import "@std/math";

export let test () :=
    (
        Testing.describe("math/abs");
        Testing.it("abs keeps positive ints", Testing.to_be(Math.abs(3), 3));
        Testing.it("abs flips negative ints", Testing.to_be(Math.abs(-3), 3));
        Testing.end_describe()
    );
