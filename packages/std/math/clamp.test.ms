let Testing := import "@std/testing";
let Math := import "@std/math";

export let test () :=
    (
        Testing.describe("math/clamp");
        Testing.it("keeps in-range value", Testing.to_be(Math.clamp(5, 0, 10), 5));
        Testing.it("clamps below low", Testing.to_be(Math.clamp(-1, 0, 10), 0));
        Testing.it("clamps above high", Testing.to_be(Math.clamp(11, 0, 10), 10));
        Testing.end_describe()
    );
