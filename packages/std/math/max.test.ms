let Testing := import "@std/testing";
let Math := import "@std/math";

export let test () :=
    (
        Testing.describe("math/max");
        Testing.it("picks larger value", Testing.to_be(Math.max(2, 5), 5));
        Testing.end_describe()
    );
