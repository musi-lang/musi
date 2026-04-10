let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
    (
        Testing.describe("bytes/equals");
        Testing.it(
            "returns true for equal arrays",
            Testing.to_be_truthy(Bytes.equals([1, 2, 3], [1, 2, 3])),
        );
        Testing.it(
            "returns false for different arrays",
            Testing.to_be_falsy(Bytes.equals([1, 2, 3], [1, 2, 4])),
        );
        Testing.end_describe()
    );
