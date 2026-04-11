let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
    (
        Testing.describe("bytes/concat");
        Testing.it(
            "concatenates arrays",
            Testing.to_be_truthy(Bytes.equals(Bytes.concat([1, 2], [3, 4]), [1, 2, 3, 4])),
        );
        Testing.end_describe()
    );
