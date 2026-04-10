let Testing := import "@std/testing";
let Bytes := import "@std/bytes";

export let test () :=
    (
        Testing.describe("bytes/copy");
        Testing.it(
            "copies the array",
            Testing.to_be_truthy(Bytes.equals(Bytes.copy([1, 2, 3]), [1, 2, 3])),
        );
        Testing.end_describe()
    );
