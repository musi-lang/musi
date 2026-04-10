let Testing := import "@std/testing";
let Option := import "@std/option";

export let test () :=
    (
        Testing.describe("option");
        Testing.it("is_some detects present value", Testing.to_be_truthy(Option.is_some[Int](Option.some[Int](1))));
        Testing.it("is_none detects missing value", Testing.to_be_truthy(Option.is_none[Int](Option.none[Int]())));
        Testing.it("unwrap_or returns fallback for none", Testing.to_be(Option.unwrap_or[Int](Option.none[Int](), 7), 7));
        Testing.end_describe()
    );
