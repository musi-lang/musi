let Testing := import "@std/testing";
let Assert := import "@std/assert";

export let test () :=
    (
        Testing.describe("assert/matchers");
        Testing.it("to_be passes on equal ints", Assert.to_be(4, 4));
        Testing.it("to_be_truthy passes on comparison", Assert.to_be_truthy(1 = 1));
        Testing.it("to_be_falsy passes on comparison", Assert.to_be_falsy(1 = 2));
        Testing.it("greater-than matcher passes", Assert.to_be_greater_than(5, 2));
        Testing.it("less-than matcher passes", Assert.to_be_less_than(2, 5));
        Testing.end_describe()
    );
