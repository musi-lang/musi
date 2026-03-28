import "@std/assert/matchers" as _;
import "@std/math/max" as _;
import "@std/testing" as _;

export let test := () : Unit ~> Unit =>
    describe("math", () => (
        it("max", () => to_be(max(2, 3), 3))
    ));
