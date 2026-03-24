/// @std/test -- Effect-based test runner API.

/// Effect providing the test runner operations.
let TestRunner := effect {
    let suite (name : String, body : () ~> Unit) : Unit;
    let case (name : String, body : () ~> Unit) : Unit;
    let before (setup : () ~> Unit) : Unit;
    let after (teardown : () ~> Unit) : Unit
};

/// Run a test suite.
export let describe (name : String, body : () ~> Unit) : Unit :=
    need TestRunner.suite(name, body);

/// Define a single test case.
export let it (name : String, body : () ~> Unit) : Unit :=
    need TestRunner.case(name, body);

/// Register a setup hook that runs before each test.
export let before_each (setup : () ~> Unit) : Unit :=
    need TestRunner.before(setup);

/// Register a teardown hook that runs after each test.
export let after_each (teardown : () ~> Unit) : Unit :=
    need TestRunner.after(teardown);
