/**
 ** Matchers for the Expectation API.
 */

import "./expect" as _;

/// Effect used internally to abort on assertion failure.
let TestAbort := effect {
    let fail (msg : String) : Never
};

/// Check a condition, raising TestAbort on failure.
let assert_check (condition : Bool, msg : String) : Unit :=
    (() if condition | need TestAbort.fail(msg) if _);

// -- Equality --

/// Assert strict reference equality.
export let to_be [T] (exp : Expectation of T, expected : T) : Unit where T : Eq :=
    (
        let passed := exp.actual = expected;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to be {expected}")
    );

/// Assert deep/structural equality.
export let to_equal [T] (exp : Expectation of T, expected : T) : Unit where T : Eq :=
    (
        let passed := exp.actual = expected;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to equal {expected}")
    );

// -- Truthiness --

/// Assert the value is .True.
export let to_be_truthy (exp : Expectation of Bool) : Unit :=
    (
        let passed := exp.actual = .True;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to be truthy")
    );

/// Assert the value is .False.
export let to_be_falsy (exp : Expectation of Bool) : Unit :=
    (
        let passed := exp.actual = .False;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to be falsy")
    );

// -- Comparison --

/// Assert the value is greater than n.
export let to_be_greater_than [T] (exp : Expectation of T, n : T) : Unit where T : Ord :=
    (
        let passed := exp.actual > n;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to be greater than {n}")
    );

/// Assert the value is less than n.
export let to_be_less_than [T] (exp : Expectation of T, n : T) : Unit where T : Ord :=
    (
        let passed := exp.actual < n;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected {exp.actual} to be less than {n}")
    );

// -- Collections --

/// Assert the array contains an item.
export let to_contain [T] (exp : Expectation of []T, item : T) : Unit where T : Eq :=
    (
        let passed := any(exp.actual, (x : T) => x = item);
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected array to contain {item}")
    );

/// Assert the array has a specific length.
export let to_have_length [T] (exp : Expectation of []T, n : Int) : Unit :=
    (
        let passed := exp.actual.len = n;
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected array to have length {n}, got {exp.actual.len}")
    );

// -- Custom --

/// Assert the value satisfies a predicate.
export let to_satisfy [T] (exp : Expectation of T, pred : T -> Bool) : Unit :=
    (
        let passed := pred(exp.actual);
        let result := (passed if not exp.negated | not passed if _);
        assert_check(result, f"expected value to satisfy predicate")
    );
