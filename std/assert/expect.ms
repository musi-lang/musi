/// Expectation record wrapping a value under test.

/// An expectation captures a value and whether the assertion is negated.
export let Expectation [T] := record {
    actual : T;
    negated : Bool
};

/// Create an expectation for the given value.
export let expect [T] (actual : T) : Expectation of T :=
    Expectation.{ actual := actual, negated := .False };

/// Negate an expectation so matchers assert the opposite.
export let not [T] (exp : Expectation of T) : Expectation of T :=
    Expectation.{ actual := exp.actual, negated := not exp.negated };
