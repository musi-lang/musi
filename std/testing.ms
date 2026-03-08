// std/testing.ms — test utilities beyond basic assert
// import { assert_eq, assert_neq, assert_true, assert_false, ... } from "std/testing"

/// Assert that two values are equal; panic with a descriptive message if not.
export fn assert_eq['T](actual: 'T, expected: 'T, msg: String): Unit => (
    if actual = expected then ()
    else assert_msg(.False, string_concat("assertion failed: ", msg))
);

/// Assert that two values are not equal.
export fn assert_neq['T](a: 'T, b: 'T, msg: String): Unit => (
    if a /= b then ()
    else assert_msg(.False, string_concat("assertion failed (expected not equal): ", msg))
);

/// Assert that a boolean is `.True`.
export fn assert_true(cond: Bool, msg: String): Unit => (
    match cond with (
        .True => ()
      | .False => assert_msg(.False, string_concat("expected true: ", msg))
    )
);

/// Assert that a boolean is `.False`.
export fn assert_false(cond: Bool, msg: String): Unit => (
    match cond with (
        .True => assert_msg(.False, string_concat("expected false: ", msg))
      | .False => ()
    )
);

/// Assert that an `Option` is `.Some`.
export fn assert_some['T](opt: Option['T], msg: String): 'T => (
    match opt with (
        .Some(v) => v
      | .None => (assert_msg(.False, string_concat("expected Some: ", msg)); panic_never())
    )
);

fn panic_never['T](): 'T => (
    // This will never be reached due to assert_msg panic
    panic_never()
);

/// Assert that an `Option` is `.None`.
export fn assert_none['T](opt: Option['T], msg: String): Unit => (
    match opt with (
        .Some(_) => assert_msg(.False, string_concat("expected None: ", msg))
      | .None => ()
    )
);

/// Assert that an `Expect` is `.Pass`.
export fn assert_pass['T, 'E](result: Expect['T, 'E], msg: String): 'T => (
    match result with (
        .Pass(v) => v
      | .Fail(_) => (assert_msg(.False, string_concat("expected Pass: ", msg)); panic_never())
    )
);

/// Assert that an `Expect` is `.Fail`.
export fn assert_fail['T, 'E](result: Expect['T, 'E], msg: String): 'E => (
    match result with (
        .Pass(_) => (assert_msg(.False, string_concat("expected Fail: ", msg)); panic_never())
      | .Fail(e) => e
    )
);

/// Assert that a string contains a substring.
export fn assert_contains(haystack: String, needle: String, msg: String): Unit => (
    if string_contains(haystack, needle) then ()
    else assert_msg(.False, string_concat(string_concat(string_concat(
        "expected '", haystack), "' to contain '"), string_concat(needle, string_concat("': ", msg))))
);

/// Assert that an array is empty.
export fn assert_empty['T](arr: []'T, msg: String): Unit => (
    if array_length(arr) = 0 then ()
    else assert_msg(.False, string_concat("expected empty array: ", msg))
);

/// Assert that an array is not empty.
export fn assert_not_empty['T](arr: []'T, msg: String): Unit => (
    if array_length(arr) > 0 then ()
    else assert_msg(.False, string_concat("expected non-empty array: ", msg))
);

/// Assert that two arrays have equal length.
export fn assert_len['T](arr: []'T, expected_len: Int, msg: String): Unit => (
    const actual := array_length(arr);
    if actual = expected_len then ()
    else assert_msg(.False, string_concat(string_concat(string_concat(
        "expected length ", int_to_string(expected_len)),
        string_concat(", got ", int_to_string(actual))), string_concat(": ", msg)))
);

/// Assert that a float is approximately equal (within epsilon).
export fn assert_approx(actual: Float, expected: Float, epsilon: Float, msg: String): Unit => (
    const diff := if actual > expected then actual - expected else expected - actual;
    if diff <= epsilon then ()
    else assert_msg(.False, string_concat(string_concat(string_concat(
        "expected ~", float_to_string(expected)),
        string_concat(", got ", float_to_string(actual))), string_concat(": ", msg)))
);

/// Run a group of tests with a name prefix.
export fn describe(group_name: String, tests: () -> Unit): Unit => (
    writeln(string_concat("=== ", string_concat(group_name, " ===")));
    tests()
);

/// Run a single test case (wrapper around intrinsic `test`).
export fn it(name: String, f: () -> Unit): Unit => test(name, f);
