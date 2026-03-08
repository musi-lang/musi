// std/assert.ms — assertion helpers for use inside test() blocks
// assert, assert_msg, test are auto-imported from prelude.
// import { assert_eq_int, assert_eq_str, assert_some, assert_none, ... } from "std/assert"

/// Assert that `cond` is `.True`; panic with "expected true" if not.
export fn assert_true(cond: Bool): Unit =>
    assert_msg(cond, "expected true");

/// Assert that `cond` is `.False`; panic with "expected false" if not.
export fn assert_false(cond: Bool): Unit => match cond with (
    .False => ()
  | .True  => assert_msg(.False, "expected false")
);

/// Assert that two integers are equal; panic with their values if not.
export fn assert_eq_int(a: Int, b: Int): Unit =>
    assert_msg(a = b, string_concat(int_to_string(a), string_concat(" /= ", int_to_string(b))));

/// Assert that two integers are not equal; panic if they are.
export fn assert_ne_int(a: Int, b: Int): Unit => match a = b with (
    .False => ()
  | .True  => assert_msg(.False, string_concat(int_to_string(a), " should differ"))
);

/// Assert that two strings are equal; panic with their values if not.
export fn assert_eq_str(a: String, b: String): Unit =>
    assert_msg(a = b, string_concat(a, string_concat(" /= ", b)));

/// Assert that two strings are not equal; panic if they are.
export fn assert_ne_str(a: String, b: String): Unit => match a = b with (
    .False => ()
  | .True  => assert_msg(.False, string_concat(a, " should differ"))
);

/// Assert that an option is `.Some`; panic if it is `.None`.
export fn assert_some['T](opt: Option['T]): Unit => match opt with (
    .None    => assert_msg(.False, "expected .Some, got .None")
  | .Some(_) => ()
);

/// Assert that an option is `.None`; panic if it is `.Some`.
export fn assert_none['T](opt: Option['T]): Unit => match opt with (
    .None    => ()
  | .Some(_) => assert_msg(.False, "expected .None, got .Some")
);
