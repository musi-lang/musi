// std/core/result.ms — Expect['T, 'E] utilities (type defined in prelude; Pass=Ok, Fail=Err)
// import { is_ok, is_err, result_unwrap_or, result_map, result_and_then, ... } from "std/core/result"

/// Return `.True` if the result is `.Pass`.
export fn is_ok['T, 'E](r: Expect['T, 'E]): Bool => match r with (
    .Fail(_) => .False
  | .Pass(_) => .True
);

/// Return `.True` if the result is `.Fail`.
export fn is_err['T, 'E](r: Expect['T, 'E]): Bool => match r with (
    .Fail(_) => .True
  | .Pass(_) => .False
);

/// Return the success value, or `default` if the result is `.Fail`.
export fn result_unwrap_or['T, 'E](r: Expect['T, 'E], default: 'T): 'T => match r with (
    .Fail(_) => default
  | .Pass(v) => v
);

/// Apply `f` to the success value; propagate the error unchanged.
export fn result_map['T, 'U, 'E](r: Expect['T, 'E], f: 'T -> 'U): Expect['U, 'E] => match r with (
    .Fail(e) => .Fail(e)
  | .Pass(v) => .Pass(f(v))
);

/// Apply `f` to the error value; propagate the success value unchanged.
export fn result_map_err['T, 'E, 'F](r: Expect['T, 'E], f: 'E -> 'F): Expect['T, 'F] => match r with (
    .Fail(e) => .Fail(f(e))
  | .Pass(v) => .Pass(v)
);

/// Apply `f` (which returns an `Expect`) to the success value; propagate errors.
export fn result_and_then['T, 'U, 'E](r: Expect['T, 'E], f: 'T -> Expect['U, 'E]): Expect['U, 'E] => match r with (
    .Fail(e) => .Fail(e)
  | .Pass(v) => f(v)
);

/// Convert an `Option` to an `Expect`, using `err` as the error if the option is `.None`.
export fn result_from_option['T, 'E](opt: Option['T], err: 'E): Expect['T, 'E] => match opt with (
    .None    => .Fail(err)
  | .Some(v) => .Pass(v)
);

/// Discard the error and convert an `Expect` to an `Option`.
export fn result_ok['T, 'E](r: Expect['T, 'E]): Option['T] => match r with (
    .Fail(_) => .None
  | .Pass(v) => .Some(v)
);
