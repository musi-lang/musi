// std/core/option.ms — Option['T] utilities (type defined in prelude)
// import { is_some, is_none, unwrap_or, option_map, option_and_then, ... } from "std/core/option"

/// Return `.True` if the option holds a value.
export fn is_some['T](opt: Option['T]): Bool => match opt with (
    .None    => .False
  | .Some(_) => .True
);

/// Return `.True` if the option is empty.
export fn is_none['T](opt: Option['T]): Bool => match opt with (
    .None    => .True
  | .Some(_) => .False
);

/// Return the contained value, or `default` if the option is `.None`.
export fn unwrap_or['T](opt: Option['T], default: 'T): 'T => match opt with (
    .None    => default
  | .Some(v) => v
);

/// Apply `f` to the contained value and wrap the result; pass through `.None`.
export fn option_map['T, 'U](opt: Option['T], f: 'T -> 'U): Option['U] => match opt with (
    .None    => .None
  | .Some(v) => .Some(f(v))
);

/// Apply `f` (which returns an `Option`) to the contained value; pass through `.None`.
/// Useful for chaining fallible operations without nesting.
export fn option_and_then['T, 'U](opt: Option['T], f: 'T -> Option['U]): Option['U] => match opt with (
    .None    => .None
  | .Some(v) => f(v)
);

/// Keep the value only if it satisfies `pred`; otherwise return `.None`.
export fn option_filter['T](opt: Option['T], pred: 'T -> Bool): Option['T] => match opt with (
    .None    => .None
  | .Some(v) => if pred(v) then .Some(v) else .None
);

/// Return `opt` if it is `.Some`, otherwise return `fallback`.
export fn option_or['T](opt: Option['T], fallback: Option['T]): Option['T] => match opt with (
    .None    => fallback
  | .Some(v) => .Some(v)
);
