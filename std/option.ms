// std/option.ms — Option['T] utilities (type defined in prelude)
// import { is_some, is_none, unwrap_or, option_map, option_and_then, ... } from "std/option"

export fn is_some['T](opt: Option['T]): Bool => match opt with (
    .None    => .False
  | .Some(_) => .True
);

export fn is_none['T](opt: Option['T]): Bool => match opt with (
    .None    => .True
  | .Some(_) => .False
);

export fn unwrap_or['T](opt: Option['T], default: 'T): 'T => match opt with (
    .None    => default
  | .Some(v) => v
);

export fn option_map['T, 'U](opt: Option['T], f: 'T -> 'U): Option['U] => match opt with (
    .None    => .None
  | .Some(v) => .Some(f(v))
);

export fn option_and_then['T, 'U](opt: Option['T], f: 'T -> Option['U]): Option['U] => match opt with (
    .None    => .None
  | .Some(v) => f(v)
);

export fn option_filter['T](opt: Option['T], pred: 'T -> Bool): Option['T] => match opt with (
    .None    => .None
  | .Some(v) => if pred(v) then .Some(v) else .None
);

export fn option_or['T](opt: Option['T], fallback: Option['T]): Option['T] => match opt with (
    .None    => fallback
  | .Some(v) => .Some(v)
);
