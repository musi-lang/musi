export fn option_is_some['T](opt: Option['T]): Bool => match opt with (
    None    => False
  | Some(_) => True
);

export fn option_is_none['T](opt: Option['T]): Bool => match opt with (
    None    => True
  | Some(_) => False
);

export fn option_unwrap_or['T](opt: Option['T], default: 'T): 'T => match opt with (
    None    => default
  | Some(v) => v
);

export fn option_map['T, 'U](opt: Option['T], f: 'T -> 'U): Option['U] => match opt with (
    None    => None
  | Some(v) => Some(f(v))
);

export fn option_and_then['T, 'U](opt: Option['T], f: 'T -> Option['U]): Option['U] => match opt with (
    None    => None
  | Some(v) => f(v)
);

export fn option_or['T](opt: Option['T], fallback: Option['T]): Option['T] => match opt with (
    None    => fallback
  | Some(v) => Some(v)
);

// Short-name aliases
export fn is_some['T](opt: Option['T]): Bool => option_is_some(opt);
export fn is_none['T](opt: Option['T]): Bool => option_is_none(opt);
export fn unwrap_or['T](opt: Option['T], default: 'T): 'T => option_unwrap_or(opt, default);
export fn map['T, 'U](opt: Option['T], f: 'T -> 'U): Option['U] => option_map(opt, f);
export fn and_then['T, 'U](opt: Option['T], f: 'T -> Option['U]): Option['U] => option_and_then(opt, f);
