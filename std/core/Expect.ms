export fn expect_is_pass['T, 'E](e: Expect['T, 'E]): Bool => match e with (
    .Fail(_) => .False
  | .Pass(_) => .True
);

export fn expect_is_fail['T, 'E](e: Expect['T, 'E]): Bool => match e with (
    .Fail(_) => .True
  | .Pass(_) => .False
);

export fn expect_unwrap_or['T, 'E](e: Expect['T, 'E], default: 'T): 'T => match e with (
    .Fail(_) => default
  | .Pass(v) => v
);

export fn expect_map['T, 'U, 'E](e: Expect['T, 'E], f: 'T -> 'U): Expect['U, 'E] => match e with (
    .Fail(err) => .Fail(err)
  | .Pass(v)   => .Pass(f(v))
);

export fn expect_map_err['T, 'E, 'F](e: Expect['T, 'E], f: 'E -> 'F): Expect['T, 'F] => match e with (
    .Fail(err) => .Fail(f(err))
  | .Pass(v)   => .Pass(v)
);

export fn expect_and_then['T, 'U, 'E](e: Expect['T, 'E], f: 'T -> Expect['U, 'E]): Expect['U, 'E] => match e with (
    .Fail(err) => .Fail(err)
  | .Pass(v)   => f(v)
);
