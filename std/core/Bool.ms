export fn bool_not(b: Bool): Bool => match b with (
    False => True
  | True  => False
);

export fn bool_and(a: Bool, b: Bool): Bool => match a with (
    False => False
  | True  => b
);

export fn bool_or(a: Bool, b: Bool): Bool => match a with (
    True  => True
  | False => b
);
