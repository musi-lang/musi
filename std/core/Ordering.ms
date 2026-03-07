export choice Ordering { Lt | Eq | Gt };

export fn ordering_is_lt(o: Ordering): Bool => match o with ( .Lt => .True | _ => .False );
export fn ordering_is_eq(o: Ordering): Bool => match o with ( .Eq => .True | _ => .False );
export fn ordering_is_gt(o: Ordering): Bool => match o with ( .Gt => .True | _ => .False );

export fn ordering_reverse(o: Ordering): Ordering => match o with (
    .Lt => .Gt
  | .Eq => .Eq
  | .Gt => .Lt
);
