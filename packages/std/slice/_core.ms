--- Internal implementation for `empty`.
export let empty [T] () : []T := [];
--- Internal implementation for `copy`.
export let copy [T] (values : []T) : []T := [...values];
--- Internal implementation for `concat`.
export let concat [T] (left : []T, right : []T) : []T := [...left, ...right];
--- Internal implementation for `equalsInt`.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;
--- Internal implementation for `isEmpty`.
export let isEmpty [T] (values : []T) : Bool := match values (
  |[] => 0 = 0
  | _ => 0 = 1
);
--- Internal implementation for `nonEmpty`.
export let nonEmpty [T] (values : []T) : Bool := match values (
  |[] => 0 = 1
  | _ => 0 = 0
);
