--- Internal implementation for `empty`.
export let empty [T] () : []T := [];
--- Internal implementation for `single`.
export let single [T] (value : T) : []T := [value];
--- Internal implementation for `pair`.
export let pair [T] (first : T, second : T) : []T := [first, second];
--- Internal implementation for `copy`.
export let copy [T] (values : []T) : []T := [...values];
--- Internal implementation for `concat`.
export let concat [T] (left : []T, right : []T) : []T := [...left, ...right];
--- Internal implementation for `append`.
export let append [T] (values : []T, value : T) : []T := [...values, value];
--- Internal implementation for `prepend`.
export let prepend [T] (values : []T, value : T) : []T := [value, ...values];
--- Internal implementation for `equalsInt`.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;
--- Internal implementation for `notEqualsInt`.
export let notEqualsInt (left : []Int, right : []Int) : Bool := left /= right;
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
