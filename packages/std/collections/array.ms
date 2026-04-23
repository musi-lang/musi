--- Core operation for `empty`.
export let empty [T] () : []T := [];
--- Core operation for `single`.
export let single [T] (value : T) : []T := [value];
--- Core operation for `pair`.
export let pair [T] (first : T, second : T) : []T := [first, second];
--- Core operation for `copy`.
export let copy [T] (values : []T) : []T := [...values];
--- Core operation for `concat`.
export let concat [T] (left : []T, right : []T) : []T := [...left, ...right];
--- Core operation for `append`.
export let append [T] (values : []T, value : T) : []T := [...values, value];
--- Core operation for `prepend`.
let prependValue [T] (values : []T, value : T) : []T := [value, ...values];
--- Core operation for `equalsInt`.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;
--- Core operation for `notEqualsInt`.
export let notEqualsInt (left : []Int, right : []Int) : Bool := left /= right;
--- Core operation for `isEmpty`.
export let isEmpty [T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 0
  | _ => 0 = 1
  );
--- Core operation for `nonEmpty`.
export let nonEmpty [T] (values : []T) : Bool :=
  match values (
  | [] => 0 = 1
  | _ => 0 = 0
  );

--- Prepend a leading value.
export let prepend [T] (value : T, values : []T) : []T :=
  prependValue[T](values, value);
