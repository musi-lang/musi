--- Core operation for `empty`.
export let empty [T] () : []T := [];
--- Core operation for `copy`.
export let copy [T] (values : []T) : []T := [...values];
--- Core operation for `concat`.
export let concat [T] (left : []T, right : []T) : []T := [...left, ...right];
--- Core operation for `equalsInt`.
export let equalsInt (left : []Int, right : []Int) : Bool := left = right;
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
