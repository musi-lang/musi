export let equals (left : []Int, right : []Int) : Bool := left = right;

export let notEquals (left : []Int, right : []Int) : Bool := left /= right;

export let isEmpty (bytes : []Int) : Bool :=
  match bytes (
  | [] => 0 = 0
  | _ => 0 = 1
  );

export let nonEmpty (bytes : []Int) : Bool :=
  match bytes (
  | [] => 0 = 1
  | _ => 0 = 0
  );
