let pass := 0 = 0;
let failBool (message : String) := 0 = 1;

export let fail (message : String) := failBool(message);

export let toBe (actual : Int, expected : Int) :=
  case () of (
  | _ if actual = expected => pass
  | _ => fail("expected values to be equal")
  );

export let toEqual (actual : Int, expected : Int) :=
  toBe(actual, expected);

export let toBeTruthy (actual : Bool) :=
  case () of (
  | _ if actual => pass
  | _ => fail("expected value to be truthy")
  );

export let toBeFalsy (actual : Bool) :=
  case () of (
  | _ if actual => fail("expected value to be falsy")
  | _ => pass
  );

export let toBeGreaterThan (actual : Int, bound : Int) :=
  case () of (
  | _ if actual > bound => pass
  | _ => fail("expected value to be greater than bound")
  );

export let toBeLessThan (actual : Int, bound : Int) :=
  case () of (
  | _ if actual < bound => pass
  | _ => fail("expected value to be less than bound")
  );

export let expect (actual : Int) := (
  let matchToBe (expected : Int) := toBe(actual, expected);
  let matchToEqual (expected : Int) := toEqual(actual, expected);
  let matchToBeGreaterThan (bound : Int) :=
    toBeGreaterThan(actual, bound);
  let matchToBeLessThan (bound : Int) := toBeLessThan(actual, bound);
  {
    toBe := matchToBe,
    toEqual := matchToEqual,
    toBeGreaterThan := matchToBeGreaterThan,
    toBeLessThan := matchToBeLessThan
  }
);
