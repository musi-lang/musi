let Runtime := import "musi:runtime";

export let intInRange (lowerBound : Int, upperBound : Int) : Int :=
  match () (
  | _ if upperBound <= lowerBound => lowerBound
  | _ => Runtime.randomIntInRange(lowerBound, upperBound)
  );

export let float01 () : Float := Runtime.randomFloat01();

export let bool () : Bool := Runtime.randomBool() = 0;

export let chance (percent : Int) : Bool :=
  match () (
  | _ if percent <= 0 => 0 = 1
  | _ if percent >= 100 => 0 = 0
  | _ => intInRange(0, 100) < percent
  );
