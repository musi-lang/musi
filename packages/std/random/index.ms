let runtime := import "musi:runtime";

export let nextIntInRange (lowerBound : Int, upperBound : Int) : Int :=
  match () (
  | _ if upperBound <= lowerBound => lowerBound
  | _ => runtime.randomIntInRange(lowerBound, upperBound)
  );

export let nextFloat () : Float := runtime.randomFloat01();

export let nextBool () : Bool := runtime.randomBool() /= 0;

export let chance (percent : Int) : Bool :=
  match () (
  | _ if percent <= 0 => 0 = 1
  | _ if percent >= 100 => 0 = 0
  | _ => nextIntInRange(0, 100) < percent
  );
