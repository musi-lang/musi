let runtime := import "musi:runtime";

--- Random integer in half-open bounds.
export let nextIntInRange (lowerBound : Int, upperBound : Int) : Int := match () (
  | _ if upperBound <= lowerBound => lowerBound
  | _ => runtime.randomIntInRange(lowerBound, upperBound)
);

--- Random float in zero-to-one range.
export let nextFloat () : Float := runtime.randomFloat01();

--- Random boolean value.
export let nextBool () : Bool := runtime.randomBool() /= 0;

--- Random boolean by integer percent chance.
export let chance (percent : Int) : Bool := match () (
  | _ if percent <= 0 => 0 = 1
  | _ if percent >= 100 => 0 = 0
  | _ => nextIntInRange(0, 100) < percent
);
