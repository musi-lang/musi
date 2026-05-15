--! Random values module.

let randomHost := import "musi:random";

--- Random integer in half-open bounds.
export let nextIntInRange (lowerBound : Int, upperBound : Int) :=
  if upperBound <= lowerBound then lowerBound
  else randomHost.intInRange(lowerBound, upperBound,);

--- Random float in zero-to-one range.
export let nextFloat () := randomHost.float01();

--- Random boolean value.
export let nextBit () := randomHost.bool() /= 0;

--- Random boolean by integer percent chance.
export let chance (percent : Int) :=
  if percent <= 0 then 0 = 1
  else if percent >= 100 then 0 = 0 else nextIntInRange(0, 100,) < percent;
