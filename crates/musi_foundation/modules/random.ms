let Core := import "musi:core";
let Float := Core.Float;
let Int := Core.Int;

export opaque let Random := effect {
  let int () : Int;
  let intInRange (lowerBound : Int, upperBound : Int) : Int;
  let bool () : Int;
  let float01 () : Float;
  let entropyHex (count : Int) : String;
};

export let int () : Int := ask Random.int();

export let intInRange (lowerBound : Int, upperBound : Int) : Int :=
  ask Random.intInRange(lowerBound, upperBound);

export let bool () : Int := ask Random.bool();

export let float01 () : Float := ask Random.float01();

export let entropyHex (count : Int) : String := ask Random.entropyHex(count);
