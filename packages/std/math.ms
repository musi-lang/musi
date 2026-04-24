let floatMath := import "./math/float.ms";
let intMath := import "./math/integer.ms";

--- Absolute integer value.
export let abs := intMath.abs;
--- Integer sign value.
export let signInt := intMath.signInt;
--- Maximum integer value.
export let max := intMath.max;
--- Minimum integer value.
export let min := intMath.min;
--- Clamp an integer into inclusive bounds.
export let clamp := intMath.clamp;

--- Absolute float value.
export let absFloat := floatMath.absFloat;
--- Maximum float value.
export let maxFloat := floatMath.maxFloat;
--- Minimum float value.
export let minFloat := floatMath.minFloat;
--- Clamp a float into inclusive bounds.
export let clampFloat := floatMath.clampFloat;
