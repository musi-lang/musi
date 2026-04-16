let intMath := import "./_int.ms";
let floatMath := import "./_float.ms";

export let abs := intMath.abs;
export let signInt := intMath.signInt;
export let max := intMath.max;
export let min := intMath.min;
export let clamp := intMath.clamp;

export let absFloat := floatMath.absFloat;
export let maxFloat := floatMath.maxFloat;
export let minFloat := floatMath.minFloat;
export let clampFloat := floatMath.clampFloat;
