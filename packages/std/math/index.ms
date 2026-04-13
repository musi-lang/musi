let IntMath := import "./_int.ms";
let FloatMath := import "./_float.ms";

export let abs := IntMath.abs;
export let signInt := IntMath.signInt;
export let max := IntMath.max;
export let min := IntMath.min;
export let clamp := IntMath.clamp;

export let absFloat := FloatMath.absFloat;
export let maxFloat := FloatMath.maxFloat;
export let minFloat := FloatMath.minFloat;
export let clampFloat := FloatMath.clampFloat;
