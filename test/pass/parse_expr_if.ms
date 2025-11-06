const x := true;
const y := false;
const z := true;

const a := if x then { 1 } else { 2 };
const b := if y then { 3 } else if z then { 4 } else { 5 };
