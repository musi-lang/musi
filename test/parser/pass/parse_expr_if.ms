val x := true;
val y := false;
val z := true;

val a := if x then { 1 } else { 2 };
val b := if y then { 3 } else if z then { 4 } else { 5 };
