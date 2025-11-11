proc incr(var x: Nat) {
  x <- x + 1;
};

proc swap(var a: Nat, var b: Nat) {
  const temp := a;
  a <- b;
  b <- temp;
};

proc compute(x: Nat, var result: Nat, var error: Bool) {
  if x < 0 then {
    error <- true;
    result <- 0;
  } else {
    error <- false;
    result <- x * 2;
  };
};

var value := 10;
incr(value);

var x := 5;
var y := 15;
swap(x, y);
