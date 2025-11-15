fn incr(var x: Nat) {
  x <- x + 1;
};

fn swap(var a: Nat, var b: Nat) {
  val temp := a;
  a <- b;
  b <- temp;
};

fn compute(x: Nat, var result: Nat, var error: Bool) {
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

var (x, y) := (5, 15);
swap(x, y);
