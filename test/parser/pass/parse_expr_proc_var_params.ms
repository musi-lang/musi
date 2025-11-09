proc incr(var x: Int) {
  x <- x + 1;
};

proc swap(var a: Int, var b: Int) {
  const temp := a;
  a <- b;
  b <- temp;
};

proc compute(x: Int, var result: Int, var error: Bool) {
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
