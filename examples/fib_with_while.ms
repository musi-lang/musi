const fib := proc (n: Nat) -> Nat {
  if n <= 1 then {
    return n;
  }

  var a: Nat := 0;
  var b: Nat := 1;
  var i: Nat := 2;

  while i <= n do {
    (a, b) <- (b, a + b);
    i <- i + 1;
  }

  b
};
