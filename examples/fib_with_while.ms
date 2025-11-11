const fib := proc (n: Nat) -> Nat {
  if n <= 1 then return n;

  var (a, b, i) := (0, 1, 2);

  while i <= n do {
    (a, b) <- (b, a + b);
    i <- i + 1;
  }

  b
};
