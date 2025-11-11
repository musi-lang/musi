const fib := proc (n: Nat) -> Nat {
  if n <= 1 then return n;

  var (a, b) := (0, 1);

  for i in 2..n do (a, b) <- (b, a + b);
  b
};
