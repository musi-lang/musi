let fib : (Int) -> Int :=
    (n) => (
        1 if n <= 1
      | fib(n - 1) + fib(n - 2) if _
    );

#[entrypoint]
let main : () ~> () under { IO } :=
    () => (
        let result := fib(10);
        writeln(f"fib(10) = {result}")
    );
