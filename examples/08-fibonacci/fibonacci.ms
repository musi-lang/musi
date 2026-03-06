// Iterative Fibonacci using mutable variables and a while loop
fn fib(n: Int): Int => (
    var a := 0;
    var b := 1;
    var i := 0;
    while i < n loop (
        const tmp := a + b;
        a <- b;
        b <- tmp;
        i <- i + 1;
    );
    a
);

writeln(int_to_string(fib(0)));
writeln(int_to_string(fib(1)));
writeln(int_to_string(fib(10)));
writeln(int_to_string(fib(20)));
