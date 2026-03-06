fn factorial(n: Int): Int (
    if n <= 1 then 1 else n * factorial(n - 1)
);
writeln(int_to_string(factorial(10)));
