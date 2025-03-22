func fib(n : Int) -> Int :=
    if n <= 1 then return n
    return fib(n - 1) + fib(n - 2)

proc greet(name : Str) :=
    writeln("Hello, ${name}!")

writeln(fib(10))
greet("World")