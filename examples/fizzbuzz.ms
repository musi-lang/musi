// Classic FizzBuzz: print 1..20, replacing multiples of 3 with Fizz,
// multiples of 5 with Buzz, and multiples of both with FizzBuzz.

var i := 1;
while i <= 20 loop (
    if i % 15 = 0 then writeln("FizzBuzz")
    elif i % 3 = 0 then writeln("Fizz")
    elif i % 5 = 0 then writeln("Buzz")
    else writeln(int_to_string(i));
    i <- i + 1
);
