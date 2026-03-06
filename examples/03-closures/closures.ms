const double := fn(x: Int): Int => x * 2;
const add := fn(a: Int, b: Int): Int => a + b;

fn apply(f: Int -> Int, x: Int): Int => f(x);

writeln(int_to_string(apply(double, 5)));
writeln(int_to_string(add(3, 4)));
