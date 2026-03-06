fn double(n: Int): Int => n * 2;
fn add(a: Int, b: Int): Int => a + b;

const x := 10.double();
const y := 3.add(4);
writeln(int_to_string(x));
writeln(int_to_string(y));
