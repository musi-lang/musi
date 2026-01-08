/// Function definition and call tests
/// Tests function semantic tokens and parameter handling

fn add(a: Int, b: Int): Int {
  a + b
};

fn greet(name: String): String {
  $"Hello, {name}!"
};

fn apply(f: Int -> Int, x: Int): Int {
  f(x)
};

val sum := add(1, 2);
val message := greet("World");

fn double(n: Int): Int { n * 2 };
val result := apply(double, 21);

fn identity[T](x: T): T { x };
val id_result := identity[Int](42);
