/// Attributes and exports test
/// Tests @[...] attributes and export modifiers

@[main]
fn run(): Int {
  val result := compute();
  result
};

@[test]
fn test_addition(): Bool {
  val sum := 2 + 2;
  sum = 4
};

@[test]
fn test_string_concat(): Bool {
  val greeting := $"Hello World";
  greeting = "Hello World"
};

@[inline]
fn fast_abs(x: Int): Int {
  if x < 0 { -x } else { x };
};

@[deprecated(message := "use 'new_api' instead")]
fn old_api(): Int { 42 };

export fn public_function(): Int { private_helper() };

fn private_helper(): Int { 100 };

export record PublicRecord { value: Int };

record PrivateRecord { secret: String };

export choice PublicChoice {
  case A,
  case B(Int),
};

@[extern(name := "c_function")]
extern fn external_call(x: Int): Int;
