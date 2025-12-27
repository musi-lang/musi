// ============================================================================
// COMMENTS
// ============================================================================

// line comment
/// documentation line comment

/* block comment */
/** documentation block comment
 * @param x - input value
 */

#! /usr/bin/env musi

// ============================================================================
// IMPORTS
// ============================================================================

import "@std/io";
import "@std/math";

// ============================================================================
// BASIC LITERALS & TYPES
// ============================================================================

val unit_val: Unit := ();
val int_val := 42;
val hex_val := 0xFF;
val bin_val := 0b1010;
val float_val := 3.14159;
val sci_val := 1.23e-4;
val bool_true := 1;
val bool_false := 0;

val str := "hello world";
val chr: Rune := 'A';
val escape := "line\n\ttab\u{1F600}";
val `and` := "escaped keyword identifier";

val typed_int: Int32 := 42;
val typed_float: Float32 := 3.14;
val typed_nat: Nat8 := 255;

// ============================================================================
// OPERATORS
// ============================================================================

val ari := (10 + 5) * 2 - 3 / 2;
val pow := 2 ** 8;
val rem := 17 % 5;

val cmp := (10 > 5) and (3 <= 7);
val log_op := bool_true or bool_false;
val neg := not bool_false;

val bitwise := (0xFF & 0x0F) | (0xF0 ^ 0x55);
val bit_not := ~0xFF;

// ============================================================================
// VARIABLES & ASSIGNMENT
// ============================================================================

var counter := 0;
counter <- counter + 1;
counter <- counter * 2;

var flags := 0b1010;
flags <- flags & 0b1100;

// ============================================================================
// POINTERS
// ============================================================================

var value := 100;
var ptr := @value;
val deref := ptr.^;
ptr.^ <- 200;

val ptr_type: ^Nat32 := @value;
val ptr_ptr: ^^Nat32 := @ptr_type;

// ============================================================================
// TYPE OPERATIONS
// ============================================================================

alias MyInt := Int32;
val casted := value as Float32;
val type_check := value is Int32;
val size_info := size_of(Int32);

// ============================================================================
// STRUCTURAL DATA TYPES
// ============================================================================

val Point := record { x: Float32; y: Float32 };
val Color := record { r: Nat8; g: Nat8; b: Nat8 };

val origin: Point := .{ x := 0.0, y := 0.0 };
val red: Color := .{ r := 255, g := 0, b := 0 };

val distance := fn (p1: Point, p2: Point) {
  val dx := p1.x - p2.x;
  val dy := p1.y - p2.y;
  sqrt(dx * dx + dy * dy)
};

// ============================================================================
// ALGEBRAIC DATA TYPES
// ============================================================================

val Option := choice Option[T] { case Some(T), case None };
val Result := choice Result[T, E] { case Ok(T), case Err(E) };

val Shape := choice Shape {
  case Circle(radius: Float32),
  case Rectangle(width: Float32, height: Float32),
  case Triangle(a: Float32, b: Float32, c: Float32)
};

val area := fn (s: Shape) {
  match s {
    case Circle(r) => 3.14159 * r * r,
    case Rectangle(w, h) => w * h,
    case Triangle(a, b, c) => {
      val s := (a + b + c) / 2.0;
      sqrt(s * (s - a) * (s - b) * (s - c))
    }
  }
};

// ============================================================================
// FUNCTIONS
// ============================================================================

val add := fn (x, y) { x + y };
val multiply := fn (x, y) { x * y };

val curry_add := fn (x) { fn (y) { x + y } };

val write_thru_ptr := fn (p: ^Int32, v: Int32) {
  p.^ <- v;
};

val factorial := fn (n) {
  if n <= 1 { return 1; };
  n * factorial(n - 1)
};

// ============================================================================
// CONTROL FLOW
// ============================================================================

val abs := fn (n) {
  if n < 0 { -n } else { n }
};

val sign := fn (n) {
  if n > 0 { 1 }
  else if n < 0 { -1 }
  else { 0 }
};

val sum_to := fn (n) {
  var total := 0;
  var i := 1;
  while i <= n {
    total <- total + i;
    i <- i + 1;
  };
  total
};

val print_range := fn (start, end) {
  for i in start..end {
    writeln($"value: {i}");
  };
};

// ============================================================================
// PATTERN MATCHING
// ============================================================================

val maybe_value := Some(42);

val unwrap := fn (opt) {
  match opt {
    case Some(v) => v,
    case None => 0
  }
};

val handle_result := fn (res) {
  match res {
    case Ok(v) => writeln($"Success: {v}"),
    case Err(e) => writeln($"Failure: {e}")
  }
};

val process_option := fn (opt) {
  if case Some(v) := opt {
    writeln($"got value '{v}'");
  };
};

val process_stream := fn (next_fn) {
  while case Some(item) := next_fn() {
    writeln($"processing '{item}'...");
  };
};

// ============================================================================
// STRING TEMPLATES
// ============================================================================

val name := "Alice";
val age := 30;
val greeting := $"Hello {name}, you are {age} years old";
val calculation := $"2 + 2 = {2 + 2}";

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

val pipe_example := 10 |> add(5) |> multiply(2);

val block_result := {
  val x := 10;
  val y := 20;
  x + y
};

val nested_match := fn (opt) {
  match opt {
    case Some(Ok(v)) => v,
    case Some(Err(e)) => -1,
    case None => 0
  }
};

// ============================================================================
// TUPLES & DESTRUCTURING
// ============================================================================

val pair := (10, 20);
val (first, second) := pair;

val swap := fn (tuple) {
  val (a, b) := tuple;
  (b, a)
};

val complex_destructure := {
  val point := .{ x := 5.0, y := 10.0 };
  val (px, py) := (point.x, point.y);
  px + py
};

// ============================================================================
// PRACTICAL EXAMPLES
// ============================================================================

val safe_divide := fn (a, b) {
  if b = 0 {
    Err("division by zero")
  } else {
    Ok(a / b)
  }
};

val find_max := fn (nums) {
  var max := nums.[0];
  for n in nums {
    if n > max {
      max <- n;
    };
  };
  max
};

val map := fn (f, list) {
  var result := [];
  for item in list {
    result <- result + [f(item)];
  };
  result
};

val filter := fn (pred, list) {
  var result := [];
  for item in list {
    if pred(item) {
      result <- result + [item];
    };
  };
  result
};

// ============================================================================
// EXTERNAL BLOCKS & FUNCTIONS
// ============================================================================

// extern "C" {
//   fn malloc(size: Nat64): ^Any;
//   fn free(ptr: ^Any): Unit;
// };
