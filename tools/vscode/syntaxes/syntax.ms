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
// KEYWORDS & CONTROL FLOW
// ============================================================================

val x: Int32 := 42;
var y: Int32 := 0;
y <- 10;

if (x > 0) { writeln("positive"); };
if (x > 0) { writeln("pos"); } else { writeln("neg"); };
if (x > 0) {
  writeln("positive");
} else if (x < 0) {
  writeln("negative");
} else {
  writeln("zero");
};

while has_next() { process(next()); };
for i in 0..10 { writeln($"{i}"); };

match result {
case Ok(v) => use(v),
case Err(e) => handle(e)
};

if x > 0 and y < 10 { in_range(); };
if valid or can_recover() { proceed(); };

// ============================================================================
// LITERALS
// ============================================================================

val dec: Int32 := 42;
val hex: Int32 := 0xFF;
val bin: Int32 := 0b1010;
val typed_int: Nat8 := 255;
val big_num: Int32 := 1000000;

val pi: Bin32 := 3.14159;
val sci: Bin32 := 1.23e-4;
val typed_float: Bin32 := 3.14;
val double: Bin64 := 2.718;

val t: Int32 := 1;
val f: Int32 := 0;

val str: Text := "hello world";
val chr: Rune := 'A';
val tmpl: Text := $"Hello {name}, you have {count} items";
val escape: Text := "line1\nline2\ttab\u{1F600}";

// ============================================================================
// OPERATORS
// ============================================================================

// Arithmetic
val add: Int32 := x + y;
val sub: Int32 := x - y;
val mul: Int32 := x * y;
val div: Int32 := x / y;
val pow: Int32 := x ** 2;

// Comparison
val eq: Int32 := x = y;
val ne: Int32 := x /= y;
val lt: Int32 := x < y;
val le: Int32 := x <= y;
val gt: Int32 := x > y;
val ge: Int32 := x >= y;

// Logical (short-circuiting)
val and_op: Int32 := a and b;
val or_op: Int32 := a or b;
val not_op: Int32 := not a;

// Bitwise (non-short-circuiting)
val bitwise_and: Int32 := a & b;
val bitwise_or: Int32 := a | b;
val xor_op: Int32 := a ^ b;
val bitwise_not: Int32 := ~a;

// Memory operators
var ptr: ^Int32 := @x;
val deref: Int32 := ptr[];

// Assignment
counter <- counter + 1;
flags <- flags & MASK;

// Type operators
val type_cast: Int32 := value as Int32;
val type_test: Int32 := value is Int32;

// Pipe operator
val piped: Int32 := 10 |> add(5) |> multiply(2);

// ============================================================================
// TYPES
// ============================================================================

val unit_val: Unit := ();
val int8_val: Int8 := 42;
val int16_val: Int16 := 42;
val int32_val: Int32 := 42;
val int64_val: Int64 := 42;
val nat8_val: Nat8 := 42;
val nat16_val: Nat16 := 42;
val nat32_val: Nat32 := 42;
val nat64_val: Nat64 := 42;
val bin16_val: Bin16 := 3.14;
val bin32_val: Bin32 := 3.14;
val bin64_val: Bin64 := 3.14;
val dec32_val: Dec32 := 3.14;
val dec64_val: Dec64 := 3.14;
val rune_val: Rune := 'A';
val text_val: Text := "hello";

val ptr_type: ^Int32 := @x;
val ptr_ptr: ^^Int32 := @ptr_type;

val any_val: Any := 42;
val size_info: Nat32 := size_of(Int32);

// ============================================================================
// FUNCTIONS
// ============================================================================

fn add(x: Int32, y: Int32) -> Int32 {
  return x + y;
};

fn scale(v: Bin32, f: Bin32) -> Bin32 {
  return v * f;
};

fn write_ptr(p: ^Int32, v: Int32) {
  [p] <- v;
};

fn get_value(ptr: ^Int32) -> Int32 {
  return [ptr];
};

extern "C" fn malloc(size: Nat32) -> ^Unit { /* ... */ };

// ============================================================================
// STRUCTURAL DATA TYPES
// ============================================================================

record Point { x: Bin64; y: Bin64 };

val p1: Point := { .x := 3.0, .y := 4.0 };
val p2: Point := { .x := 0.0, .y := 0.0 };
val p3: Point := { .x := 1.0, .y := 2.0 };

fn distance(a: Point, b: Point) -> Bin64 {
  val dx: Bin64 := a.x - b.x;
  val dy: Bin64 := a.y - b.y;
  return sqrt(dx ** 2 + dy ** 2);
};

// ============================================================================
// ALGEBRAIC DATA TYPES
// ============================================================================

sum Option { case Some(value: Int32), case None };

sum Result { case Ok(value: Int32), case Err(msg: Text) };

sum Shape { case Circle(radius: Bin64), case Rect(width: Bin64, height: Bin64), case Triangle(a: Bin64, b: Bin64, c: Bin64) };

fn area(s: Shape) -> Bin64 {
  match s {
  case Circle(r) => 3.14159 * r ** 2,
  case Rect(w, h) => w * h,
  case Triangle(a, b, c) => {
    val s: Bin64 := (a + b + c) / 2.0;
    return sqrt(s * (s - a) * (s - b) * (s - c));
  }
  };
};

// ============================================================================
// PATTERN MATCHING
// ============================================================================

match opt {
case Some(v) => use(v),
case None => {}
};

// Match in conditional
if case Some(value) := opt {
  use(value);
};

// Match in loop
while case Some(item) := get_next() {
  process(item);
};

match result {
case Ok(v) => process(v),
case Err(e) => handle_error(e)
};

// ============================================================================
// MODULES & IMPORTS
// ============================================================================

import "@std/io";
import "@std/io";
import "./config.ms";
import "./types.ms";

// ============================================================================
// ERROR HANDLING
// ============================================================================

fn load(path: Text) -> Result {
  val file: Result := open(path);
  match file {
  case Ok(f) => {
    val content: Result := read_file(f);
    match content {
    case Ok(data_) => return Ok(data_),
    case Err(e) => return Err(e)
    };
  },
  case Err(e) => return Err(e)
  };
};

val handle_result: Int32 := match operation() {
case Ok(v) => v,
case Err(e) => recover(e)
};

// ============================================================================
// IDENTIFIERS
// ============================================================================

val MAX_SIZE: Nat32 := 1024;
val DEFAULT_TIMEOUT: Nat32 := 30;

record User {
  name: Text;
  age: Nat32;
  active: Int32
};

sum HttpError { case NotFound, case ServerError };

val user_count: Nat32 := 42;
var active_sessions: Nat32 := 0;

fn get_user(id: Nat32) -> Option {
  return None;
};

fn is_valid(x: Int32) -> Int32 {
  return x > 0;
};

val status: Int32 := 1;
val opt: Option := Some(42);
val res: Result := Ok(42);

val x_coord: Bin64 := point.x;
val name: Text := user.name;

val result: Int32 := compute(42);
val output: Int32 := process(input, config);

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

val complex: Int32 := {
  fn helper(x: Int32) -> Int32 {
    return x + 1;
  };
  val value: Int32 := 42;
  return helper(value);
};

val chained: Int32 := 10 |> add(5) |> multiply(2);

// ============================================================================
// EDGE CASES
// ============================================================================

match nested {
case Some(Ok(v)) => use(v),
case Some(Err(e)) => handle(e),
case None => fallback()
};

val ptr: ^Int32 := @value;
val deref: Int32 := ptr[];
ptr[] <- 100;

val msg: Text := $"User {user.name} has {count} items";

val a: Int32 := 10;
val b: Int32 := 20;
val result: Int32 := {
  val x: Int32 := compute();
  val y: Int32 := transform(x);
  return x + y;
};

val unit: Unit := {
  action();
  done();
  return ();
};
