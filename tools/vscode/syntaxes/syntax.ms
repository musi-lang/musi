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
// HELPER FUNCTIONS & VALUES
// ============================================================================

val sqrt := fn (x) { x ** 0.5 };
val open := fn (path) { Succ(path) };
val read_file := fn (f) { Succ("file content") };
val has_next := fn () { 0 };
val next := fn () { 42 };
val process := fn (x) { x };
val use := fn (v) { v };
val handle := fn (e) { e };
val in_range := fn () { () };
val valid := 1;
val can_recover := fn () { 1 };
val proceed := fn () { () };
val add := fn (x) { fn (y) { x + y } };
val multiply := fn (x) { fn (y) { x * y } };
val compute := fn () { 42 };
val transform := fn (x) { x * 2 };
val action := fn () { () };
val done := fn () { () };
val recover := fn (e) { 0 };
val operation := fn () { Succ(42) };
val handle_error := fn (e) { () };
val get_next := fn () { None };
val fallback := fn () { () };
val size_of := fn (t) { 4 };

// ============================================================================
// KEYWORDS & CONTROL FLOW
// ============================================================================

val x := 42;
var y := 0;
y <- 10;

if x > 0 { writeln("pos"); };
if x > 0 { writeln("pos"); } else { writeln("neg"); };
if x > 0 { writeln("pos");
} else if (x < 0) { writeln("neg");
} else { writeln("zero"); };

while has_next() { process(next()); };
for i in 0..10 { writeln($"{i}"); };

val result := Succ(42);
match result {
case Succ(v) => use(v),
case Fail(e) => handle(e)
};

if x > 0 and y < 10 { in_range(); };
if valid or can_recover() { proceed(); };

// ============================================================================
// LITERALS
// ============================================================================

val dec := 42;
val hex := 0xFF;
val bin := 0b1010;
val typed_int: Nat8 := 255;
val big_num := 1000000;

val PI := 3.14159;
val sci := 1.23e-4;
val typed_float: Float32 := 3.14;
val double := 2.718;

val t := 1;
val f := 0;

val name := "Alice";
val count := 5;
val str := "hello world";
val chr: Rune := 'A';
val tmpl := $"Hello {name}, you have {count} items";
val escape := "line1\nline2\ttab\u{1F600}";
val `and` := "keyword as escaped identifier";

// ============================================================================
// OPERATORS
// ============================================================================

val a := 10;
val b := 20;

val add_op := x + y;
val sub := x - y;
val mul := x * y;
val div := x / y;
val rem := x % y;
val mod_op := mod(x, y);
val pow := x ** 2;

val eq := x = y;
val ne := x /= y;
val lt := x < y;
val le := x <= y;
val gt := x > y;
val ge := x >= y;

val and_op := a and b;
val or_op := a or b;
val not_op := not a;

val MASK := 0xFF;
val bit_and := a & b;
val bit_or := a | b;
val xor_op := a ^ b;
val bit_not := ~a;

var value := 42;
var ptr := @x;
val deref := ptr.^;

var counter := 0;
counter <- counter + 1;
var flags := 0xFF;
flags <- flags & MASK;

val type_cast := value as Int32;
val type_test := value is Int32;

val piped := 10 |> add(5) |> multiply(2);

// ============================================================================
// TYPES
// ============================================================================

alias MyInt := Int;
alias Bool := Nat8;

val unit_val: Unit := ();
val int8_val: Int8 := 42;
val int16_val: Int16 := 42;
val int32_val: Int32 := 42;
val int64_val: Int64 := 42;
val nat8_val: Nat8 := 42;
val nat16_val: Nat16 := 42;
val nat32_val: Nat32 := 42;
val nat64_val: Nat64 := 42;
val float32_val: Float32 := 3.14;
val float64_val: Float64 := 3.14;
val rune_val: Rune := 'A';
val string_val: String := "hello";

val ptr_type: ^Int32 := @x;
val ptr_ptr: ^^Int32 := @ptr_type;

val any_val: Any := 42;
val size_info := size_of(Int32);

// ============================================================================
// FUNCTIONS
// ============================================================================

val add_fn := fn (x, y) { x + y };

val scale := fn (v, f) { v * f };

val write_ptr := fn (p, v) { p.^ <- v };

val get_value := fn (ptr) { ptr.^ };

val malloc := extern "C" unsafe fn (size) { /* ... */ };

// ============================================================================
// STRUCTURAL DATA TYPES
// ============================================================================

val Point := record { x: Float32; y: Float32 };

val p1: Point := .{ x := 3.0, y := 4.0 };
val p2: Point := .{ x := 0.0, y := 0.0 };
val p3: Point := .{ x := 1.0, y := 2.0 };

val dist := fn (a: Point, b: Point) {
  val (dx, dy) := (a.x - b.x, a.y - b.y);
  sqrt(dx ** 2 + dy ** 2)
};

val point := p1;
val x_coord := point.x;

// ============================================================================
// ALGEBRAIC DATA TYPES
// ============================================================================

val Option := choice Option[T] { case Some(T), case None };

val Expect := choice Expect[T, E] { case Succ(T), case Fail(E) };

val Shape := choice { case Circle(radius), case Rect(width, height), case Triangle(a, b, c) };

val area := fn (s: Shape) {
  match s {
  case Circle(r) => 3.14159 * r ** 2,
  case Rect(w, h) => w * h,
  case Triangle(a, b, c) => {
    val s := (a + b + c) / 2.0;
    return sqrt(s * (s - a) * (s - b) * (s - c));
  }
  }
};

val Some := fn (v) { v };
val None := 0;
val Succ := fn (v) { v };
val Fail := fn (e) { e };

// ============================================================================
// PATTERN MATCHING
// ============================================================================

val opt := Some(42);
match opt {
case Some(v) => use(v),
case None => {}
};

if case Some(value) := opt {
  use(value);
};

while case Some(item) := get_next() {
  process(item);
};

val result2 := Succ(100);
match result2 {
case Succ(v) => process(v),
case Fail(e) => handle_error(e)
};

// ============================================================================
// ERROR HANDLING
// ============================================================================

val load := fn (path) {
  val file := open(path);
  match file {
  case Succ(f) => {
    val content := read_file(f);
    match content {
    case Succ(data) => return Succ(data),
    case Fail(e) => return Fail(e)
    }
  },
  case Fail(e) => return Fail(e)
  }
};

val handle_result := match operation() {
case Succ(v) => v,
case Fail(e) => recover(e)
};

// ============================================================================
// IDENTIFIERS
// ============================================================================

val MAX_SIZE := 1024;
val DEFAULT_TIMEOUT := 30;

val User := record { name: String; age: Nat32; active: Bool };

val HttpError := choice { case NotFound, case ServerError };

val user_count := 42;
var active_sessions := 0;

val get_user := fn (id) { None };

val is_valid := fn (x) { x > 0 };

val status := 1;
val (opt2, res) := (Some(42), Succ(42));

val user := .{ name := "Bob", age := 25, active := 1 };
val user_name := user.name;

val input := 10;
val config := 5;
val result3 := compute();
val output := process(input);

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

val complex := {
  val helper := fn (x) { x + 1 };
  val value := 42;
  helper(value)
};

val chained := 10 |> add(5) |> multiply(2);

// ============================================================================
// EDGE CASES
// ============================================================================

val nested := Some(Succ(42));
match nested {
case Some(Succ(v)) => use(v),
case Some(Fail(e)) => handle(e),
case _ => fallback()
};

val value2 := 50;
val ptr2 := @value2;
val deref2 := ptr2.^;
ptr2.^ <- 100;

val msg := $"User {user.name} has {count} items";

val (pair_a, pair_b) := (10, 20);
val result4 := {
  val x_val := compute();
  val (x_res, y_res) := (compute(), transform(x_val));
  x_res + y_res
};

val unit := { action(); done(); () };
