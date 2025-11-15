// ============================================================================
// COMMENTS
// ============================================================================

// single-line comment






/* block comment */
/** documentation block comment
 * @param x - input value
 */

#! /usr/bin/env musi

// ============================================================================
// KEYWORDS & CONTROL FLOW
// ============================================================================

val x := 42;
var y := 0;
y <- 10;

if x > 0 then { writeln("positive") };
if x > 0 then { writeln("pos") } else { writeln("neg") };
if x > 0 then {
  writeln("positive")
} else if x < 0 then {
  writeln("negative")
} else {
  writeln("zero")
};

while hasNext() do { process(next()) };
for item in items do { handle(item) };
for i in 0..<10 do { writeln(`${i}`) };
do { step() } while continuing();

match result with {
case .Succ(v) -> use(v),
case .Fail(e) -> handle(e),
};

if x > 0 and then y < 10 then { inRange() };
if valid or else canRecover() then { proceed() };

defer cleanup();
break;
continue;
return value;
yield item;

// ============================================================================
// LITERALS
// ============================================================================

val dec := 42;
val hex := 0xFF;
val bin := 0b1010;
val oct := 0o755;
val typed_int := 255_n8;
val big_num := 1_000_000;

val pi := 3.14159;
val sci := 1.23e-4;
val typed_float := 3.14_b32;
val double := 2.718_b64;

val t := true;
val f := false;

val str := "hello world";
val chr := 'A';
val tmpl := `Hello ${name}, you have ${count} items`;
val escape := "line1\nline2\ttab\u{1F600}";

val self_fld := self;
val self_ref := Self;

// ============================================================================
// OPERATORS
// ============================================================================

// Arithmetic
val add := x + y;
val sub := x - y;
val mul := x * y;
val div := x / y;
val euc := x mod y; // Euclidean division
val pow := x^2;

// Comparison
val eq := x = y;
val ne := x =/= y;
val lt := x < y;
val le := x <= y;
val gt := x > y;
val ge := x >= y;

// Logical
val and_op := a and b;
val or_op := a or b;
val not_op := not a;
val xor_op := a xor b;
val nand_op := a nand b;
val nor_op := a nor b;
val xnor_op := a xnor b;

// Bitwise
val shl_op := x shl 2;
val shr_op := x shr 2;

// Ranges
val inclusive := 0..10;
val exclusive := 0..<10;

// Type operators
val optional := value?;
val fallible := operation!;
val type_cast := value as Int;
val type_test := value is Int;

// Arrow
val arrow := fn (x: Int) -> Int { x + 1 };

// ============================================================================
// TYPES
// ============================================================================

val unit_val: Unit := ();
val bool_val: Bool := true;
val int_val: Int := 42;
val nat_val: Nat := 42;
val bin_val: Bin := 3.14;
val bin32_val: Bin32 := 3.14_b32;
val bin64_val: Bin64 := 3.14_b64;
val rune_val: Rune := 'A';
val text_val: String := "hello";

val array_type: [Int] := [1, 2, 3];
val tuple_type: (Int, String) := (42, "answer");
val ptr_type: Ptr<Int> := addr(x);
val proc_type: fn (Int) -> Int := inc;

val box_val: Box<Int> := Box{ 42 };
val opt_val: Int? := .Some(42);
val res_val: Int!String := .Succ(42);

val maybe: Int? := .Some(42);
val result: Data! := try load();
val result_typed: Data! := try load();

// ============================================================================
// PROCEDURES
// ============================================================================

val add := fn (x: Int, y: Int) -> Int { x + y };

val scale := fn (v: Bin, f: Bin) -> Bin {
  v * f
};

fn fill(var buf: [Int], val: Int) {
  for i in 0..<len(buf) do {
    buf[i] <- val
  }
};

val generic<T> := fn (x: T) -> T { x };

val multi_generic<T, U> := fn (x: T, y: U) -> (T, U) {
  (x, y)
};

val async_proc := async fn (url: String) -> Data! {
  val res := await http.get(url);
  val body := await res.json();
  body
};

fn write_ptr(p: Ptr<Nat>, v: Nat) {
  p.pointee <- v;
};

export fn raw_read(addr: Ptr<Unit>, size: Nat) -> [Nat8] {
  val buf: [Nat8] := [Nat8; size]{};
  memcpy(addr(buf), addr, size);
  buf
};

extern "C" {
  fn malloc(size: Nat) -> Ptr<Unit>;
};

// ============================================================================
// RECORDS (PRODUCT TYPES)
// ============================================================================

val Point := record {
  x: Bin64 := 0.0;
  y: Bin64 := 0.0;
};

val p1 := Point{ 3.0, 4.0 };
val p2 := Point{};
val p3 := Point{ .x := 1.0, .y := 2.0 };

val distance := fn (a: Point, b: Point) -> Bin64 {
  val dx := a.x - b.x;
  val dy := a.y - b.y;
  sqrt(dx * dx + dy * dy)
};

// ============================================================================
// CHOICES (SUM TYPES)
// ============================================================================

val Option<T> := choice {
  case Some(T),
  case None,
};

val Expect<T, E> := choice {
  case Pass(T),
  case Fail(E),
};

val Shape := choice Shape {
  case Circle(r: Bin64),
  case Rect(w: Bin64, h: Bin64),
  case Triangle(a: Bin64, b: Bin64, c: Bin64),
};

val area := fn (s: Shape) -> Bin64 {
  match s with {
  case .Circle(r) -> 3.14159 * r^2,
  case .Rect(w, h) -> w * h,
  case .Triangle(a, b, c) -> {
    val s := (a + b + c) / 2.0;
    sqrt(s * (s - a) * (s - b) * (s - c))
  },
  }
};

// ============================================================================
// PATTERN MATCHING
// ============================================================================

if case .Some(v) := opt then { use(v) };
while case .Some(v) := next() do { loop(v) };
for case .Succ(v) in results do { process(v) };

match value with {
case x -> handle(x),
case _ -> ignore(),
};

match opt with {
case .Some(v) -> act(v),
case .None -> fallback(),
};

match tuple with {
case (a, b) -> combine(a, b),
};

match num with {
case n if n > 10 -> large(n),
case n if n > 0 -> small(n),
case _ -> zero_or_neg(),
};

// ============================================================================
// MODULES & IMPORTS
// ============================================================================

import * as io from "@std/io";
import { writeln, readln } from "@std/io";
import { Config } from "./config.ms";
import { User, Admin } from "./types.ms";

export { Config } from "./config.ms";
export { myFunc, MyType };

// ============================================================================
// ATTRIBUTES/DECORATORS
// ============================================================================

@inline
val fast := fn (x: Int) -> Int { x * 2 };

@deprecated("use 'new_sum' instead")
val old_sum := fn (x: Int, y: Int) -> Int { x + y };

@test
fn test_addition() {
  val result := add(2, 3);
  assert(result = 5);
}

@inline
val compute := fn (x: Int) -> Int { x^2 };

// ============================================================================
// ERROR HANDLING
// ============================================================================

val load := fn (path: String) -> Data! {
  val file := try open(path);
  val text := try file.read();
  try parse(text)
};

val handle_result := match operation() with {
  case .Succ(v) -> v,
  case .Fail(e) -> recover(e),
};

// ============================================================================
// IDENTIFIERS
// ============================================================================

val MAX_SIZE := 1024;
val DEFAULT_TIMEOUT := 30;

val UserData := record { name: String; age: Nat };
val HttpError := choice HttpError { case NotFound, case ServerError };

val user_count := 42;
var active_sessions := 0;

val get_user := fn (id: Nat) -> User? { /* ... */ };
val is_valid := fn (x: Int) -> Bool { x > 0 };

val status := .Success;
val color := .Red;
val opt := .Some(42);
val res := .Succ(data);

val x_coord := point.x;
val name := user.name;
val first := tuple.0;
val second := array[1];
val third := obj["key"];

val result := compute(42);
val value := transform<Int>(data);
val output := process(input, config);

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

val complex := {
  val helper := fn (x: Int) -> Int { x + 1 };
  val value := 42;
  helper(value)
};

val with_where := compute() where {
  val helper := fn (x: Int) -> Int { x + 1 };
  val value := 42;
};

val nested_generics := Map<String, List<Int?>>{};

val chained := data
  .filter(|x| -> Bool { x > 0 })
  .map(|x| -> Int { x * 2 })
  .reduce(0, |acc, x| -> Int { acc + x });

// ============================================================================
// EDGE CASES
// ============================================================================

val multi<T, U, V> := fn (a: T, b: U, c: V) -> (T, U, V) { (a, b, c) };

match nested with {
case .Some(.Succ(v)) -> use(v),
case .Some(.Fail(e)) -> handle(e),
case .None -> fallback(),
};

val complex_type: fn (Int, String) -> List<Data?>! := loader;

val ptr := addr(value);
val deref := ptr.pointee;
ptr.pointee <- 100;

val msg := `User ${user.name} has ${user.items.len()} items (${user.items.len() * 2} total)`;

val a := 10;
val b := 20;
val result := {
  val x := compute();
  val y := transform(x);
  x + y
};

val unit := (
  action();
  done();
);
