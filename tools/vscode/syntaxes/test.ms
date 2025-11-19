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

val x := 42;
var y := 0;
y := 10;

if x > 0 { writeln("positive"); };
if x > 0 { writeln("pos"); } else { writeln("neg"); };
if x > 0 {
  writeln("positive");
} else if x < 0 {
  writeln("negative");
} else {
  writeln("zero");
};

while has_next() { process(next()); };
for item in items { handle(item); };
for i in 0..<10 { writeln($"{i}"); };

match result {
case .Succ(v) => use(v);
case .Fail(e) => handle(e);
};

if x > 0 && y < 10 { in_range(); };
if valid || can_recover() { proceed(); };

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
val tmpl := $"Hello {name}, you have {count} items";
val escape := "line1\nline2\ttab\u{1F600}";

// ============================================================================
// OPERATORS
// ============================================================================

// Arithmetic
val add := x + y;
val sub := x - y;
val mul := x * y;
val div := x / y;
val pow := x ** 2;

// Comparison
val eq := x = y;
val ne := x != y;
val lt := x < y;
val le := x <= y;
val gt := x > y;
val ge := x >= y;

// Logical
val and_op := a && b;
val or_op  := a || b;
val not_op := !a;
val xor_op := a ^ b;

// Bitwise
val shl_op := x << 2;
val shr_op := x >> 2;

// Compound assignments
counter :+= 1;
flags :&= MASK;
bits :<<= 2;
valid :&&= check_condition();

// Ranges
val inclusive := 0..10;
val exclusive := 0..<10;

// Type operators
val type_cast := value as Int;
val type_test := value is Int;

// Arrow
val arrow := def (x: Int) -> Int { x + 1 };

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
val proc_type: (Int) -> Int := inc;

val box_val: Box<Int> := Box{ 42 };
val opt_val: Int? := .Some(42);

val maybe := .Some(42);
val result := try load() else err { handle(err); };

// ============================================================================
// PROCEDURES
// ============================================================================

val add := def (x: Int, y: Int) -> Int { x + y };

val scale := def (v: Bin, f: Bin) -> Bin {
  v * f
};

val fill := def (var buf: [Int], value: Int) {
  for i in 0..<len(buf) {
    buf[i] := value;
  };
};

val generic<T> := def (x: T) -> T { x };

val multi_generic<T, U> := def (x: T, y: U) -> (T, U) {
  (x, y)
};

val write_ptr := def (p: Ptr<Nat>, v: Nat) {
  p.pointee := v;
};

export val raw_read := def (addr: Ptr<Unit>, size: Nat) -> [Nat8] {
  val buf: [Nat8] := [Nat8]{};
  memcpy(addr(buf), addr, size);
  buf
};

extern "C" {
  def malloc(size: Nat) -> Ptr<Unit>;
};

// ============================================================================
// RECORDS (PRODUCT TYPES)
// ============================================================================

data Point := {
  x: Bin64,
  y: Bin64
};

val p1 := Point{ .x := 3.0, .y := 4.0 };
val p2 := Point{ .x := 0.0, .y := 0.0 };
val p3 := Point{ .x := 1.0, .y := 2.0 };

val distance := def (a: Point, b: Point) -> Bin64 {
  val dx := a.x - b.x;
  val dy := a.y - b.y;
  sqrt(dx ** 2 + dy ** 2)
};

// ============================================================================
// CHOICES (SUM TYPES)
// ============================================================================

data Option<T> := case Some(T) | case None;

data Expect<T, E> := case Pass(T) | case Fail(E);

data Shape := case Circle(Bin64) | case Rect(Bin64, Bin64) | case Triangle(Bin64, Bin64, Bin64);

val area := def (s: Shape) -> Bin64 {
  match s {
  case .Circle(r) => 3.14159 * r ** 2;
  case .Rect(w, h) => w * h;
  case .Triangle(a, b, c) => {
    val s := (a + b + c) / 2.0;
    sqrt(s * (s - a) * (s - b) * (s - c))
  };
  }
};

// ============================================================================
// PATTERN MATCHING
// ============================================================================

match opt {
case .Some(v) => use(v);
case .None => {};
};

match value {
case x => handle(x);
case _ => ignore();
};

match opt {
case .Some(v) => act(v);
case .None => fallback();
};

match tuple {
case (a, b) => combine(a, b);
};

match num {
case n if n > 10 => large(n);
case n if n > 0 => small(n);
case _ => zero_or_neg();
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

@[inline]
val fast := def (x: Int) -> Int { x * 2 };

@[deprecated(msg := "use 'new_sum' instead")]
val old_sum := def (x: Int, y: Int) -> Int { x + y };

@[test]
val test_addition := def () {
  val result := add(2, 3);
  assert(result = 5);
};

// ============================================================================
// ERROR HANDLING
// ============================================================================

val load := def (path: String) -> Expect<Data, Error> {
  val file := try open(path) else err { return .Fail(err); };
  val text := try file.read() else err { return .Fail(err); };
  try parse(text) else err { return .Fail(err); }
};

val handle_result := match operation() {
  case .Succ(v) => v;
  case .Fail(e) => recover(e);
};

// ============================================================================
// IDENTIFIERS
// ============================================================================

val MAX_SIZE := 1024;
val DEFAULT_TIMEOUT := 30;

data User := {
  name: String,
  age: Nat,
  items: [String]
};
data HttpError := case NotFound | case ServerError;

val user_count := 42;
var active_sessions := 0;

val get_user := def (id: Nat) -> Option<User> { .None };
val is_valid := def (x: Int) -> Bool { x > 0 };

val status := .Success;
val color := .Red;
val opt := .Some(42);
val res := .Succ(data_value);

val x_coord := point.x;
val name := user.name;
val first := tuple.0;
val second := array[1];
val third := obj["key"];

val result := compute(42);
val value := transform<Int>(data_value);
val output := process(input, config);

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

val complex := {
  val helper := def (x: Int) -> Int { x + 1 };
  val value := 42;
  helper(value)
};

val nested_generics := Map<String, List<Option<Int>>>{};

val chained := filter((x) { x > 0 }) |> map((x) { x * 2 }) |> reduce(0, (acc, x) { acc + x });

// ============================================================================
// EDGE CASES
// ============================================================================

val multi<T, U, V> := def (a: T, b: U, c: V) -> (T, U, V) { (a, b, c) };

match nested {
case .Some(.Succ(v)) => use(v);
case .Some(.Fail(e)) => handle(e);
case .None => fallback();
};

val complex_type: (Int, String) -> Expect<List<Option<Data>>, Error> := loader;

val ptr := addr(value);
val deref := ptr.pointee;
ptr.pointee := 100;

val msg := $"User {user.name} has {user.items.len()} items ({user.items.len() * 2} total)";

val a := 10;
val b := 20;
val result := {
  val x := compute();
  val y := transform(x);
  x + y
};

val unit := {
  action();
  done();
};
