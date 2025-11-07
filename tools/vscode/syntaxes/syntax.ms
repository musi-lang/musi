// ============================================================================
// COMMENTS
// ============================================================================

// single-line comment
/// documentation comment
/// @param {Int?} [value:=42] - parameter with default
/// @returns {Data!} - operation result
/// @throws {NetworkError} - when connection fails
/// @deprecated - use new_function instead

/* block comment */
/** documentation block comment
 * @param x - input value
 */

#! /usr/bin/env musi

// ============================================================================
// KEYWORDS & CONTROL FLOW
// ============================================================================

const x := 42;
var y := 0;
y <- 10;

if x > 0 then { writeln("positive") }
if x > 0 then { writeln("pos") } else { writeln("neg") }
if x > 0 then {
  writeln("positive")
} else if x < 0 then {
  writeln("negative")
} else {
  writeln("zero")
}

while hasNext() do { process(next()) }
for item in items do { handle(item) }
for const i in 0..<10 do { writeln(`${i}`) }
do { step() } while continuing()

match result with {
case .Pass(const v) -> use(v),
case .Fail(const e) -> handle(e),
}

if x > 0 and then y < 10 then { inRange() }
if valid or else canRecover() then { proceed() }

defer cleanup();
break;
continue;
return value;
yield item;

// ============================================================================
// LITERALS
// ============================================================================

const dec := 42;
const hex := 0xFF;
const bin := 0b1010;
const oct := 0o755;
const typed_int := 255_n8;
const big_num := 1_000_000;

const pi := 3.14159;
const sci := 1.23e-4;
const typed_float := 3.14_b32;
const double := 2.718_b64;

const t := true;
const f := false;

const str := "hello world";
const chr := 'A';
const tmpl := `Hello ${name}, you have ${count} items`;
const escape := "line1\nline2\ttab\u{1F600}";

const self_fld := self;
const self_ref := Self;

// ============================================================================
// OPERATORS
// ============================================================================

// Arithmetic
const add := x + y;
const sub := x - y;
const mul := x * y;
const div := x / y;
const modulo := x mod y;
const power := x^2;

// Comparison
const eq := x = y;
const neq := x =/= y;
const lt := x < y;
const lte := x <= y;
const gt := x > y;
const gte := x >= y;

// Logical
const and_op := a and b;
const or_op := a or b;
const not_op := not a;
const xor_op := a xor b;
const nand_op := a nand b;
const nor_op := a nor b;
const xnor_op := a xnor b;

// Bitwise
const shl_op := x shl 2;
const shr_op := x shr 2;

// Ranges
const inclusive := 0..10;
const exclusive := 0..<10;

// Type operators
const optional := value?;
const fallible := operation!;
const fallible_typed := operation!Error;
const cast := value as Int;
const typecheck := value is Int;

// Arrow
const arrow := proc (x: Int) -> Int { x + 1 };

// ============================================================================
// TYPES
// ============================================================================

const unit_val: Unit := ();
const bool_val: Bool := true;
const int_val: Int := 42;
const nat_val: Nat := 42;
const bin_val: Bin := 3.14;
const bin32_val: Bin32 := 3.14_b32;
const bin64_val: Bin64 := 3.14_b64;
const rune_val: Rune := 'A';
const text_val: Text := "hello";

const array_type: [Int] := [1, 2, 3];
const tuple_type: (Int, Text) := (42, "answer");
const ptr_type: Ptr<Int> := addr(x);
const proc_type: proc (Int) -> Int := inc;

const box_val: Box<Int> := Box{ 42 };
const opt_val: Int? := .Some(42);
const res_val: Int!Text := .Pass(42);

const maybe: Int? := .Some(42);
const result: Data! := try load();
const result_typed: Data! := try load();

// ============================================================================
// PROCEDURES
// ============================================================================

const add := proc (x: Int, y: Int) -> Int { x + y };

const scale := proc (v: Bin, f: Bin) -> Bin {
  v * f
};

proc fill(var buf: [Int], val: Int) {
  for const i in 0..<len(buf) do {
    buf[i] <- val
  }
}

const generic := proc <T>(x: T) -> T { x };

const multi_generic := proc <T, U>(x: T, y: U) -> (T, U) {
  (x, y)
};

const async_proc := async proc (url: Text) -> Data! {
  const res := await http.get(url);
  const body := await res.json();
  body
};

unsafe proc write_ptr(p: Ptr<Nat>, v: Nat) {
  p.pointee <- v;
}

export unsafe proc raw_read(addr: Ptr<Unit>, size: Nat) -> [Nat8] {
  const buf: [Nat8] := [Nat8; size]{};
  memcpy(addr(buf), addr, size);
  buf
}

unsafe extern "C" proc malloc(size: Nat) -> Ptr<Unit>;

export unsafe extern "C" proc free(ptr: Ptr<Unit>);

// ============================================================================
// RECORDS (PRODUCT TYPES)
// ============================================================================

const Point := record {
  x: Bin64 := 0.0;
  y: Bin64 := 0.0;
};

const p1 := Point{ 3.0, 4.0 };
const p2 := Point{};
const p3 := Point{ .x := 1.0, .y := 2.0 };

const distance := proc (a: Point, b: Point) -> Bin64 {
  const dx := a.x - b.x;
  const dy := a.y - b.y;
  sqrt(dx * dx + dy * dy)
};

// ============================================================================
// CHOICES (SUM TYPES)
// ============================================================================

const Option := choice Option<T> {
  case Some(T),
  case None,
};

const Expect := choice Expect<T, E> {
  case Pass(T),
  case Fail(E),
};

const Shape := choice Shape {
  case Circle(r: Bin64),
  case Rect(w: Bin64, h: Bin64),
  case Triangle(a: Bin64, b: Bin64, c: Bin64),
};

const area := proc (s: Shape) -> Bin64 {
  match s with {
  case .Circle(const r) -> 3.14159 * r^2,
  case .Rect(const w, const h) -> w * h,
  case .Triangle(const a, const b, const c) -> {
    const s := (a + b + c) / 2.0;
    sqrt(s * (s - a) * (s - b) * (s - c))
  },
  }
};

// ============================================================================
// PATTERN MATCHING
// ============================================================================

if case .Some(const v) := opt then { use(v) }
while case .Some(const v) := next() do { loop(v) }
for case .Pass(const v) in results do { process(v) }

match value with {
case const x -> handle(x),
case const _ -> ignore(),
}

match opt with {
case .Some(const v) -> act(v),
case .None -> fallback(),
}

match tuple with {
case const (a, b) -> combine(a, b),
}

match num with {
case const n if n > 10 -> large(n),
case const n if n > 0 -> small(n),
case const _ -> zero_or_neg(),
}

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
const fast := proc (x: Int) -> Int { x * 2 };

@deprecated("use 'new_sum' instead")
const old_sum := proc (x: Int, y: Int) -> Int { x + y };

@test
proc test_addition() {
  const result := add(2, 3);
  assert(result = 5);
}

@inline
const compute := proc (x: Int) -> Int { x^2 };

// ============================================================================
// UNSAFE BLOCKS
// ============================================================================

const unsafe_op := unsafe {
  const p := addr(x);
  p.pointee
};

unsafe proc raw_copy(dest: Ptr<Unit>, src: Ptr<Unit>, size: Nat) {
  memcpy(dest, src, size);
}

// ============================================================================
// ERROR HANDLING
// ============================================================================

const load := proc (path: Text) -> Data! {
  const file := try open(path);
  const text := try file.read();
  try parse(text)
};

const handle_result := match operation() with {
  case .Pass(const v) -> v,
  case .Fail(const e) -> recover(e),
};

// ============================================================================
// IDENTIFIERS
// ============================================================================

const MAX_SIZE := 1024;
const DEFAULT_TIMEOUT := 30;

const UserData := record { name: Text; age: Nat };
const HttpError := choice HttpError { case NotFound, case ServerError };

const user_count := 42;
var active_sessions := 0;

const get_user := proc (id: Nat) -> User? { /* ... */ };
const is_valid := proc (x: Int) -> Bool { x > 0 };

const status := .Success;
const color := .Red;
const opt := .Some(42);
const res := .Pass(data);

const x_coord := point.x;
const name := user.name;
const first := array.0;

const result := compute(42);
const value := transform<Int>(data);
const output := process(input, config);

// ============================================================================
// COMPLEX EXPRESSIONS
// ============================================================================

const complex := {
  const helper := proc (x: Int) -> Int { x + 1 };
  const value := 42;
  helper(value)
};

const with_where := compute() where {
  const helper := proc (x: Int) -> Int { x + 1 };
  const value := 42;
};

const nested_generics := Map<Text, List<Int?>>{};

const chained := data
  .filter(proc (x) -> Bool { x > 0 })
  .map(proc (x) -> Int { x * 2 })
  .reduce(0, proc (acc, x) -> Int { acc + x });

// ============================================================================
// EDGE CASES
// ============================================================================

const multi := proc <T, U, V>(a: T, b: U, c: V) -> (T, U, V) { (a, b, c) };

match nested with {
case .Some(.Pass(const v)) -> use(v),
case .Some(.Fail(const e)) -> handle(e),
case .None -> fallback(),
}

const complex_type: proc (Int, Text) -> List<Data?>! := loader;

const ptr := addr(value);
const deref := ptr.pointee;
ptr.pointee <- 100;

const msg := `User ${user.name} has ${user.items.len()} items (${user.items.len() * 2} total)`;

const a := 10;
const b := 20;
const result := {
  const x := compute();
  const y := transform(x);
  x + y
};

const unit := (
  action();
  done();
);
