# Musi Language Specification

Musi is statically typed systems programming language that also lets you use dynamic types when you want. It compiles to bytecode and focuses on being simple, safe, and expressive.

## Type System

Musi uses **gradual typing** with local `Hindley-Milner` inference. You can write types explicitly or let compiler figure them out.

### Basic Types

**Integers (signed):**

- `Int8`, `Int16`, `Int32`, `Int64`, `Int128`

**Naturals (unsigned):**

- `Nat8`, `Nat16`, `Nat32`, `Nat64`, `Nat128`

**Floats (IEEE-754 binary):**

- `Bin16`, `Bin32`, `Bin64`

**Decimals (IEEE-754 decimal, software... for now):**

- `Dec32`, `Dec64`, `Dec128`

**Text:**

- `Rune` (single UTF-32 character)
- `String` (UTF-8 encoded text)

**Logic:**

- `Bool` (true/false)

**Special:**

- `Unit` (empty tuple `()`, like void but better, but also not quite like void)
- `Any` (dynamic type)
- `Never` (bottom type, for things that never return)

### Building Complex Types

- **Generics:** `List[T]`
- **Optional:** `?T` (same as `Option[T]`)
- **Pointer:** `^T` (raw pointer, reading is safe, writing is not)
- **Array:** `[]T` (dynamic slice) or `[N]T` (fixed size)
- **Tuple:** `(A, B, C)`
- **Function:** `A -> B`

**Examples:**

```musi
val i: Int32 := 0;
val s: String := "hello";
val opt: ?Int32 := 10;
val ptr: ^Int32 := @i;      // @ is addr-of, borrowed from Pascal
val list: List[String] := List.{ head := "a", tail := None };
val func: Int32 -> Bool := fn (x) { x > 0 };
```

### Functions as Types

Functions are first-class values. The `->` operator is right-associative, so you can curry.

**Curried (default way):**

```musi
val add: Int32 -> Int32 -> Int32 := fn(x, y) { x + y };
val add5 := add(5);  // partial application works
```

**Tupled (for interop or when you need it):**

```musi
val add_pair: (Int32, Int32) -> Int32 := fn(pair) { pair.0 + pair.1 };
```

## Writing Code

### Literals

```musi
// Numbers
42          // Int
0xFF        // Int (hex)
0o77        // Int (octal)
0b1011      // Int (binary)
3.14        // Float

// Text
"Hello"             // String (just slice of Runes)
'⌘'                 // Rune (Unicode scalar, actually Nat32)
$"Value: {x + 1}"   // String interpolation

// Booleans
true, false
```

### Operators

**Arithmetic:**

- `+`, `-`, `*`, `/`, `%` (remainder), `mod` (true modulus), `**` (power)

**Bitwise:**

Also, these are non-shorting in logical context.

- `&`, `|`, `^`, `<<`, `>>`, `~` (not)

**Logical:**

- `and`, `or`, `not`

**Comparison:**

- `<`, `>`, `<=`, `>=`, `=`, `/=` (not equal)

**Special:**

- `|>` (pipe, for chaining functions)
- `??` (optional coalesce, like `??` in Swift)

## Data Structures

### Records

Product types with named fields:

```musi
record Point[T] {
  x: T;
  y: T
};

val p := Point.{ x := 10, y := 20 };
val x := p.x;
```

### Sum Types

Tagged unions for when something can be one thing or another:

```musi
sum Option[T] {
  case Some(T),
  case None
};

// use backticks to escape reserved keywords or operators
val `val` := Option.Some(42);
```

### Pattern Matching

Match on values, structure, variants, whatever:

```musi
match `val` {
case Some(x) => x,
case None => 0
};
```

## Control Flow

Everything is expression. Blocks return values.

```musi
val result := if x > 0 { "Positive" } else { "Negative" };

while running {
  process();
};

for item in list {
  writeln(item);
};
```

**Defer** runs code at end of block (LIFO order, like Go):

```musi
val f := open("file.txt");
defer close(f);  // runs when block exits
```

## Systems Programming

### Unsafe Code & FFI

Raw pointers and C functions need `unsafe` blocks (stolen from Rust):

```musi
extern "C" unsafe {
  fn malloc(size: Nat64): ^Any; // because Unit =/= void, Any makes more sense... somehow
  fn free(ptr: ^Any): Any;
};

unsafe {
  val ptr := malloc(16);
  defer free(ptr);
};
```

### Attributes

Compiler hints using `[< ... >]` syntax (stolen from F#):

```musi
[<inline>]
fn fast_add(x: Int32, y: Int32) { x + y };
```

## Why Square Brackets for Generics?

You may/may not wonder why `List[Int]` instead of `List<Int>` like C++ or Rust.

**Three reasons:**

### 1. Parsing is simpler

Angle brackets clash with less-than and greater-than operators. The parser cannot tell if `a<b>` is generic type or comparison without looking ahead or knowing what `a` is. This breaks grammar rules that keep parsing simple.

### 12. No "turbofish" needed

Rust needs `::<>` syntax to disambiguate generics in some contexts. Square brackets avoid this entirely.

### 13. Conceptual consistency

Generics are like "indexing into family of types". So `List[Int]` (type indexing) looks like `array[0]` (value indexing). Same bracket syntax, similar mental model.
