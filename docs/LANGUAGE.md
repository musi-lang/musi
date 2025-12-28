# Musi Language Specification

Musi is statically typed systems programming language that also lets you use dynamic types when you want. It compiles to bytecode and focuses on being simple, safe, and expressive.

## Type System

Musi uses **gradual typing** with local `Hindley-Milner` inference. You can write types explicitly or let compiler figure them out.

### Basic Types

**Integers (signed):**

- `Int8`, `Int16`, `Int32`, `Int64`

**Naturals (unsigned):**

- `Nat8`, `Nat16`, `Nat32`, `Nat64`

**Floats (IEEE-754 binary):**

- `Float16`, `Float32`, `Float64`

**Decimals (IEEE-754 decimal, software):**

- `BigFloat32`, `BigFloat64`, `BigFloat128`

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

Functions are first-class values. The `->` operator constructs **function types** (also called the "function space" or "arrow type").

**Important:** `->` is for *type expressions*, not function definitions. Function definitions use `fn` with either a block `{ }` or expression body `=>`.

```musi
// Block body (multiple statements, implicit return of last expression)
val add: Int -> Int -> Int := fn(x, y) { x + y };

// Expression body (single expression, like C#)
val add: Int -> Int -> Int := fn(x, y) => x + y;

// Fully explicit
val add: Int -> Int -> Int := fn(x: Int, y: Int): Int => x + y;

// Compare to OCaml:
// let add : int -> int -> int = fun x y -> x + y
```

**Right-associative:** `A -> B -> C` means `A -> (B -> C)` (returns a function).

**From type theory:** The `->` notation comes from type theory and mathematical logic, where it represents the function space (the set of all functions from A to B). This is why Musi uses `=>` for match case arrows instead of `->` — to avoid ambiguity between function types and pattern matching. Lean4 makes the same distinction.

**Curried (default way):**

```musi
val add: Int32 -> Int32 -> Int32 := fn(x, y) { x + y };
val add5 := add(5);  // partial application returns Int32 -> Int32
```

**Tupled (for interop or when you need it):**

```musi
val add_pair: (Int32, Int32) -> Int32 := fn(pair) { pair.0 + pair.1 };
```

**Higher-order functions:**

```musi
// map takes a function and returns a function
val map: (A -> B) -> []A -> []B := fn(f, xs) { ... };

// compose takes two functions and returns their composition
val compose: (B -> C) -> (A -> B) -> A -> C := fn(g, f, x) { g(f(x)) };
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

- `+`, `-`, `*`, `/`, `%` (remainder), `**` (power)

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
record Point[T] { x: T, y: T };

val p := Point.{ x := 10, y := 20 };
val x := p.x;
```

### Choice Types

Tagged unions for when something can be one thing or another:

```musi
choice Option[T] {
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

### Foreign Function Interface (FFI)

Import C functions using `extern` modifier on `fn` (no body):

```musi
@[link("libc")]
extern "C" fn malloc(size: Nat64): ^Any;

@[link("libc")]
extern "C" fn free(ptr: ^Any): Unit;
```

**Calling** C functions requires `unsafe` block:

```musi
unsafe {
  val ptr := malloc(16);
  defer free(ptr);
};
```

Export Musi functions to C using `Callback.register`:

```musi
val my_add := fn(x: Int32, y: Int32): Int32 => x + y;
Callback.register("my_add", my_add);
```

### Attributes

Compiler hints using `@[...]` syntax:

```musi
@[inline]
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
