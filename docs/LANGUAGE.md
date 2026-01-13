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

**Text:**

- `Rune` (single UTF-32 character)
- `String` (UTF-8 encoded text)

**Logic:**

- `Bool` (true/false)

**Special:**

- `Unit` (empty tuple `()`, like void but better, but also not quite like void)
- `Any` (top type)
- `Never` (bottom type, for things that never return)

### Building Complex Types

- **Generics:** `List[T]`
- **Array:** `[]T` (dynamic slice) or `[N]T` (fixed size)
- **Tuple:** `(A, B, C)`
- **Function:** `A -> B`

**Examples:**

```musi
val i: Int32 := 0;           // immutable binding
val s: String := "hello";
val opt: Option[Int32] := Some(10);
val ptr: Ptr[Int32] := addr_of(i);
val list: List[String] := List.{ head := "a", tail := None };
val func: (Int32) -> Bool := (x) => x > 0;
```

**Mutation:**

```musi
var x: Int32 := 10;         // mutable binding
x <- 20;                    // mutate (assignment with <-)
```

**Note:** `:=` is for initialization (both `val` and `var`), `<-` is for assignment/mutation (only `var`).

### Functions as Types

Functions are first-class values. `->` operator constructs **function types** (also called "function space" or "arrow type").

**Important:** `->` is for *type expressions*, not function literals. Function literals use `(params) => body` syntax.

```musi
// Function with type annotation on binding
val add: Int -> Int -> Int := (x, y) => x + y;

// Expression body (single expression)
val add: (Int, Int) -> Int := (x, y) => x + y;

// Block body (multiple statements, implicit return of last expression)
val add: (Int, Int) -> Int := (x, y) => {
  x + y
};

// Compare to OCaml:
// let add : int -> int -> int = fun x y -> x + y
```

**Right-associative:** `A -> B -> C` means `A -> (B -> C)` (returns function).

**From type theory:** `->` notation comes from type theory and mathematical logic, where it represents function space (the set of all functions from to B). This is why Musi uses `=>` for match case arrows instead of `->` — to avoid ambiguity between function types and pattern matching. Lean4 makes same distinction.

**Curried (default way):**

```musi
val add: Int32 -> Int32 -> Int32 := (x, y) => x + y;
val add5 := add(5);  // partial application returns Int32 -> Int32
```

**Tupled (for interop or when you need it):**

```musi
val add_pair: (Int32, Int32) -> Int32 := (pair) => pair.0 + pair.1;
```

**Higher-order functions:**

```musi
// map takes function and returns function
val map: ((A -> B), []A) -> []B := (f, xs) => { ... };

// compose takes two functions and returns their composition
val compose: ((B -> C), (A -> B)) -> (A -> C) := (g, f) => (x) => g(f(x));
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

- `+`, `-`, `*`, `/`, `%` (remainder)

**Logical:**

- `and`, `or`, `not`

**Comparison:**

- `<`, `>`, `<=`, `>=`, `=` (equality), `/=` (not equal)
- `in` (membership test)

**Type Operators:**

- `in` (type test): `x in Int32`
- `<:` (type cast): `y := x <: Int;` (runtime check if downcast, safe if upcast)

**List Operators:**

- `::` (cons): `head :: tail` (prepend to list)

**Range Operators:**

- `..` (inclusive range): `1..10` (1 to 10)
- `..<` (exclusive range): `1..<10` (1 to 9)

## Data Structures

### Types as Values (Zig-Inspired)

In Musi, **types are first-class values**. You define types using `record` or `choice` expressions and bind them to names using `val`:

```musi
// Define record type (product type)
val Point := record {
  x: Int32,
  y: Int32,

  distance: (self: Point) -> Float32 => (self) =>
    sqrt(self.x * self.x + self.y * self.y),

  move: (self: Point, dx: Int32, dy: Int32) -> Point => (self, dx, dy) => .{
    x := self.x + dx,
    y := self.y + dy
  }
};

// Create instance
val p := Point.{ x := 10, y := 20 };
val d := p.distance();
```

**Key insight:** `record` is expression that returns type value, which you bind to name. This is exactly how Zig's `struct` works.

### Records

Product types with named fields and optional methods:

```musi
// Generic record
val Point := record[T] {
  x: T,
  y: T,

  distance: (self: Point[T]) -> T => (self) => sqrt(self.x * self.x + self.y * self.y)
};

// Anonymous record literal (structural type)
val p := .{ x := 10, y := 20 };

// Named construction
val p2 := Point[Int32].{ x := 5, y := 15 };

// Record update with "with" keyword
val p3 := .{ p with x := 20 };  // Update existing record
```

**Records can contain:**

- Fields (data)
- Methods (functions with explicit `self` parameter)
- Associated constants (`val` declarations)
- Nested type definitions

### Choice Types

Tagged unions for when something can be one thing or another:

```musi
val Option := choice[T] {
  Some(T),
  None,

  unwrap_or: (self: Option[T], default: T) -> T => (self, default) => match self {
    Some(x) => x,
    None => default
  }
};

// Use backticks to escape reserved keywords or operators
val `val` := Option.Some(42);
val result := `val`.unwrap_or(0);
```

### Type Bindings

In Musi, **types are first-class values**. Type aliases are created by binding type values using `val` or `var`:

```musi
val Meters := Float64;
val Callback[T] := T -> Unit;
val Point2D := record { x: Int32, y: Int32 };
```

Since types are values, the same binding syntax works for type definitions, type aliases, and regular values.

```musi
val Point := record { x: Int32, y: Int32 };
val MyPoint := Point;  // type alias - just binds type value to new name
```

### Pattern Matching

Match on values, structure, variants, whatever:

```musi
match value {
  Some(x) => x,
  None => 0
}

// Pattern matching with guards
match point {
  .{ x, y } if x > 0 and y > 0 => "Quadrant I",
  .{ x, y } if x < 0 and y > 0 => "Quadrant II",
  _ => "Other"
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

**Pattern binding in conditionals:**

```musi
val opt: Option[Int32] := Some(42);

if Some(x) := opt {
  writeln($"Got value: {x}");
} else {
  writeln("No value");
};
```

**Labeled loops and `cycle`:**

```musi
#outer: for item in list {
  #inner: for sub in item.items {
    if should_break { cycle #outer; }  // break outer loop
  }
};
```

**Defer** runs code at end of block (LIFO order, like Go):

```musi
val f := open("file.txt");
defer close(f);  // runs when block exits
```

## Modules and Namespacing

### File = Module

Every file is module. No `module` or `namespace` keyword needed.

### Export Syntax

Two styles are supported:

### Export syntax

Use statement-style exports at module boundaries:

```musi
// geometry.ms
val Point := record {
  x: Int32,
  y: Int32,

  distance: (self: Point) -> Float32 => (self) => sqrt(self.x * self.x + self.y * self.y)
};

val Circle := record {
  center: Point,
  radius: Float32,

  area: (self: Circle) -> Float32 => (self) => 3.14159 * self.radius * self.radius
};

// Export at end of file
export { Point, Circle };
```

### Import (ES6-style)

```musi
// Import specific items
import { Point, Circle } from "./geometry";

// Import with renaming
import { Point as P } from "./geometry";

// Use imported types
val p := Point.{ x := 5, y := 10 };
val c := Circle.{ center := p, radius := 5.0 };
```

### Nested Types as Namespaces

Records and choices can contain nested type definitions, creating natural namespaces:

```musi
val Geometry := record {
  val Point := record {
    x: Int32,
    y: Int32,

    distance: (self: Point) -> Float32 => (self) => sqrt(self.x * self.x + self.y * self.y)
  },

  val Circle := record {
    center: Point,
    radius: Float32
  }
};

// Usage
val p := Geometry.Point.{ x := 5, y := 10 };
```

## Structural Interfaces

Interfaces define structural contracts that types satisfy by having matching method signatures. Go/TypeScript style - no explicit implementation declaration needed.

```musi
val Drawable[T] := interface {
  val T;
  draw: (self: T) -> Unit
};

// Type automatically satisfies Drawable if it has matching method
val Point := record {
  x: Int32,
  y: Int32,
  draw: (self: Point) -> Unit := (self) => {
    writeln($"Point at {self.x}, {self.y}")
  }
};
```

Interfaces are just type values:

```musi
val Show[T] := interface {
  val T;
  show: (self: T) -> String
};

val Eq[T] := interface {
  val T;
  eq: (self: T, other: T) -> Bool
};

// Types automatically satisfy if they have matching methods
// No explicit impl declarations needed - structural checking only
```

**Key points:**

- Interfaces define behavior contracts (type signatures)
- Any type with matching method signatures automatically satisfies the interface
- Supports ad-hoc polymorphism without explicit implementations
- "Types are values" philosophy applies - interfaces are bound with `val`

## Systems Programming

### Foreign Function Interface (FFI)

**Import** C functions using `import native` statement:

```musi
import native { malloc, free } from "libc";

@[link(name := "libc")]
import native { write } from "libc";
```

**Calling** C functions requires `unsafe` block:

```musi
unsafe {
  val ptr := malloc(16);
  defer free(ptr);
};
```

### Attributes

Compiler hints using `@[...]` syntax:

```musi
@[inline]
val fast_add: (Int32, Int32) -> Int32 := (x, y) => x + y;
```

## Why Square Brackets for Generics?

You may wonder why `List[Int]` instead of `List<Int>` like C++ or Rust.

**Three reasons:**

### 1. Parsing is simpler

Angle brackets clash with less-than and greater-than operators. parser cannot tell if `a<b>` is generic type or comparison without looking ahead or knowing what `a` is. This breaks grammar rules that keep parsing simple.

### 2. No "turbofish" needed

Rust needs `::<>` syntax to disambiguate generics in some contexts. Square brackets avoid this entirely.

### 3. Conceptual consistency

Generics are like "indexing into family of types". So `List[Int]` (type indexing) looks like `array.[0]` (value indexing). Same bracket syntax, similar mental model.

## Complete Example

```musi
// geometry.ms
val Point := record[T] {
  x: T,
  y: T,

  distance: (self: Point[T]) -> Float64 => (self) => {
    sqrt(self.x * self.x + self.y * self.y)
  },

  move: (self: Point[T], dx: T, dy: T) -> Point[T] => (self, dx, dy) => .{
    x := self.x + dx,
    y := self.y + dy
  },

  val zero: Point[Int32] := .{ x := 0, y := 0 }
};

val Shape := choice {
  Circle(center: Point[Int32], radius: Float64),
  Rect(p1: Point[Int32], p2: Point[Int32]),

  area: (self: Shape) -> Float64 => (self) => match self {
    Circle(_, r) => 3.14159 * r * r,
    Rect(p1, p2) => abs((p2.x - p1.x) * (p2.y - p1.y))
  }
};

export { Point, Shape };

// main.ms
import { Point, Shape } from "./geometry";

val p := Point[Int32].{ x := 5, y := 10 };
val d := p.distance();

val c := Shape.Circle(Point.zero, 10.0);
val area := c.area();

writeln($"Point distance: {d}");
writeln($"Circle area: {area}");
```
