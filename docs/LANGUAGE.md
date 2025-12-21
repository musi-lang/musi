# Musi Language Specification

Musi is a statically typed, compiled systems programming language with a focus on simplicity, safety, and expressiveness. It features gradual typing, Hindley-Milner type inference, and a syntax inspired by modern OCaml, Rust, and TypeScript.

## Type System

Musi uses a **Gradual Typing** system with local type inference.

### primitives

- **Integers**: `Int8`, `Int16`, `Int32`, `Int64` (Signed)
- **Naturals**: `Nat8`, `Nat16`, `Nat32`, `Nat64` (Unsigned)
- **Floats**: `Bin16`, `Bin32`, `Bin64` (IEEE-754)
- **Decimals**: `Dec32`, `Dec64`, `Dec128` (Software decimal floating point)
- **Text**: `Rune` (UTF-32 scalar), `String` (UTF-8 seq encoding)
- **Logic**: `Bool` (true/false)
- **Special**: `Unit` (empty tuple `()`), `Any` (dynamic top type), `Never` (bottom type)

### Type Constructors

Musi provides standard constructors for composing types.

- **Generics**: `List[T]`
- **Optional**: `?T` (Isomorphic to `Option[T]`)
- **Pointer**: `^T` (Unsafe raw pointer)
- **Array**: `[]T` (Dynamic slice) or `[N]T` (Fixed size array)
- **Tuple**: `(A, B, C)`
- **Function**: `A -> B`

**Examples:**

```musi
val i: Int32 := 0;
val s: String := "hello";
val opt: ?Int32 := 10;
val ptr: ^Int32 := @i;      // Address-of operator
val list: List[String] := List.{ head := "a", tail := None };
val func: Int32 -> Bool := fn(x) { x > 0 };
```

### Function Types

Musi functions are first-class citizens. The `->` operator is right-associative, supporting Currying.

1. **Curried**: `A -> B -> C` (Idiomatic).
2. **Tupled**: `(A, B) -> C` (Used for interop or specific grouping).

```musi
// Curried (Default)
val add: Int32 -> Int32 -> Int32 := fn(x, y) { x + y };
val add5 := add(5); // Partial application

// Tupled
val add_pair: (Int32, Int32) -> Int32 := fn(pair) { pair.0 + pair.1 };
```

---

## Lexical Structure

### Literals

Musi supports a rich set of literals for numeric and textual data.

```musi
// Numbers
42          // Decimal
0xFF        // Hex
0o77        // Octal
0b1011      // Binary
3.14        // Float

// Text
"Hello"             // String
'⌘'                 // Rune
$"Value: {x + 1}"   // String Interpolation

// Logic
true, false
```

### Operators

Standard arithmetic and logical operators are available.

- **Arithmetic**: `+`, `-`, `*`, `/`, `%` (remainder), `mod` (modulus), `**` (Power)
- **Bitwise**: `&`, `|`, `^`, `<<`, `>>`, `~` (Not)
- **Logical**: `and`, `or`, `not`
- **Comparison**: `<`, `>`, `<=`, `>=`, `=`, `/=` (Not Equal)
- **Pipe**: `|>` (Function application pipeline)
- **Coalesce**: `??` (Null/Optional coalesce)

---

## Data Structures

### Records

Structs with named fields.

```musi
record Point[T] {
  x: T;
  y: T
};

val p := Point.{ x := 10, y := 20 };
val x := p.x;
```

### Sum Types (Variants)

Tagged unions for expressing distinct cases.

```musi
sum Option[T] {
  case Some(T),
  case None
};

val `val` := Option.Some(42);
```

### Pattern Matching

Robust matching on literals, structure, and variants.

```musi
match val {
case Some(x) => x,
case None => 0
};
```

---

## Control Flow

Musi is an expression-oriented language. `if`, `match`, and blocks return values.

```musi
val result := if x > 0 { "Positive" } else { "Negative" };

while running {
  process();
};

for item in list {
  writeln(item);
};
```

**Defer** statements run at the end of the enclosing block (LIFO order).

```musi
val f := open("file.txt");
defer close(f);
```

---

## System Programming

### Unsafe & FFI

Access to raw pointers and C ABI is governed by `unsafe` blocks.

```musi
extern "C" unsafe {
  fn malloc(size: Nat64): ^Any;
  fn free(ptr: ^Any): Any;
};

unsafe {
  val ptr := malloc(16);
  defer free(ptr);
};
```

### Attributes

Compiler directives using `[< ... >]` syntax (borrowed from F#).

```musi
[<inline>]
fn fast_add(x: Int32, y: Int32) { x + y };
```

---

## Design Rationale

### Generic Syntax: Square Brackets `[]`

Musi uses square brackets for generics (e.g., `List[Int]`, `fn identity[T]`) instead of the C++-style angle brackets (`List<Int>`).

**Reasoning:**

1. **Strict Context-Free Grammar (CFG)**: Angle brackets introduce an ambiguity with "Less Than" and "Greater Than" operators (`a < b`). Resolving this often requires infinite lookahead or semantic information (checking if `a` is a type) during parsing, which violates strict CFG rules.
2. **No "Turbofish"**: By using distinct delimiters `[]`, we avoid the need for special disambiguation syntax like Rust's `::<>`.
3. **Unified "indexing" Semantics**: Generics can be conceptually viewed as "indexing into a family of types". Syntactically, `List[Int]` (Type Indexing) parallels `array[0]` (Value Indexing).
