# Musi Language Specification

Musi is a statically-typed systems programming language with TypeScript-like gradual typing semantics. Musi is interpreted at runtime with ECMAScript-style module system. Files are modules without main entry points—executed directly like JavaScript/Deno/Bun scripts. The language focuses on being simple, safe, and expressive.

## 1. Lexical Syntax

### 1.1 Identifiers

Identifiers follow standard naming conventions:

```musi
val foo := 42;
val bar_baz := "hello";
val `val` := Some(42);  // backticks escape reserved keywords
```

### 1.2 Literals

**Numbers:**

```musi
42          // decimal integer
0xFF        // hexadecimal
0o77        // octal
0b1011      // binary
3.14        // floating point
```

**Text:**

```musi
"Hello"             // String (UTF-8)
'⌘'                 // Rune (single UTF-32 character)
$"Value: {x + 1}"   // Template string with interpolation
```

**Arrays and Tuples:**

```musi
[1, 2, 3]           // array literal
(1, "hello", True)  // tuple literal
```

## 2. Modules and Imports

### 2.1 Module System

Every file is a module. No `module` or `namespace` keyword needed.

### 2.2 Export Syntax

Export items from the current module:

```musi
export { Point, Circle, area };
```

### 2.3 Import Syntax

Import items from other modules (ES6-style):

```musi
import { Point, Circle } from "./geometry";
import { Point as P } from "./geometry";
```

## 3. Declarations and Bindings

### 3.1 Immutable and Mutable Bindings

```musi
val x := 42;              // immutable binding
var y := "hello";         // mutable binding
y <- "world";             // assignment to mutable
```

### 3.2 Type Annotations

Types are optional due to gradual typing:

```musi
val x: Int[32] := 42;
val y := "inferred as String";
```

### 3.3 Functions

Two syntax forms for functions:

```musi
// Arrow function (expression body)
val add := fn(x: Int[32], y: Int[32]) => x + y;

// Block function (statement body)
val greet := fn(name: String) {
  val greeting := $"Hello, {name}!";
  writeln(greeting);
};
```

### 3.4 Records

Record types define product types with named fields:

```musi
val Point := record {
  x: Int[32],
  y: Int[32]
};

// Create instance with object literal
val p := .{ x := 10, y := 20 };

// Record update
val p2 := .{ p with x := 30 };
```

### 3.5 Choice Types

Choice types define tagged unions:

```musi
val Option[T] := choice {
  Some(T),
  None
};

val opt := Some(42);
val nothing := None;
```

### 3.6 Opaque Types

Opaque type aliases hide the underlying type representation outside the defining module:

```musi
// Inside user_id.ms
opaque val UserId := String;

val create_user_id := fn(s: String): UserId => {
  if validate_id(s) { s as UserId } else { panic("Invalid ID") }
};

export { UserId, create_user_id };

// Outside module - UserId is opaque
import { UserId, create_user_id } from "./user_id";

val id := create_user_id("user_123");
// id ++ "_suffix"  // ERROR: UserId is not String outside module
```

This enables:

- **Newtypes**: Wrap primitives with semantic meaning (UserId, Email, Meters)
- **Information hiding**: Expose only safe operations
- **FFI handles**: Wrap native pointers in opaque types

### 3.7 Structural Typing

Records define structural contracts. Any type with matching structure is compatible:

```musi
val Drawable := record {
  draw: Self -> Unit
};

val Circle := record {
  radius: Float[64],
  draw: Circle -> Unit
};

// Circle is compatible with Drawable (structural subtyping)
val d: Drawable := circle_instance;
```

## 4. Expressions

### 4.1 Block Expressions

Blocks are expressions that return their last value:

```musi
val result := {
  val x := 10;
  val y := 20;
  x + y  // returns 30
};
```

### 4.2 Conditional Expressions

```musi
val sign := if x > 0 { "positive" } else if x < 0 { "negative" } else { "zero" };

// Pattern binding in condition (requires val or var)
if val Some(x) := opt {
  writeln($"Got: {x}");
} else {
  writeln("Nothing");
};

// Mutable pattern binding
if var Some(x) := opt {
  x <- x + 1;
  writeln($"Incremented: {x}");
};
```

### 4.3 Loop Expressions

```musi
while condition {
  // body
};

for item in collection {
  // body
};
```

### 4.4 Loop Labels

Label loops for control from nested loops:

```musi
outer: while cond {
  inner: for i in 0..10 {
    if done { cycle :outer; };  // continue outer loop
    if exit { break :inner; };  // break inner loop
  }
};
```

### 4.5 Defer Expressions

Defer runs code at block exit (LIFO order):

```musi
val f := open("file.txt");
defer close(f);  // runs when block exits
```

### 4.6 Match Expressions

Pattern matching with exhaustiveness checking:

```musi
match opt {
  Some(x) -> x,
  None -> 0
};

match point {
  .{ x := px, y := py } if px > 0 -> "Quadrant I",
  .{ x := px } if px < 0 -> "Quadrant II",
  _ -> "Other"
};
```

## 5. Operators

### 5.1 Arithmetic

```musi
+   // addition
-   // subtraction
*   // multiplication
/   // division
%   // remainder
```

### 5.2 Bitwise and Logical

```musi
and  // bitwise AND / logical AND (context-dependent)
or   // bitwise OR / logical OR
xor  // bitwise XOR / logical XOR
not  // bitwise NOT / logical NOT
shl  // shift left
shr  // shift right
rol  // rotate left
ror  // rotate right
```

### 5.3 Comparison

```musi
=   // equality
/=  // not equal
<   // less than
>   // greater than
<=  // less than or equal
>=  // greater than or equal
in  // membership test
```

### 5.4 Type Operators

```musi
as  // type cast (runtime check for downcast, safe for upcast)
```

### 5.5 List Operators

```musi
::   // cons: prepend element to list
```

### 5.6 Range Operators

```musi
..   // inclusive range: 1..10
..<  // exclusive range: 1..<10
```

### 5.7 Assignment

```musi
<-   // assignment to mutable variable
```

## 6. Type System

### 6.1 Basic Types

**Integers (signed):**

```musi
Int[N]  // parametric integer (N = bit width)
Int[8], Int[16], Int[32], Int[64]

// Type aliases
val Int8 := Int[8];
val Int32 := Int[32];
```

**Naturals (unsigned):**

```musi
Nat[N]  // parametric natural
Nat[8], Nat[16], Nat[32], Nat[64]

val Nat8 := Nat[8];
val Nat32 := Nat[32];
```

**Floats (IEEE-754 binary):**

```musi
Float[N]  // parametric float
Float[32], Float[64]

val Float32 := Float[32];
val Float64 := Float[64];
```

**Text:**

```musi
Rune    // single UTF-32 character
String  // UTF-8 encoded text
```

**Logic:**

```musi
Bool  // True or False (sum type)
```

**Special:**

```musi
Unit   // empty tuple (), like void but better
Any       // top type - all types are subtypes of Any
Nothing   // bottom type - Nothing is subtype of all types
```

### 6.2 Complex Types

**Generics:**

```musi
List[T]
Option[T]
HashMap[K, V]
```

**Arrays:**

```musi
[]T        // dynamic slice
[N]T      // fixed-size array
```

**Tuples:**

```musi
(A, B, C)
```

**Functions:**

```musi
fn(A, B) -> C         // function type
fn(A) -> fn(B) -> C   // curried function (right-associative)
A -> B -> C           // shorthand for curried function
```

### 6.3 Type Inference

Musi uses local bidirectional type inference. Types can be explicitly annotated or inferred:

```musi
val x := 42;            // inferred as Int[32]
val y: Int[64] := 42;   // explicit annotation
```

> NOTE: If type inference fails, you can always provide an explicit type annotation, or else it defaults to `Any`.

### 6.4 Gradual Typing

The `Any` type enables opt-out of static type checking (unless you toggle `noImplicitAny` in `compilerOptions`):

```musi
val dynamic: Any := 42;
val back: Int[32] := dynamic;  // runtime type check
```

### 6.5 Structural Subtyping

Records use structural subtyping:

```musi
val Point := record { x: Int[32], y: Int[32] };
val NamedPoint := record { x: Int[32], y: Int[32], name: String };

// NamedPoint is subtype of Point (has all required fields)
val p: Point := named_point_instance;
```

## 7. FFI and Native Code

### 7.1 Native Functions

Declare externally implemented functions:

```musi
// SAFETY: Function signature matches libc malloc
#[link(name := "libc")]
native "C" fn malloc(size: Int[64]): Ptr[Nat8];

#[link(name := "libc")]
native "C" fn free(ptr: Ptr[Nat8]);
```

### 7.2 Native Types

Define opaque handles for external resources:

```musi
native record NativeHandle;

opaque val GLTexture := NativeHandle;
opaque val FileHandle := NativeHandle;
```

### 7.3 Exporting to Native Code

```musi
#[no_mangle]
native "C" val add_numbers := fn(a: Int[32], b: Int[32]): Int[32] => a + b;

export { add_numbers };
```

### 7.4 Memory Safety

Pointers are opaque - no pointer arithmetic allowed. Access through slices:

```musi
native fn ptr_to_slice[T](ptr: Ptr[T], len: Int[64]): []T;

val ptr := malloc(100);
val slice := ptr_to_slice[Nat8](ptr, 100);
slice.[0] <- 42;  // bounds-checked access
```

See `UNSAFE-AND-FFI.md` for detailed FFI documentation.

## 8. Attributes

Attributes provide compiler hints and metadata:

```musi
#[inline]
fn fast_add(x: Int[32], y: Int[32]) => x + y;

#[deprecated("use `new_function` instead")]
val old_function := fn() {};

#[link(name := "mylib")]
native "C" fn external_func();

#[no_mangle]
native "C" val exported_func := fn() {};
```

## 9. Modifiers

Modifiers affect the behavior of declarations:

```musi
export    // export from module
native    // external implementation (FFI)
opaque    // hide type representation
```

## 10. Standard Library

### 10.1 Prelude

The `prelude.ms` module is imported implicitly, providing common types and functions:

```musi
// Automatically available
val Option[T] := choice { Some(T), None };
val Result[T, E] := choice { Ok(T), Err(E) };
val List[T] := /* ... */;
```

### 10.2 Explicit Imports

Other standard library modules require explicit import:

```musi
import { List, HashMap } from "std/collections";
import { read_file, write_file } from "std/fs";
```

## 11. Complete Example

```musi
// geometry.ms
val Point := record {
  x: Int[32],
  y: Int[32]
};

val distance := fn(p: Point) {
  val squared := p.x * p.x + p.y * p.y;
  sqrt(squared)
};

val Shape := choice {
  Circle(center: Point, radius: Float[64]),
  Rect(p1: Point, p2: Point)
};

val shape_area := fn(s: Shape) => match s {
  Circle(_, r) -> 3.14159 * r * r,
  Rect(p1, p2) -> abs((p2.x - p1.x) * (p2.y - p1.y))
};

export { Point, distance, Shape, shape_area };

// main.ms
import { Point, distance, Shape, shape_area } from "./geometry";

val p: Point := .{ x := 5, y := 10 };
val d := distance(p);

val c := Shape.Circle(p, 10.0);
val area := shape_area(c);

writeln($"Point distance: {d}");
writeln($"Circle area: {area}");
```

Run with:

```bash
musi run main.ms
```
