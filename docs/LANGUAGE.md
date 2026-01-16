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

### 1.3 Keywords and Symbols

Keywords: `val`, `var`, `fn`, `if`, `else`, `match`, `while`, `for`, `in`, `return`, `break`, `cycle`, `defer`, `record`, `choice`, `import`, `export`, `unsafe`, `and`, `or`, `xor`, `not`, `in`, `as`, `with`, `shl`, `shr`, `rol`, `ror`

Symbols: `:=`, `<-`, `=>`, `->`, `::`, `..`, `..<`, `=`, `/=`, `<`, `>`, `<=`, `>=`, `.`, `.[`, `.^`, `(`, `)`, `{`, `}`, `[`, `]`, `,`, `;`, `:`, `#[`, `@`

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
val x: Int32 := 42;
val y := "inferred as String";
```

### 3.3 Functions

Two syntax forms for functions:

```musi
// Arrow function (expression body)
val add := fn (x: Int32, y: Int32) => x + y;

// Block function (statement body)
val greet := fn (name: String) => {
  val greeting := $"Hello, {name}!";
  writeln(greeting);
};
```

### 3.4 Records

Record types define product types with named fields:

```musi
val Point := record {
  x: Int32,
  y: Int32
};

// Create instance with object literal
val p := .{ x := 10, y := 20 };

// Record update
val p2 := .{ p with x := 30 };
```

### 3.5 Choice Types

Choice types define tagged unions:

```musi
val Option['T] := choice {
  Some('T),
  None
};

val opt := Some(42);
val nothing := None;
```

### 3.6 Structural Typing

Records define structural contracts. Any type with matching structure is compatible:

```musi
val Drawable := record {
  draw: Self -> Unit
};

val Circle := record {
  radius: Float64,
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
in  // type test: `x in Int32`
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
Int // platform-dependent
Int8, Int16, Int32, Int64
```

**Naturals (unsigned):**

```musi
Nat // platform-dependent
Nat8, Nat16, Nat32, Nat64
```

**Floats (IEEE-754 binary):**

```musi
Float // platform-dependent
Float32, Float64
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
Any    // top type - all types are subtypes of Any
Never  // bottom type - Never is subtype of all types
```

### 6.2 Complex Types

**Generics:**

```musi
List['T]
Option['T]
HashMap['K, 'V]
```

**Arrays:**

```musi
[]T        // dynamic slice
['N]T       // fixed-size array
```

**Tuples:**

```musi
(A, B, C)
```

**Functions:**

```musi
fn(A, B) -> C         // function type
fn(A) -> fn(B) -> C   // curried function (right-associative)
```

### 6.3 Type Inference

Musi uses local bidirectional type inference. Types can be explicitly annotated or inferred:

```musi
val x := 42;        // inferred as Int32
val y: Int64 := 42; // explicit annotation
```

> NOTE: If type inference fails, you can always provide an explicit type annotation, or else it defaults to `Any`.

### 6.4 Gradual Typing

The `Any` type enables opt-out of static type checking (unless you toggle `noImplicitAny` in `compilerOptions`):

```musi
val dynamic: Any := 42;
val back: Int32 := dynamic;  // runtime type check
```

### 6.5 Structural Subtyping

Records use structural subtyping:

```musi
val Point := record { x: Int32, y: Int32 };
val NamedPoint := record { x: Int32, y: Int32, name: String };

// NamedPoint is subtype of Point (has all required fields)
val p: Point := named_point_instance;
```

## 7. Attributes

Attributes provide compiler hints and metadata:

```musi
#[inline]
fn fast_add(x: Int32, y: Int32) => x + y;

#[deprecated("use `new_function` instead")]
val old_function := fn() => { };
```

## 8. Modifiers

Modifiers affect the behavior of declarations:

```musi
export    // export from module
unsafe    // allow unsafe operations (FFI, pointer arithmetic)
```

## 9. Standard Library

### 9.1 Prelude

The `prelude.ms` module is imported implicitly, providing common types and functions:

```musi
// Automatically available
val Option['T] := choice { Some(T), None };
val Result['T, 'E] := choice { Ok(T), Err(E) };
val List['T] := /* ... */;
```

### 9.2 Explicit Imports

Other standard library modules require explicit import:

```musi
import { List, HashMap } from "std/collections";
import { read_file, write_file } from "std/fs";
```

## 10. Complete Example

```musi
// geometry.ms
val Point := record {
  x: Int32,
  y: Int32
};

val distance := fn (p: Point) => {
  val squared := p.x * p.x + p.y * p.y;
  sqrt(squared)
};

val Shape := choice {
  Circle(center: Point, radius: Float64),
  Rect(p1: Point, p2: Point)
};

val shape_area := fn (s: Shape) => match s {
  Circle(_, r) -> 3.14159 * r * r,
  Rect(p1, p2) -> abs((p2.x - p1.x) * (p2.y - p1.y))
};

export { Point, distance, Shape, shape_area };

// main.ms
import { Point, distance, Shape, shape_area} from "./geometry";

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

## 11. Syntax Summary

### 11.1 Expressions

| Form | Syntax |
|------|--------|
| Block | `{ stmt* expr? }` |
| If | `if cond expr_block (else if cond expr_block)* (else expr_block)?` |
| While | `(label:)? while cond (if guard)? expr_block` |
| For | `(label:)? for pat in expr (if guard)? expr_block` |
| Match | `match expr { pat (if guard)? (-> expr)?,+ }` |
| Return | `return expr?` |
| Break | `break (:label \| expr)?` |
| Cycle | `cycle (:label)? (if guard)?` |
| Defer | `defer expr` |
| Record | `record { field,+ }` |
| Choice | `choice { case,+ }` |
| Function | `fn (params) => expr` or `fn (params) expr_block` |
| Binding | `(export\|unsafe)? (val\|var) pat (: ty_expr)? (:= expr)?` |

### 11.2 Patterns

| Form | Syntax |
|------|--------|
| Identifier | `ident` |
| Literal | `lit` |
| Wildcard | `_` |
| Tuple | `(pat,*)` |
| Array | `[pat,*]` |
| Object | `.{ field := pat, ... }` or `TypeName.{ field := pat, ... }` |
| Variant | `ident (ty_args)? (pat,*)?` |
| Cons | `pat :: pat` |
| Or | `pat \| pat` |
| As | `pat as ident` |

### 11.3 Type Expressions

| Form | Syntax |
|------|--------|
| Identifier | `ident` |
| Tuple | `(ty_expr, ty_expr, ...)` |
| Application | `ident ["'", ty_expr, ...]` |
| Array | `[lit?] ty_expr` |
| Function | `ty_expr -> ty_expr` |
| Pointer | `^ty_expr` |
