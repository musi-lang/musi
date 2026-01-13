# Musi Runtime System

## Execution Model

**Stack-based virtual machine** executing `.mso` bytecode.

### Components

- **Operand stack**: temporary values during expression evaluation
- **Local slots**: function arguments and local variables
- **Call frames**: per-function context (locals, return address, previous frame)
- **Heap**: garbage-collected objects (records, arrays, strings)

### Evaluation

Everything is an expression that produces a value. Blocks return their last expression.

```musi
val x: Int32 := { val a := 1; a + 2 };  // x = 3
val y: Int32 := if cond { 10 } else { 20 };
```

## Compilation Pipeline

```text
Source → Lex → Parse → Sema → Codegen
```

- **Lex**: tokenization
- **Parse**: AST construction with type expressions
- **Sema**: type checking, inference (local HM), desugaring
- **Codegen**: emit `.mso` bytecode

## Memory Management

### Garbage Collection (Default)

Automatic GC for heap objects. No manual deallocation required.

```musi
val arr: []Int32 := [1, 2, 3];    // GC-managed
val point: Point := .{x := 1.0, y := 2.0};  // GC-managed
```

**Restrictions in unsafe blocks:**

- Pointer arithmetic allowed (but recommended to use array slices instead)
- No bounds checking (unless explicit)
- FFI unrestricted

## Type System Runtime

### Monomorphization

Generics are specialized at compile time. Each instantiation produces a separate concrete implementation.

```musi
val identity[T]: (T) -> T := (x) => x;
identity[Int32](42);      // generates Int32 version
identity[String]("hi");   // generates String version
```

### Sum Type Layout

Tagged union: `[discriminant:u8][padding][payload]`

```musi
choice Option[T] {
  Some(T),
  None
};
```

**Memory**: discriminant (tag) followed by payload data (if present).

### Record Layout

C-compatible struct layout. Fields sequential with alignment.

```musi
record Point { x: Bin64, y: Bin64 };
```

**Memory**: `[x:8][y:8]` = 16 bytes total.

### Pattern Matching

Compiled to efficient dispatch:

- Small number of cases: condition chain
- Large number of integer cases: jump table (`switch` opcode)
- Exhaustiveness checked at compile time

## Foreign Function Interface

### Importing C Functions

Use `import native` statement to bring in external functions:

```musi
import native { malloc, free } from "libc";

@[link(name := "libc")]
import native { write } from "libc";

// Symbol override when C name differs from Musi name
@[link(name := "libc"), symbol(name := "__strdup")]
import native { strdup } from "libc";
```

**Attributes**:

- `@[link(name := "lib")]` — library to link against
- `@[symbol(name := "name")]` — C symbol name (defaults to function name)

**Calling** requires `unsafe` block:

```musi
import native { malloc, free } from "libc";

unsafe {
  val ptr := malloc(64);
  defer free(ptr);
};
```

**Layout compatibility**:

- Musi records ≡ C structs
- Musi pointers ≡ C pointers
- Primitive types match C types

## Error Handling

**Musi uses**: `defer` + `Expect[T, E]` (Result type)

```musi
choice Expect[T, E] {
  Ok(T),
  Err(E)
};

val divide: (Int32, Int32) -> Expect[Int32, String] := (a, b) => {
  if b = 0 { return Err("division by zero"); };
  return Ok(a / b);
};
```

**Defer**: cleanup on scope exit (normal or error)

```musi
val process_file: (String) -> Expect[Unit, String] := (path) => {
  val file := open(path).unwrap();
  defer close(file);
  return read(file);
};
```

**Bytecode support for interop**:

- `throw`/`rethrow`: exception-based languages
- `ldnil`: optional/nullable references (Musi has no null)

## Standard Library

### Core Types

- `Never`: bottom type (uninhabited)
- `Unit`: empty tuple
- `Any`: top type (all types are subtypes)
- `Bool`, `Int*`, `Nat*`, `Float*`, `Rune`, `String`

### Standard Modules

Must be explicitly imported:

```musi
import "std/io.ms";           // I/O operations (FFI-based)
import "std/mem.ms";          // memory utilities
import "std/math.ms";         // mathematical functions
import "std/collections.ms";  // vectors, maps, sets
```

**Usage:**

```musi
val io := import "std/io.ms";
io.writeln("Hello, world!");
```

## Performance Model

### Interpreter

Current VM is bytecode interpreter:

- Fast startup (no compilation overhead)
- Predictable performance (no JIT warmup)
- Portable (runs on any VM implementation)

### Future Optimizations

- JIT compilation for hot paths
- AOT compilation to native executables
- Profile-guided optimization

## Debugging

### Debug Information

Optional metadata in `.mso` files:

- Source locations (file, line, column)
- Variable names and types
- Call stack info for traces

### Runtime Inspection

- Type reflection
- Object layout queries
- GC statistics

## Bytecode Format

See [BYTECODE.md](./BYTECODE.md) for complete specification.

**Key features:**

- Binary format with header, constant pool, symbol table, code section
- Magic number: `0x4D555349` (ASCII "MUSI")
- Stack-based instruction set
- Exception handling blocks (for interop)
- Defer blocks (Musi's error handling)
- Optimization hints (inline, specialize, unroll)

## Examples

### Generic Function with Monomorphization

```musi
val swap[T]: (T, T) -> (T, T) := (x, y) => {
  return (y, x);
};

val a := swap[Int32](1, 2);        // generates Int32 version
val b := swap[String]("x", "y");   // generates String version
```

### Result Type Error Handling

```musi
import "std/io";

val safe_divide: (Int32, Int32) -> Expect[Int32, String] := (a, b) => {
  if b = 0 { return Err("division by zero"); };
  return Ok(a / b);
};

match safe_divide(10, 0) {
  Ok(result) => writeln($"Result: {result}"),
  Err(msg) => writeln($"Error: {msg}")
};
```

### FFI with Defer

```musi
extern fn fopen(path: ^Nat8, mode: ^Nat8): ^Any;
extern fn fclose(file: ^Any): Int32;
extern fn fread(buf: ^Any, size: Nat64, count: Nat64, file: ^Any): Nat64;

unsafe {
  val file := fopen(@"data.bin", @"rb");
  defer fclose(file);

  val buffer: [1024]Nat8;
  val bytes_read := fread(@buffer, 1, 1024, file);
};
```

### Sum Type Dispatch

```musi
choice Tree[T] {
  Leaf(T),
  Node(^Tree[T], ^Tree[T])
};

val height[T]: (Tree[T]) -> Int32 := (tree) => {
  match tree {
    Leaf(_) => 1,
    Node(left, right) => 1 + max(height(left.^), height(right.^))
  }
};
```
