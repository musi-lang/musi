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

### Unsafe Blocks

GC disabled. Manual memory management required. Used for FFI and low-level operations.

```musi
unsafe {
  val ptr: ^Int32 := malloc(4);
  ptr.^ <- 42;
  free(ptr);
};
```

**Restrictions in unsafe blocks:**

- No automatic GC
- Pointer arithmetic allowed
- No bounds checking (unless explicit)
- FFI unrestricted

## Type System Runtime

### Monomorphization

Generics are specialized at compile time. Each instantiation produces a separate concrete implementation.

```musi
fn identity[T](x: T): T { x };
identity[Int32](42);      // generates Int32 version
identity[String]("hi");   // generates String version
```

### Sum Type Layout

Tagged union: `[discriminant:u8][padding][payload]`

```musi
sum Option[T] {
  case Some(T),
  case None
};
```

**Memory**: discriminant (tag) followed by payload data (if present).

### Record Layout

C-compatible struct layout. Fields sequential with alignment.

```musi
record Point {
  x: Bin64;
  y: Bin64
};
```

**Memory**: `[x:8][y:8]` = 16 bytes total.

### Pattern Matching

Compiled to efficient dispatch:

- Small number of cases: condition chain
- Large number of integer cases: jump table (`switch` opcode)
- Exhaustiveness checked at compile time

## Foreign Function Interface

### C Interop

```musi
@[link(name := "c")]
extern "C" unsafe {
  fn malloc(size: Nat64): ^Any;
  fn free(ptr: ^Any): Any;
  fn write(fd: Int32, buf: ^Nat8, count: Nat64): Int64;
};
```

**Calling convention**: C standard (cdecl/System V on Unix, stdcall on Windows)

**Layout compatibility**:

- Musi records ≡ C structs
- Musi pointers ≡ C pointers
- Primitive types match C types

### Exporting Functions

```musi
@[no_mangle]
export fn add_numbers(a: Int32, b: Int32): Int32 {
  return a + b;
};
```

**Effect**: Function exposed with C calling convention, no name mangling.

## Error Handling

**Musi uses**: `defer` + `Expect[T, E]` (Result type)

```musi
sum Expect[T, E] {
  case Ok(T),
  case Err(E)
};

fn divide(a: Int32, b: Int32): Expect[Int32, String] {
  if b = 0 { return Err("division by zero"); };
  return Ok(a / b);
};
```

**Defer**: cleanup on scope exit (normal or error)

```musi
fn process_file(path: String): Expect[Unit, String] {
  val file := open(path).unwrap();
  defer close(file);
  return read(file);
};
```

**Bytecode support for interop**:

- `throw`/`rethrow`: exception-based languages
- `ldnull`: nullable references (Musi has no null)

## Standard Library

### Core Types

- `Never`: bottom type (uninhabited)
- `Unit`: empty tuple
- `Any`: top type (all types are subtypes)
- `Bool`, `Int*`, `Nat*`, `Float*`, `Rune`, `String`
- `BigFloat*`: decimal floating-point (STL implementation using `Float*`)

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
fn swap[T](x: T, y: T): (T, T) {
  return (y, x);
};

val a := swap[Int32](1, 2);        // generates Int32 version
val b := swap[String]("x", "y");   // generates String version
```

### Result Type Error Handling

```musi
val io := import "std/io.ms";

fn safe_divide(a: Int32, b: Int32): Expect[Int32, String] {
  if b = 0 { return Err("division by zero"); };
  return Ok(a / b);
};

match safe_divide(10, 0) {
case Ok(result) => io.writeln($"Result: {result}"),
case Err(msg) => io.writeln($"Error: {msg}")
};
```

### FFI with Defer

```musi
extern "C" unsafe {
  fn fopen(path: ^Nat8, mode: ^Nat8): ^Any;
  fn fclose(file: ^Any): Int32;
  fn fread(buf: ^Any, size: Nat64, count: Nat64, file: ^Any): Nat64;
};

unsafe {
  val file := fopen(@"data.bin", @"rb");
  defer fclose(file);

  val buffer: [1024]Nat8;
  val bytes_read := fread(@buffer, 1, 1024, file);
};
```

### Sum Type Dispatch

```musi
sum Tree[T] {
  case Leaf(T),
  case Node(^Tree[T], ^Tree[T])
};

fn height[T](tree: Tree[T]): Int32 {
  match tree {
  case Leaf(_) => 1,
  case Node(left, right) => 1 + max(height(left.^), height(right.^))
  }
};
```
