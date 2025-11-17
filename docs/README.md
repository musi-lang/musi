# Musi Language Documentation

> Stack-only systems programming language

## Philosophy

Musi is designed for **clarity, safety, and performance** with a stack-only memory model. No heap allocation, no garbage collection, no runtime overhead.

### Core Principles

1. **Stack-only memory** - All data on stack, compile-time lifetimes
2. **Immutability by default** - `val` immutable, `var` explicit mutable
3. **Expression-oriented** - Everything yields a value
4. **Strong static typing** - With inference
5. **Zero runtime overhead** - No allocator, no GC, no ARC
6. **Explicit over implicit** - Clear, readable code

## Quick Start

### Hello World

```musi
writeln("Hello, Musi!");
```

### Basic Example

```musi
val Counter := record {
  var value: Nat;
};

val inc := fn(ref var c: Counter) {
  c.value <- c.value + 1;
};

val show := fn(ref c: Counter) {
  writeln(`Counter: ${c.value}`);
};

var counter := Counter{ .value := 0 };
inc(ref var counter);
inc(ref var counter);
show(ref counter);
```

## Key Features

### Stack-Only Memory

All values stored on stack with compile-time known sizes:

```musi
val x: Nat := 42;                    // 8 bytes
val arr: [Nat; 100] := [0; 100];     // 800 bytes
val s: Str<32> := "hello";           // 32 runes + length
```

### Borrowing

References to stack values:

```musi
val add := fn (ref x: Int, ref y: Int) -> Int {  // immutable borrow
  x + y
};

val inc := fn (ref var x: Int) {                 // mutable borrow
  x <- x + 1;
};
```

### Pattern Matching

```musi
val Shape := choice Shape {
  case Circle(radius: Bin32),
  case Rectangle(width: Bin32, height: Bin32),
};

val area := fn(ref shape: Shape) -> Bin32 {
  match shape with {
  case .Circle(r) -> 3.14159 * r * r,
  case .Rectangle(w, h) -> w * h,
  }
};
```

### Optionals & Fallible Types

```musi
val maybe: Nat? := .Some(42);
val nothing: Nat? := .None;

val result: Expect<Nat, Error> := .Pass(42);
val failure: Expect<Nat, Error> := .Fail(err);
```

### String Types

**Fixed-size (`Str<N>`):**

```musi
val Str<N: Nat> := record {
  data: [Rune; N];
  len: Nat;
};

val name: Str<32> := "Alice";
```

**Growable (StrBuf):**

```musi
val StrBuf := record {
  ptr: Ptr<Rune>;
  var len: Nat;
  var cap: Nat;
};

Arena.with(fn(ref arena: Arena) {
  var text := StrBuf.new(ref arena);
  text.push_str("Hello");
});
```

## Documentation Structure

- **[SYNTAX.md](SYNTAX.md)** - Complete syntax reference
- **[MEMORY.md](MEMORY.md)** - Stack-only model, borrowing, lifetimes
- **[TYPES.md](TYPES.md)** - Type system, generics, inference
- **[FFI.md](FFI.md)** - Foreign function interface
- **[COMPILER.md](COMPILER.md)** - Compiler architecture
- **[RUNTIME.md](RUNTIME.md)** - VM architecture
- **[EXAMPLES.md](EXAMPLES.md)** - Code examples
- **[PROGRESS.md](PROGRESS.md)** - Implementation status

## Design Decisions

### Why Stack-Only?

- **Zero overhead** - No allocator, no GC, no ARC
- **Predictable** - Constant-time allocation/deallocation
- **Simple** - No cycles, no leaks, no fragmentation
- **Fast** - Stack allocation is bump-pointer fast
- **Embedded-friendly** - Works without OS

## License

See [LICENSE](../LICENSE) for details.
