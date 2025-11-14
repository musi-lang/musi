# Musi Language Knowledge Base

LLM-focused documentation for implementing and understanding the Musi programming language.

## Purpose

This knowledge base is designed for AI assistants (LLMs) to:
- Understand Musi's syntax and semantics
- Implement compiler features
- Implement runtime features
- Understand design decisions
- Reference similar languages

## Structure

### [syntax/](syntax/)
Current language syntax (single source of truth)
- [bindings.md](syntax/bindings.md) - val/var bindings
- [types.md](syntax/types.md) - Type system
- [patterns.md](syntax/patterns.md) - Pattern matching
- [control-flow.md](syntax/control-flow.md) - Loops, conditionals
- [ffi.md](syntax/ffi.md) - Foreign function interface

### [semantics/](semantics/)
Language behavior and rules
- [memory-model.md](semantics/memory-model.md) - ARC memory management
- [type-system.md](semantics/type-system.md) - Type inference, checking

### [compiler/](compiler/)
OCaml compiler implementation
- [pipeline.md](compiler/pipeline.md) - Compilation phases
- [lexer.md](compiler/lexer.md) - Tokenization
- [parser.md](compiler/parser.md) - AST construction

### [runtime/](runtime/)
Zig VM implementation
- [vm-arch.md](runtime/vm-arch.md) - VM architecture
- [opcodes.md](runtime/opcodes.md) - Instruction set

### [decisions/](decisions/)
Design rationale (why, not what)
- [val-var-const.md](decisions/val-var-const.md) - Binding keywords
- [arc-choice.md](decisions/arc-choice.md) - Memory management
- [no-inheritance.md](decisions/no-inheritance.md) - Composition over inheritance

### [examples/](examples/)
Reference implementations from other languages
- [swift-arc.md](examples/swift-arc.md) - Swift's ARC implementation
- [rust-ownership.md](examples/rust-ownership.md) - Rust's ownership model

## Quick Reference

### Current Implementation Status
See [PROGRESS.md](PROGRESS.md) for detailed status.

**Working:**
- Lexer (basic)
- Parser (basic)
- Type system (basic)
- Instruction set (defined)

**In Progress:**
- val/var migration
- Semantic analysis
- Code generation

**TODO:**
- Zig VM
- ARC implementation
- FFI bridge
- Optimizations

### Key Design Principles

1. **Immutability by default** - val (immutable), var (explicit mutable)
2. **ARC memory management** - Automatic, deterministic, predictable
3. **Expression-oriented** - Everything is an expression
4. **Strong static typing** - With inference
5. **No inheritance** - Composition over inheritance
6. **Explicit over implicit** - Clear, readable code

### Syntax Quick Reference

```musi
// Bindings
val x := 42;              // immutable
var y := 42;              // mutable
y <- 43;                  // reassignment

// Types
val n: Int := 42;
val f: Bin32 := 3.14;
val s: String := "hello";

// Records
val Point := record {
  x: Bin32,
  var y: Bin32,
};

// Choice Types
val Shape := choice Shape {
  case Circle(radius: Bin32),
  case Rectangle(width: Bin32, height: Bin32),
};

// Pattern Matching
match shape with {
  case .Circle(r) -> 3.14159 * r * r,
  case .Rectangle(w, h) -> w * h,
}

// Procedures
proc add(x: Int, y: Int) -> Int {
  x + y
}

// FFI
@extern("C")
proc malloc(size: N64) -> Ptr<Unit>;
```

## For LLM Assistants

When implementing features:
1. Check [PROGRESS.md](PROGRESS.md) for current status
2. Read relevant syntax/ docs for specification
3. Read relevant compiler/ or runtime/ docs for implementation
4. Check decisions/ for design rationale
5. Reference examples/ for proven patterns

When uncertain:
- Create TODO in relevant file
- Ask human for clarification
- Document assumptions
