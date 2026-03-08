# Musi - Programming Language

Musi is a programming language with a mathematically-motivated type system, functional features, and efficient stack-based bytecode execution.

> [!WARNING]
> Musi is under active development. Language design and implementation subject to change.

## Design principles

- **Algebraic types** -- records are product types (`A × B`), choices are sum types (`A + B`); syntax reflects the algebra directly
- **Strict LL(1) grammar** -- every parse decision requires exactly one token of lookahead; the grammar is self-documenting and hand-parseable
- **Mathematical purity** -- syntax forms follow their type-theoretic meaning; no overloaded delimiters, no arbitrary precedence exceptions

## Type system

- Records -- `record Point { x: Float, y: Float }` -- product types, fields separated by `,`
- Choices -- `choice Shape { Circle(Float) | Rect(Float, Float) | Point }` -- sum types, variants separated by `|`
- Generics -- `Option['T]`, `Map['K,'V]`, `fn id['T](x: 'T): 'T` -- type parameters in `[...]`
- Spread -- `point.{ <..old, x := 1.0 }` -- functional record update via `<..` spread operator
- Pattern matching -- exhaustive `match` with `case` arms, or-patterns, destructuring

## Features

- **Type-safe** with bidirectional type inference
- **Functional** -- pattern matching, lambda literals, higher-order functions, cons (`::`) for lists
- **General-purpose** -- strings, arrays, records, choices (tagged unions), type classes
- **Efficient execution** -- stack-based MSIL bytecode with reference-counted heap values
- **Editor integration** -- VS Code extension with syntax highlighting and snippets
- **Sound implementation** -- index-based AST (no lifetimes), union-find type unification

## Quick start

```bash
# Build the toolchain
cargo build --release

# Run a program (use ./target/release/musi or add to PATH)
./target/release/musi run examples/hello/index.ms

# Type-check
./target/release/musi check myfile.ms

# Run inline tests
./target/release/musi test myfile.test.ms
```

A minimal Musi program:

```musi
// hello.ms
writeln("Hello, world!");
```

A taste of the type system:

```musi
record Point { x: Float, y: Float }

fn distance(a: Point, b: Point): Float => (
    const dx := a.x - b.x;
    const dy := a.y - b.y;
    sqrt(dx * dx + dy * dy)
);

const origin := .{ x := 0.0, y := 0.0 };
const p      := .{ x := 3.0, y := 4.0 };
writeln(float_to_string(distance(origin, p)));  // 5.0
```

Pattern matching on a choice type:

```musi
choice Shape { Circle(Float) | Rect(Float, Float) }

fn area(s: Shape): Float =>
    match s with (
        .Circle(r)  => 3.14159 * r * r
      | .Rect(w, h) => w * h
    );
```

## Building from source

```bash
cargo build --release
cargo clippy --workspace
```

Tests are run per-crate to avoid memory pressure:

```bash
cargo build --tests -p musi_lex -p musi_parse -p musi_codegen -p musi_vm
./target/debug/deps/musi_parse-<hash>
```

## Contributing

We welcome contributions from community. Please see [CONTRIBUTING.md](CONTRIBUTING.md) for coding standards and details on how to submit pull requests.

## Code of Conduct

All contributors are expected to follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

This project is licensed under MIT License - see the [LICENSE](LICENSE) file for details.
