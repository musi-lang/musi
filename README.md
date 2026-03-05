# Musi - Programming Language

Musi is a programming language with a mathematically-motivated type system, functional features, and efficient stack-based bytecode execution.

> [!WARNING]
> Musi is under active development. Language design and implementation subject to change.

## Design principles

- **Algebraic types** — records are product types (`A × B`), choices are sum types (`A + B`); syntax reflects the algebra directly
- **Strict LL(1) grammar** — every parse decision requires exactly one token of lookahead; the grammar is self-documenting and hand-parseable
- **Mathematical purity** — syntax forms follow their type-theoretic meaning; no overloaded delimiters, no arbitrary precedence exceptions

## Type system

- Records — `record Point { x: Float, y: Float }` — product types, fields separated by `,`
- Choices — `choice Shape { Circle(Float) | Rect(Float, Float) | Point }` — sum types, variants separated by `|`
- Spread — `point.{ <..old, x := 1.0 }` — functional record update via `<..` spread operator
- Pattern matching — exhaustive `match` with `case` arms, or-patterns, destructuring

## Features

- **Type-safe** with bidirectional type inference
- **Functional** — pattern matching, lambda literals, labeled blocks, cons (`::`) for lists
- **General-purpose** — strings, records, choices (tagged unions), interfaces
- **Efficient execution** — stack-based MSIL bytecode with mark-sweep GC
- **Editor integration** — full LSP support with diagnostics and code navigation
- **Production-ready** — index-based references (no lifetimes), union-find unification

## Building

```bash
cargo build --release
cargo test --workspace
cargo clippy --workspace
```

## Contributing

We welcome contributions from community. Please see [CONTRIBUTING.md](CONTRIBUTING.md) for coding standards and details on how to submit pull requests.

## Code of Conduct

All contributors are expected to follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

This project is licensed under MIT License - see the [LICENSE](LICENSE) file for details.
