# Musi - Programming Language

Musi is programming language with clean type system, functional features, and efficient stack-based bytecode execution.

> [!WARNING]
> Musi is under active development. Language design and implementation subject to change.

## Features

- **Type-safe** with bidirectional type inference
- **Functional features** pattern matching, lambda literals, cons (`::`s) for lists
- **General-purpose** strings, records, choices (tagged unions), interfaces
- **Efficient execution** stack-based MSIL bytecode with mark-sweep GC
- **Editor integration** full LSP support with diagnostics and code navigation
- **Production-ready** index-based references (no lifetimes), union-find unification

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
