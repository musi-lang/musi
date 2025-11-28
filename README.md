# Musi

> [!WARNING]
> Musi is under active development. Language design and implementation are subject to change.

Systems programming language designed for clarity, safety, and performance. Readable syntax meets stack-only memory discipline.

## Highlights

- Expression-oriented semantics; every construct yields a value
- Strong static typing with inference
- Stack-only memory model with region-based arena allocators
- Compile-time lifetime checking with Ada-level safety
- Exhaustive pattern matching with guard support
- Fallible types (`Expect<T, E>`) and structured error propagation

## Getting Started

### Prerequisites

- [OCaml](https://ocaml.org/install) (5.4.0 or greater)
- [opam](https://opam.ocaml.org/doc/Install.html) (2.3.0 or greater)

### Build

```bash
git clone https://github.com/musi-lang/musi.git
cd musi

# install specific OCaml stuffs
opam install ocamlformat ocaml-lsp-server dune

# finally,... lock and load
opam exec -- dune pkg lock
opam exec -- dune build
```

### Run & Test

```bash
opam exec -- dune exec bin/compiler/msc.exe
opam exec -- dune test

# TODO: runtime cmd
```

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on submitting pull requests, code standards, and development setup.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

See [LICENSE](LICENSE) for details.
