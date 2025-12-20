# Musi

> [!WARNING]
> Musi is currently under active development. Language design and implementation are subject to change.

Musi is systems programming language designed for clarity, safety, and performance. It combines readable syntax with strict, stack-only memory model.

## Key Features

- **Expression-oriented**: Every construct in language produces value.
- **Strong static typing**: Includes automatic type inference to keep code clean and concise.
- **Stack-only memory**: Uses region-based arena allocators for predictable performance and memory safety.
- **Safety checks**: Provides compile-time checking of lifetimes, ensuring high level of safety.
- **Pattern matching**: Supports exhaustive pattern matching with guards for robust logic.
- **Error handling**: Uses fallible types (`Expect<T, E>`) for structured and reliable error management.

## Installation

- **macOS/Linux**: Install `pkg-config` using your package manager.
- **Windows**: Install `opam` using the method of your choice.

```bash
opam init

git clone https://github.com/musi-lang/musi.git
cd musi

opam install ocamlformat ocaml-lsp-server dune
opam exec -- dune pkg lock
opam exec -- dune build
```

## Running and Testing

```bash
opam exec -- dune exec bin/main.exe
opam exec -- dune test
```

## Contributing

We welcome contributions from community. Please see [CONTRIBUTING.md](CONTRIBUTING.md) for coding standards and details on how to submit pull requests.

## Code of Conduct

All contributors are expected to follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

Project licensed under terms of [LICENSE](LICENSE) file.
