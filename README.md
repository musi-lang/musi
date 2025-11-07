# Musi

> [!WARNING]
> Musi is under active development. Language design and implementation are subject to change.

Systems programming language designed for clarity, safety, and performance. Readable syntax meets explicit ARC memory control.

## Highlights

- Expression-oriented semantics; every construct yields a value
- Strong static typing with inference plus gradual escape hatches
- ARC by default with manual control when needed
- Exhaustive pattern matching with guard support
- Fallible types (`T!E`) and structured error propagation

## Example

```musi
const Counter := record { var value: Nat; };

const Counter.inc := proc (var c: Counter) {
  const temp := c.value;
  c.value <- temp + 1;
};

const Counter.show := proc (c: Counter) {
  writeln(`Counter is now: ${c.value}`);
};

var counter := Counter{ .value := 0 };
counter.inc();      // value is now 1
counter.inc();      // value is now 2
counter.show();     // prints: `Counter is now: 2`
```

## Getting Started

### Prerequisites

- [OCaml 5.3+](https://ocaml.org/install) (5.4.0 or greater)
- [opam](https://opam.ocaml.org/doc/Install.html) (2.3.0 or greater)
- [Clang](https://clang.llvm.org/) (19.1.7 or greater)

### Build

```bash
git clone https://github.com/musi-lang/musi.git
cd musi

# build `msc` a.k.a. Musi Compiler
opam exec -- dune pkg lock
opam exec -- dune build

# build `musi` a.k.a. Musi Runtime Environment (MRE)
xmake project -k compile_commands --lsp=clangd
xmake build
```

### Run & Test

```bash
opam exec -- dune exec bin/main.exe
opam exec -- dune tests

# TODO: VM usage in C++ here
```

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on submitting pull requests, code standards, and development setup.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

See [LICENSE](LICENSE) for details.
