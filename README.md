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
- No garbage collection, no reference counting, no runtime overhead

## Example

```musi
val Counter := record {
  var value: Nat;
};

val inc := fn (ref var c: Counter) {
  c.value <- c.value + 1;
};

val show := fn (ref c: Counter) {
  writeln(`Counter is now: ${c.value}`);
};

var counter := Counter{ .value := 0 };
inc(ref var counter);  // value is now 1
inc(ref var counter);  // value is now 2
show(ref counter);     // prints: `Counter is now: 2`
```

## Getting Started

### Prerequisites

- [OCaml](https://ocaml.org/install) (5.4.0 or greater)
- [opam](https://opam.ocaml.org/doc/Install.html) (2.3.0 or greater)
- [Alire](https://alire.ada.dev/) (2.1.0 or greater)

### Build

```bash
git clone https://github.com/musi-lang/musi.git
cd musi

# build Musi Compiler
opam exec -- dune pkg lock
opam exec -- dune build

# build Musi Runtime
alr build
```

### Run & Test

```bash
opam exec -- dune exec bin/compiler/msc.exe
opam exec -- dune test

# run 'hello.ms' file
./bin/musi run examples/hello.ms
```

## Contributing

Contributions welcome. See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines on submitting pull requests, code standards, and development setup.

## Code of Conduct

All contributors must follow [Code of Conduct](CODE_OF_CONDUCT.md).

## License

See [LICENSE](LICENSE) for details.
