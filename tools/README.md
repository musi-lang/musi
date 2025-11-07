# Tools

## gen_opcodes.ml

This tool creates the file `runtime/src/opcode.hpp` using information from `lib/codegen/instr.ml`. Helps keep all opcode definitions in one place.

### How to Use

To run the tool, use:

```bash
make gen-opcodes
```

Or run it directly:

```bash
opam exec -- dune exec tools/gen_opcodes.exe -- lib/codegen/instr.ml runtime/src/opcode.hpp
```

### When to Run

Run `make gen-opcodes` whenever you change `lib/codegen/instr.ml`, for example if you:

- Add new opcodes
- Change opcode values
- Remove opcodes

It'll update the C++ header file automatically.
