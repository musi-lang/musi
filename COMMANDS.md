# Musi Commands Reference

## Build Commands

### Build Compiler (msc)

```bash
cd /home/Kasutaja/musi
opam exec -- dune build
```

### Build Runtime (musi)

```bash
cd /home/Kasutaja/musi/runtime
cargo build
```

### Build Both

```bash
cd /home/Kasutaja/musi
opam exec -- dune build && cd runtime && cargo build
```

## Compilation Commands

### Compile Single File

```bash
cd /home/Kasutaja/musi
opam exec -- dune exec bin/main.exe -- <input.ms> -o <output.msc>
```

### Compile Standard Library

```bash
cd /home/Kasutaja/musi
opam exec -- dune exec bin/main.exe -- stdlib/io.ms -o stdlib/io.msc
```

### Compile Example

```bash
cd /home/Kasutaja/musi
opam exec -- dune exec bin/main.exe -- examples/hello.ms -o examples/hello.msc
```

## Runtime Commands

### Run Bytecode

```bash
cd /home/Kasutaja/musi
runtime/target/debug/musi run <file.msc>
```

### Disassemble Bytecode

```bash
cd /home/Kasutaja/musi
runtime/target/debug/musi disasm <file.msc>
```

## Testing Commands

### Run Compiler Tests

```bash
cd /home/Kasutaja/musi
opam exec -- dune test
```

### Run Specific Test Suite

```bash
cd /home/Kasutaja/musi
opam exec -- dune exec lib/musi_codegen/test_emitter.exe
opam exec -- dune exec lib/musi_codegen/test_encoder.exe
opam exec -- dune exec lib/musi_parse/test_parser.exe
```

## Full Workflow

### Compile and Run hello.ms

```bash
cd /home/Kasutaja/musi
opam exec -- dune build
opam exec -- dune exec bin/main.exe -- stdlib/io.ms -o stdlib/io.msc
opam exec -- dune exec bin/main.exe -- examples/hello.ms -o examples/hello.msc
cd runtime && cargo build && cd ..
runtime/target/debug/musi run examples/hello.msc
```

## Inspection Commands

### Hexdump Bytecode

```bash
hexdump -C <file.msc> | head -n 20
```

### Check Bytecode Structure

```bash
python3 -c "
data = open('<file.msc>', 'rb').read()
print(f'Magic: {data[0:4]}')
print(f'Version: {int.from_bytes(data[4:8], \"little\")}')
print(f'Import offset: {int.from_bytes(data[8:12], \"little\")}')
print(f'Const offset: {int.from_bytes(data[16:20], \"little\")}')
print(f'Proc offset: {int.from_bytes(data[24:28], \"little\")}')
"
```

## Clean Commands

### Clean Compiler Build

```bash
cd /home/Kasutaja/musi
opam exec -- dune clean
```

### Clean Runtime Build

```bash
cd /home/Kasutaja/musi/runtime
cargo clean
```

### Clean All

```bash
cd /home/Kasutaja/musi
opam exec -- dune clean && cd runtime && cargo clean
```

## Development Commands

### Watch and Rebuild Compiler

```bash
cd /home/Kasutaja/musi
opam exec -- dune build --watch
```

### Format Rust Code

```bash
cd /home/Kasutaja/musi/runtime
cargo fmt
```

### Format OCaml Code

```bash
cd /home/Kasutaja/musi
opam exec -- dune fmt
```

## Quick Reference

| Task | Command |
|------|---------|
| Build compiler | `opam exec -- dune build` |
| Build runtime | `cargo build` (in runtime/) |
| Compile .ms → .msc | `opam exec -- dune exec bin/main.exe -- input.ms -o output.msc` |
| Run .msc | `runtime/target/debug/musi run file.msc` |
| Disassemble .msc | `runtime/target/debug/musi disasm file.msc` |
| Run tests | `opam exec -- dune test` |
