# SEAM IL

SEAM is Musi's virtual machine. SEAM IL is the umbrella term for code after Musi source has been lowered for SEAM.

## Layers

- **SEAM HIL** is high-level, typed SEAM IL. It keeps functions, blocks, values, data construction, calls, `given`, `answer`, `handle`, `resume`, native calls, contextual requirements, and proof-trust roots explicit.
- **SEAM BC/IL** is lowered canonical transport IL. It is stack-based assembly form that maps one-to-one to bytecode and VM execution.

`music disasm` prints SEAM HIL projection by default. Use `music disasm --level seam` to print lowered `.seam` IL.

HIL and BC/IL use different textual syntaxes by design:

- HIL text is frontend/tooling-readable typed keyword IR.
- BC/IL text is runtime/transport-readable assembly form.
- HIL keeps source intent, not source sugar. It does not model `class`, `instance`, `with`, `via`, `using`, `for`, `provide`, or `borrow` as source forms. Source `unsafe pin` scopes lower to ordinary local scope unless FFI address extraction needs VM pin leases.

BC/IL mnemonics are canonical, short, and strict:

- dotted text mnemonics are canonical
- binary opcodes stay compact numeric positions
- no legacy aliases
- examples: `ld.loc`, `ld.c.i4`, `br.false`, `call.ind`, `call.iface`, `call.dyn`, `call.ffi`, `new.obj`, `hdl.push`, `raise`

BC/IL is stack based:

- method results are stack type lists
- block entries declare exact incoming stack lists
- branches transfer the whole current stack after condition/index pop
- target stack must exactly match target block `stack [...]`

BC/IL is not source syntax. It has no opcodes named `given`, `shape`, `law`, `proof`, `answer`, `ask`, `class`, `instance`, `option`, `result`, `tuple`, or `sum`.

Core lowering examples:

- tuples, records, variants, `?T`, and `E!T`: `new.obj` plus `ld.fld` / `st.fld`
- first-class functions: `ld.fn`, `new.fn`, `call.ind`
- receiver dispatch: `call.virt`, `call.iface`, `call.dyn`
- FFI: `call.ffi`, `ld.ffi`
- resumable control: `hdl.push`, `raise`, `resume`, `hdl.pop`

Full opcode positions and stack effects live in `specs/seam/bytecode.md`.

## HIL invariants

`music_seam` exposes HIL data structures and verifier support. The verifier rejects:

- functions without blocks
- duplicate blocks or values
- values used before definition
- mismatched binary operand types
- return values that do not match function result type
- missing branch targets
- effect or native calls without declared capabilities

HIL is for compiler and tooling views. `.seam` IL remains the runtime transport.
