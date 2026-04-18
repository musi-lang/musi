# SEAM IL

SEAM is Musi's virtual machine. SEAM IL is the umbrella term for code after Musi source has been lowered for SEAM.

## Layers

- **SEAM HIL** is high-level, typed SEAM IL. It keeps functions, blocks, values, data construction, calls, effects, foreign calls, and capability requirements explicit.
- **`.seam` IL** is lowered true SEAM IL. It is the assembly-like artifact form that maps to SEAM bytecode and VM execution.

`music disasm` prints SEAM HIL projection by default. Use `music disasm --level seam` to print lowered `.seam` IL.

## HIL invariants

`music_seam` exposes HIL data structures and verifier support. The verifier rejects:

- functions without blocks
- duplicate blocks or values
- values used before definition
- mismatched binary operand types
- return values that do not match function result type
- missing branch targets
- effect or foreign calls without declared capabilities

HIL is for compiler and tooling views. `.seam` IL remains the runtime transport.
