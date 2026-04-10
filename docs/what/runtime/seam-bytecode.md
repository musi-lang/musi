# SEAM Bytecode

## Identity

SEAM is the executable contract between compilation and runtime.

It is:

- stack-based
- family-organized
- runtime-contract typed
- explicit about sequences, aggregates, effects, FFI, and runtime typing

It is not an AST dump and not a generic hardware assembly.

## Layers

The SEAM layer is intentionally split:

- `music_bc` owns artifact model, descriptors, ids, operands, opcodes, and validation rules
- `music_assembly` owns binary encoding, decoding, text form, and transport validation

That split keeps transport from redefining semantics.

## Operand Model

Instructions use explicit operand shapes rather than backend-private ad hoc payloads.

Important operand categories include:

- locals and globals
- constants and strings
- methods and captures
- labels and branch tables
- type-bearing operands
- effect and foreign references

## Opcode Families

SEAM groups execution into clear families:

- load/store
- scalar and compare
- branch
- call and closure
- sequence
- aggregate/data
- runtime type
- effect and handler
- host edge

That family split is semantic and implementation-facing, not just naming style.

## Artifact Sections

The artifact contract includes:

- strings
- types
- constants
- globals
- methods
- effects
- classes
- foreigns
- exports
- data
- metadata

These sections define the runtime-visible handoff from compiler to loader.

## Boundaries

SEAM does not directly encode:

- high-level source sugar
- package/workspace semantics
- human-facing compiler diagnostics

## See Also

- `docs/what/runtime/seam-vm.md`
- `docs/how/runtime/runtime-api.md`
- `docs/why/runtime-boundary.md`
