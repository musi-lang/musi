# SEAM Module Format

Status: proposed

This spec defines the public SEAM module formats.

SEAM is the primary lowered target for Musi and any other frontend that wants to target the VM.

Public artifacts:

- `.seam` for canonical binary modules
- `.seamil` for canonical textual twin of one `.seam` module
- `.mar` for archives that package one or more `.seam` modules with resources and metadata

SEAM is not described as `v2`, `v3`, or similar product branding. Compatibility is expressed by file-format version fields.

## Rust Comparison

Rust 2024 is the host implementation language for current SEAM readers, writers, verifier, and runtime integration.

SEAM format is not Rust metadata. Binary and text forms must stay stable across host implementation refactors and must not expose Rust module paths, trait names, type layout details, or crate-private concepts.

## File Roles

### `.seam`

`.seam` is the primary target artifact.

It is:

- one verified module image
- compact
- deterministic
- suitable for direct frontend targeting
- suitable for VM loading
- suitable for JIT input after verification

It is not:

- a source-language dump
- a package archive
- a host-native executable

### `.seamil`

`.seamil` is the canonical textual twin of `.seam`.

It is:

- exact in semantics
- round-trippable
- readable
- stable enough for third-party frontends and tooling

It may contain:

- comments
- symbolic labels
- named references to imports, exports, pools, methods, and types

It may not contain:

- semantics that `.seam` cannot encode
- source-language sugar
- a richer lowering contract than the binary module image

`.seamil` is BC/IL transport text, not HIL text. Public HIL uses a separate typed syntax.

### `.mar`

`.mar` is an archive bundle.

It contains:

- manifest
- one or more `.seam` modules
- resources
- optional native libraries
- optional introspection payloads such as source maps and symbol tables

Native app mode embeds `.mar` into a launcher/runtime image.

## Compatibility

SEAM uses Java-like file-format versioning.

Header fields:

- magic `SEAM`
- `major`
- `minor`
- image kind
- required domains
- required domain features
- chunk table location and count

Compatibility rules:

- unsupported `major` rejects
- unsupported required `minor` rejects
- missing required domain rejects
- missing required domain feature rejects
- malformed required chunk rejects
- unknown optional chunk skips
- unknown required chunk rejects

`major.minor` describes file-format compatibility, not product marketing names.

## Module Image Kind

One `.seam` file is one module image.

Module image responsibilities:

- declares imports
- declares exports
- declares required domains and features
- carries verified code, metadata, and runtime descriptors needed for execution

Program packaging happens in `.mar`, not by inventing a second VM code image format.

## Domains

SEAM extension families are called domains.

Public domains:

- `managed`
- `resumable`
- `native`
- `link`
- `introspect`

A module declares required features as fully qualified names such as:

- `managed.views`
- `resumable.cont.oneshot`
- `native.pin`
- `link.dynamic`
- `introspect.invoke`

## Binary Layout

The binary format is a canonical chunked envelope.

Canonical high-level structure:

1. header
2. domain and feature manifest
3. chunk table
4. chunk payloads

Core chunks:

- constant pool
- signatures
- imports
- methods
- exports
- module metadata

Domain chunks are declared by the domains they belong to and keyed by chunk schema id.

Chunk entries carry:

- chunk id
- domain id or core marker
- required or optional bit
- schema id
- byte offset
- byte length

Canonical ordering:

- core chunks first
- domain chunks after core chunks
- domain chunks ordered by domain name then chunk id
- no semantic dependence on source emission order

Opcode wire model:

- primary opcode: one byte (`0x00..0xFE`)
- extended opcode: prefix `0xFF` plus a two-byte opcode id
- `.seamil` mnemonics are canonical dotted names such as `ld.loc`, `br.false`, `call.ind`, `new.obj`, and `call.ffi`
- assembler/disassembler mnemonics are canonical and alias-free
- opcode semantics, stack effects, and numeric positions are defined by `specs/seam/bytecode.md`

## Textual Form

`.seamil` uses the same compatibility model as `.seam`.

Text modules declare:

- format version
- required domains
- required features
- imports
- exports
- method signatures, including multi-result stack lists
- block stack signatures
- chunks and code in canonical textual order

Text tooling must support:

- assemble `.seamil` to `.seam`
- disassemble `.seam` to `.seamil`
- validate both with the same verifier contract

## Lowering Boundary

SEAM is the lowered target boundary.

Frontends targeting SEAM must emit explicit lowered machinery rather than source-level constructs.

That means SEAM modules encode:

- explicit locals
- explicit block and label structure
- explicit calls and indirect calls
- explicit runtime object and layout operations
- explicit handler-frame and continuation machinery
- explicit FFI interop operations

SEAM modules do not encode source-level syntax such as pattern matching, `given` contextual visibility, range syntax, or `effect` / `answer` / `handle` / `resume` syntax directly.

Source-level ideas lower to stack bytecode plus descriptors. For example, tuples, records, variants, `?T`, and `E!T` use `new.obj` plus layout fields; dynamic messages use `call.dyn`; interface or shape dispatch uses `call.iface`; foreign ABI edges use `call.ffi`.

## See Also

- `specs/runtime/memory-model.md`
- `specs/runtime/interop.md`
- `specs/interop/c-interop-memory.md`
- `specs/seam/bytecode.md`
