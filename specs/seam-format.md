# SEAM Module Format

Status: proposed

This spec defines the public SEAM module formats.

SEAM is the primary lowered target for Musi and any other frontend that wants to target the VM.

Public artifacts:

- `.seam` for canonical binary modules
- `.seamil` for canonical textual twin of one `.seam` module
- `.mar` for archives that package one or more `.seam` modules with resources and metadata

SEAM is not described as `v2`, `v3`, or similar product branding. Compatibility is expressed by file-format version fields.

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

## Textual Form

`.seamil` uses the same compatibility model as `.seam`.

Text modules declare:

- format version
- required domains
- required features
- imports
- exports
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
- explicit handler and continuation machinery
- explicit native interop operations

SEAM modules do not encode source-level syntax such as pattern matching, class declarations, range syntax, borrow syntax, or effect syntax directly.

## See Also

- [runtime-memory-model.md](/Users/krystian/CodeProjects/musi-next/specs/runtime-memory-model.md)
- [unsafe-and-addresses.md](/Users/krystian/CodeProjects/musi-next/specs/unsafe-and-addresses.md)
- [c-interop-memory.md](/Users/krystian/CodeProjects/musi-next/specs/c-interop-memory.md)
