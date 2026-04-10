# Compiler Attributes

**What**: language-level attribute surface and reserved compiler namespaces.
**Why**: attrs affect language-adjacent meaning, metadata, and tooling boundaries, so their tiers must stay explicit.
**How**: use this when changing attr syntax, attr semantics, or compiler-reserved attr namespaces.
**Where**: attribute validation lives mostly in `music_sema`; FFI-specific surface also relates to `docs/what/language/ffi.md`.

## Attribute Tiers

Musi keeps three tiers:

- public language attrs
- compiler-reserved `@musi.*`
- inert metadata

That split prevents tool metadata, compiler intrinsics, and language-visible attrs from collapsing into one bag.

## Argument Model

Attrs use one argument model:

- positional and named forms are allowed
- named form is canonical in docs
- positional mapping follows declared order
- duplicate names are invalid

## Public Language Attrs

Important public attrs include:

- `@link(...)`
- `@when(...)`
- `@repr(...)`
- `@layout(...)`
- diagnostic policy attrs where supported

These remain explicit surface features, not hidden backend switches.

## Compiler-Reserved Attrs

Compiler-owned attrs live under `@musi.*`.

They exist for:

- language-version or compiler-owned switches
- intrinsic wiring
- runtime-layout and codegen metadata

They are not general user metadata names.

## Inert Metadata

Attrs may also carry information that does not alter core semantics directly.

That metadata remains structured, explicit, and source-visible.

## Boundaries

Attrs do not replace:

- startup rules
- typeclass semantics
- effect semantics
- ordinary declarations

## See Also

- `docs/what/language/ffi.md`
- `docs/what/language/metaprogramming.md`
- `docs/why/compiler-architecture.md`
