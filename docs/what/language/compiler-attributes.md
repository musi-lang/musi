# Compiler Attributes

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

This matches Swift's layering goal: public attributes stay public, compiler-reserved implementation hooks stay separate.

They are not general user metadata names.
They are not the same thing as the `musi:*` foundation namespace.

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
