# FFI

## Boundary Model

Musi keeps FFI explicit:

- `foreign "abi"` marks ABI-facing declarations
- `@link(...)` provides symbol and library metadata
- `@when(...)` gates declarations by target
- `@repr(...)` and `@layout(...)` mark layout-sensitive data

If the ABI string is omitted, the default ABI is `"c"`.

## Foreign Declarations

The direct inbound FFI surface covers:

- foreign call signatures
- explicit ABI strings
- native symbol naming
- link-time metadata

These declarations are not treated as ordinary Musi callables with hidden lowering tricks.

## Linking And Target Gating

`@link(...)` and `@when(...)` express:

- library selection
- symbol selection
- target-specific availability

They are part of the explicit source contract.

## Aggregate And Layout Concerns

Interop-sensitive data uses:

- explicit repr/layout attrs
- sema-side validation
- runtime/SEAM lowering that respects the declared surface

## Exported Foreign Surface

`export foreign` group behavior belongs to the language-facing boundary too, because exported foreign availability affects package and host integration.

## Boundaries

This document does not define:

- host loader policy
- native symbol resolution internals
- VM-side foreign call implementation details

## See Also

- `docs/what/language/compiler-attributes.md`
- `docs/what/runtime/seam-vm.md`
- `docs/why/runtime-boundary.md`
