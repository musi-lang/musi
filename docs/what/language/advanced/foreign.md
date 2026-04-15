---
title: "Foreign"
description: "Keep foreign declarations separate from general attributes so boundary thinking stays clear."
group: "Advanced and Tooling"
section: "Advanced and Tooling"
order: 31
slug: "foreign"
summary: "Declare foreign bindings at the runtime boundary, not inside ordinary domain code."
---

{{snippet:chapter-foreign}}

## Boundary Tool

Foreign declarations describe bindings implemented outside Musi.
The example names a C function and its Musi-facing type so code on Musi side can call across boundary with explicit contract.
This is advanced because it is about integration, not about core language flow.

## When to Reach for It

Users working near system boundaries need to know how Musi reaches native code without pretending that boundary is ordinary function definition.
If docs bury foreign declarations under attribute notes or runtime pages, the integration story stays fuzzy.
A dedicated page keeps the riskier cross-language surface explicit.

## Read the Boundary

Read `foreign "c"` as declaration of external implementation source.
Then read remainder of line as ordinary Musi-facing name and type surface that callers will see on Musi side.
When adding foreign bindings, keep signatures minimal, verify types carefully, and isolate these declarations near integration boundaries instead of scattering them through domain code.
Calls to foreign bindings belong inside `unsafe { ... }` because Musi cannot prove what native code does with raw pointers, strings, global state, or process state.

Wrap a foreign call in a small `unsafe` block, then expose a safe wrapper when ordinary callers should not see the native boundary.

## Small Exercise

- Declare one foreign binding.
- Identify language/runtime boundary it crosses.
- Wrap the call in a small `unsafe` block.
- Explain what Musi side promises about arguments and result.

## Mistake to Avoid

Do not treat foreign declarations as casual shortcut for code that could stay inside normal Musi modules.

## Next Page

Continue to [Unsafe and FFI](/learn/book/advanced/unsafe-and-ffi) for raw pointer and native-call boundaries.
