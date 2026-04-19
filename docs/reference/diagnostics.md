# Diagnostics

Musi diagnostics follow Clang-style precision: exact source location, concrete subject, expected/found facts, related source ranges, and fix-its when safe.

## Contract

- Every user-facing diagnostic comes from typed diagnostic enums in owning crate/phase.
- Every source diagnostic has one primary label on offending token/expression.
- Headlines name exact broken thing when known: source text, symbol, operator, field, variant, import specifier, type, or expected/found pair.
- Secondary labels point to related source: declaration, expected annotation, parameter, previous definition, operation signature.
- Hints explain actionable repair. Fix-its only for exact replacement.
- Do not use articles in headlines: `a`, `an`, `the`.
- Do not use filler words in headlines: `was`, `were`, `is`.
- Do not abbreviate user-facing text: no `arg`, `attr`, `expr`, `fn`.

## Examples

```diff
- type mismatch
+ return value expected `Int`, found `String`

- callee lacks callable type
+ callee `foo` lacks callable type `Int`

- unknown field
+ field `name` missing from record type `{ id : Int }`

- duplicate class member
+ duplicate class member `equals`
```

## Renderer Shape

```text
src/main.ms:7:8: error[MS3091]: call argument `value` expected `String`, found `Int`
  |
7 | repeat(value := 10)
  |        ^^^^^ argument provided here
  |        ~~~~~ parameter `value` declared here with type `String`
help: pass string value or change parameter type
```

Keep diagnostics compact, but never force users to infer which token, symbol, or type caused the failure.
