---
name: musi-diagnostics
description: Write and review Musi compiler/runtime diagnostics. Use when changing diagnostic enums, messages, labels, hints, fix-its, renderer output, diagnostic tests, or any user-facing compiler error wording in crates such as music_base, music_syntax, music_resolve, music_sema, music_session, musi_project, music_emit, musi_vm, or tooling/LSP diagnostic surfaces.
---

# Musi Diagnostics

## Rule

Make compiler output specific enough that user can identify exact broken thing without guessing.

Bad:

```text
type mismatch
callee lacks callable type
unknown field
```

Good:

```text
return value expected `Int`, found `String`
callee `foo` lacks callable type `Int`
field `name` missing from record type `{ id : Int }`
```

## Workflow

1. Locate owning typed diagnostic enum/kind. Do not create ad hoc user-facing strings outside owning phase.
2. Find source span for offending token/expression. Use it as primary label.
3. Put concrete subject in headline: source text, symbol, operator, field, variant, import specifier, type, or expected/found pair.
4. Add secondary labels for related source when available: annotation, parameter, declaration, previous definition, field declaration, operation signature.
5. Add help only when it gives actionable edit. Add fix-it only when replacement is exact and safe.
6. Add tests for diagnostic kind/code plus rendered output when wording/ranges matter.

## Wording Contract

- Headlines use subject-first style.
- Headlines include backticked concrete subject when known.
- Labels answer “where exactly?” and “why exactly?”; do not merely repeat headline.
- Avoid articles: `a`, `an`, `the`.
- Avoid filler/weak verbs in headlines: `was`, `were`, `is`.
- Avoid abbreviations: `arg`, `attr`, `expr`, `fn`.
- Avoid vague standalone categories: `invalid target`, `unknown thing`, `type mismatch`, `arity mismatch`.

## Output Shape

```text
path.ms:line:col: error[MS3091]: call argument `value` expected `String`, found `Int`
  |
7 | repeat(value := 10)
  |        ^^^^^ argument provided here
  |        ~~~~~ parameter `value` declared here with type `String`
help: pass string value or change parameter type
```

Keep output compact. Add detail only when it removes ambiguity.
