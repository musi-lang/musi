# Effect System

## Core Model

Effects are first-class language features, not library sugar.

Core surface:

- `effect`
- `perform`
- `handle`
- `resume`
- open rows via `with { ... }`

## Effect Declarations

Effect declarations define:

- effect identity
- operation surface
- result signatures
- optional laws

They participate in semantic checking and exported surface metadata.

## Performing And Handling

`perform` introduces an effectful operation call.

`handle` provides:

- value clause behavior for normal completion
- op-clause behavior for matching operations
- continuation control through `resume`

Handlers are part of language semantics, not post-lowering recovery logic.

## `resume`

`resume` represents continuation re-entry from inside handler op clauses.

At the language level, it is part of the handled-effect model:

- zero resumes may replace a computation
- one resume behaves like ordinary continuation flow
- multiple resumes are semantically meaningful and remain part of the model

## Rows

Rows track effect openness and composition.

They matter in:

- function signatures
- generic effectful abstractions
- checking whether a computation remains pure

## Laws And Boundaries

Effect laws are specification surface, not runtime-enforced VM behavior by themselves.

This document defines language semantics, not:

- emitted opcodes
- handler frame layout
- host fallback rules

## See Also

- `docs/what/language/type-system.md`
- `docs/what/runtime/seam-vm.md`
- `docs/why/runtime-boundary.md`
