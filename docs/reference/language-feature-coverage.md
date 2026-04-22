# Language feature coverage

This checklist keeps Learn docs aligned with grammar without turning reader pages into grammar dumps.
Each feature should have one current-language explanation and at least one snippet-backed example when practical.

## Start

- Files, final expressions, and direct commands: `start/getting-started.md`, `start/first-program.md`
- `let`, `let rec`, blocks, no `return`, and no loop statements: `start/values-and-let.md`, `start/blocks-and-expressions.md`
- `mut`, reassignment, and mutation by value: `start/mutation.md`

## Syntax surface

- First-class everything and comparison references: `specs/language/first-class-everything.md`
- Delimiter law, arrays, tuples, sequences, records, indexing, semicolon rules, structural `let` members, and named arguments: `specs/language/syntax.md`, `specs/language/type-core.md`
- `:=`, `/=`, `=`, `=>`, `->`, and `~>` as source separators and operators: `core/operators.md`, `types/type-annotations.md`
- `?T` and `E!T` as optional and error-shaped type surfaces: `types/type-annotations.md`, `types/generics.md`

## Core expressions

- Literals, number forms, strings, booleans, runes, and templates: `core/literals.md`, `advanced/templates-and-splices.md`
- Tuples and unit: `core/tuples-and-unit.md`
- Operators, ranges, `.(op)`, `?.`, `!.`, `?.[i]`, `!.[i]`, `?.(op)`, `!.(op)`, `??`, and `catch`: `core/operators.md`, `core/ranges.md`, `advanced/operator-forms.md`
- Functions, mandatory-backslash lambdas, calls, named arguments, generic calls, pipelines, and procedures: `core/functions.md`, `core/lambdas.md`, `core/calls.md`, `core/procedures.md`

## Data

- Record literals, spread updates, arrays, slices, indexing, and fields: `data/records.md`, `data/arrays-and-slices.md`, `data/indexing-and-fields.md`
- `data` definitions, record-shaped data, variant payloads, defaults, constructors, and matching: `data/data-definitions.md`, `data/patterns.md`
- Pattern forms, named payload patterns, guards, pattern aliases with `as`, and pattern alternatives: `data/patterns.md`

## Organization and types

- Imports, exports, packages, no import `as` aliases, and native boundaries: `organization/imports-and-exports.md`, `organization/packages.md`
- `mut`, `known`, `Proof[P]`, annotations, constraints, callable types, inference, generics, type tests, refinement aliases with `as`, and casts: `types/type-annotations.md`, `types/callable-types.md`, `types/type-inference.md`, `types/generics.md`, `types/type-tests-and-casts.md`

## Abstractions, effects, and advanced forms

- Contextual `shape` plus `given` values, contextual parameters, operator members, and ambiguity diagnostics: `abstractions/contextual-capabilities.md`
- Effects, `effect`, `ask`, `answer`, `handle`, `resume`, and answer members: `effects-runtime/effects-and-answers.md`
- Proofs, `law`, `Proof[P]`, contextual proof evidence, and `@axiom` trust roots: `specs/language/first-class-everything.md`, `specs/language/type-core.md`, `specs/language/syntax.md`, `specs/language/items-and-attributes.md`
- Pin action scopes are only valid inside `unsafe` blocks; `name` has type `Pin[T]`, remains scoped to `body`, and cannot be returned from that body.
- Foundation, runtime, stdlib layering, attributes, hygienic `quote`, `#` splices, comptime, templates, tests, and tooling: `effects-runtime/foundation.md`, `effects-runtime/runtime.md`, `effects-runtime/stdlib.md`, `advanced/attributes.md`, `advanced/quote-and-syntax.md`, `advanced/comptime.md`, `advanced/templates-and-splices.md`, `advanced/testing.md`, `advanced/running-and-tooling.md`

