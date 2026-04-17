# Language feature coverage

This checklist keeps Learn docs aligned with grammar without turning reader pages into grammar dumps.
Each feature should have one current-language explanation and at least one snippet-backed example when practical.

## Start

- Files, final expressions, and direct commands: `start/getting-started.md`, `start/first-program.md`
- `let`, `let rec`, blocks, no `return`, and no loop statements: `start/values-and-let.md`, `start/blocks-and-expressions.md`
- `mut` and reassignment: `start/mutation.md`

## Core expressions

- Literals, number forms, strings, booleans, runes, and templates: `core/literals.md`, `advanced/templates-and-splices.md`
- Tuples and unit: `core/tuples-and-unit.md`
- Operators, ranges, and fixity/operator values: `core/operators.md`, `core/ranges.md`, `advanced/operator-forms.md`
- Functions, lambdas, calls, named arguments, generic calls, pipelines, and methods: `core/functions.md`, `core/lambdas.md`, `core/calls.md`, `core/methods.md`

## Data

- Record literals, spread updates, arrays, slices, indexing, and fields: `data/records.md`, `data/arrays-and-slices.md`, `data/indexing-and-fields.md`
- `data` definitions, record-shaped data, variant payloads, defaults, constructors, and matching: `data/data-definitions.md`, `data/patterns.md`
- Pattern forms, named payload patterns, guards, `as`, and pattern alternatives: `data/patterns.md`

## Organization and types

- Imports, exports, packages, and `export opaque`: `organization/imports-and-exports.md`, `organization/packages.md`
- Annotations, constraints, callable types, inference, generics, `forall`, type tests, and casts: `types/type-annotations.md`, `types/callable-types.md`, `types/type-inference.md`, `types/generics.md`, `types/forall-types.md`, `types/type-tests-and-casts.md`

## Abstractions, effects, and advanced forms

- Classes, instances, laws, and constraints: `abstractions/classes.md`, `abstractions/instances.md`, `abstractions/laws.md`
- Effects, `using`, effect sets, `request`, `handle`, handler clauses, and `resume`: `effects-runtime/effects.md`, `effects-runtime/using.md`, `effects-runtime/handlers.md`
- Foundation, runtime, stdlib layering, attributes, foreign declarations, quote, comptime, syntax splices, tests, and tooling: `effects-runtime/foundation.md`, `effects-runtime/runtime.md`, `effects-runtime/stdlib.md`, `advanced/attributes.md`, `advanced/foreign.md`, `advanced/quote-and-syntax.md`, `advanced/comptime.md`, `advanced/templates-and-splices.md`, `advanced/testing.md`, `advanced/running-and-tooling.md`
