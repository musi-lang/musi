# Musi syntax

Canonical syntax sources live in grammar, not in scattered prose, but this page should still tell you what shapes to expect before you dive into parser rules.

## Source of truth

- `grammar/MusiParser.g4`
- `grammar/MusiLexer.g4`
- `grammar/Musi.abnf`
- `docs/what/language/` chapters for user-facing examples

## Syntax map

- Surface syntax is expression-first.
- Modules use `.ms`.
- Comments use `--` and `/- ... -/`; item docs use `---` and `/-- ... -/`; module docs use `--!` and `/-! ... -/`. Block comments nest.
- Pattern matching uses `match expr (| pattern => expr ...)`.
- Effect invocation uses `request Effect.op(...)`.
- Record updates use spread literals such as `{ ...point, y := 9 }`.
- Data variants use constructor-style payload declarations such as `| Some(T)` or `| Configured(port : Int, secure : Bool)`.
- Ordinary calls can use positional or named arguments, such as `render(8080, secure := .True)` or `render(port := 8080, secure := .True)`.
- Receiver callable dot form uses UDNS notation: `value.inc(by := 2)` lowers as an attached or receiver-first call. Receiver-pattern declarations use `let (value : Type).inc(by : Int) := ...`; the explicit namespace form is `Type.inc(value, by := 2)`.
- Pure function types use `T -> U`.
- Effectful function types use `T ~> U`.
- Generic calls can be bare or dotted: `f[T](x)` and `value.f[T](x)`.
- Compile-time values use value-position `comptime`: `let x := comptime 3;` and `let scale(comptime n : Int, x : Int) := x * n;`. See `docs/what/language/advanced/comptime.md` for specialization and generated syntax.
- Class constraints use `where T : Eq`; class members can be called through evidence as `Eq.eq(left, right)`.
- Existential and opaque implementer types use type-position modifiers: `any Writer` and `some Writer`. Bare `Writer` names the class/type itself; it is not an existential.
- Higher-kinded type parameters use arrow kinds: `F : Type -> Type`.
- Type constructors can be partially applied from the left: `Result[String]` can fit `Type -> Type` when `Result` has two type parameters.
- Parsing pipeline today is `music_syntax` lexer + parser, then `music_resolve`, `music_sema`, `music_ir`, `music_emit`.

## One symbol, several jobs

Musi uses `:` in more than one place, and the surrounding form gives each use a clear role:

- type annotation: `let port : Int := 8080;`
- implements constraint: `let eq[T] (x : T, y : T) : Bool where T : Eq := ...;`
- subtype constraint: `let clamp[T] (x : T) : T where T <: Number := x;`
- named variant payload declaration: `| Configured(port : Int, secure : Bool)`

Constructor-style payloads line variant definitions up with construction and matching:

- definition: `| Configured(port : Int, secure : Bool)`
- construction: `.Configured(port := 8080, secure := .True)`
- pattern: `.Configured(port, secure := enabled)`

Named variant payloads are order-independent at construction and pattern sites.
Positional variants stay positional, and named variants stay named.

Ordinary calls follow the same readability rule:

- positional arguments may come first
- named arguments may come after
- once named arguments begin, later positional arguments are not allowed

## Reading order

Start here when you need exact syntax answers:

1. `grammar/MusiParser.g4` for parser rules
2. `grammar/MusiLexer.g4` for token rules
3. `grammar/Musi.abnf` for compact grammar reference
4. matching chapter under `docs/what/language/` for examples and explanation

## Where examples live

Language chapters intentionally use referenced snippets instead of raw Musi fences.
That keeps examples syntax-checked, highlightable, and tied to real repo sources instead of drifting prose-only fragments.
Website Learn pages keep source links out of the reading flow. The snippet registry and tests keep examples traceable inside the repo.

## No-return, no-loop baseline

Musi is expression-first:

- ordinary functions do not need `return`
- blocks produce values from their final expression
- there are no `for`, `while`, `break`, or `continue` statements
- repetition usually comes from recursion, ranges, pipelines, and library helpers

## Scope note

This file is index, not second grammar.
Keep normative syntax details in grammar files, and keep user-facing explanation in chapter docs.
