# Public API Map (crates_new)

This document is a practical inventory of what each clean-room crate exports today and what is considered its public Rust surface.

Policy:

- The `pub use ...` set from each crate root is treated as the primary stability boundary.
- Diagnostics are user-facing: error kinds, diag codes, and message style are part of the API contract.
- Internal modules may change freely as long as the crate root surface remains coherent.

## `music_basic`

Exports (crate root):

- `Span`, `Spanned`
- `Source`, `SourceId`, `SourceMap`
- diagnostics: `Diag`, `DiagCode`, `DiagLevel`, `emit*`, `supports_color`
- literals: `Literal`
- string literal decoding: `string_lit` module

Contracts:

- Diagnostic text style is validated in debug/tests (no `": "` and no articles `a/an/the`).

## `music_storage`

Exports:

- `Arena`
- `Idx`

## `music_names`

Exports:

- `Symbol`, `Interner`
- `Ident`
- resolution graph: `NameResolution`, `NameBinding*`, `NameSite`

## `music_lex`

Exports:

- `Lexer`, `LexedSource`
- `Token`, `TokenKind`, trivia kinds
- `LexError`, `LexErrorKind`
- `Cursor`

## `music_ast`

Exports:

- green/red tree model (`SyntaxTree`, `SyntaxNode*`, `SyntaxToken*`)
- `SyntaxNodeKind`
- typed views for syntax (`Expr`, `Pat`, `Ty`, `Attr`, etc.)

## `music_parse`

Exports:

- `parse` and `ParsedSource`
- `ParseError`, `ParseErrorKind`, `ParseResult`

Contracts:

- Parse error kinds are the stable contract; most tests should match variants, not strings.

## `music_known`

Exports:

- `KnownSymbols`

Contracts:

- Canonical spelling and seeding set for compiler-known symbols and lang items.

## `music_hir`

Exports:

- HIR node model (`HirModule`, `HirStore`, `HirExpr*`, `HirPat*`, `HirTy*`)
- attr model (`HirAttr*`, `HirStringLit`)
- origin/provenance (`HirOrigin`)

## `music_resolve`

Exports:

- `resolve_module`, `ResolvedModule`
- options: `ResolveOptions`, `ImportEnv`
- errors: `ResolveError`, `ResolveErrorKind`

## `music_sema`

Exports:

- `analyze_module`, `AnalyzedModule`
- errors: `SemaError`, `SemaErrorKind`, `SemaErrorKinds`, `SemaErrors`
- effect/type surface used by sema: `EffectRow`, `EffectKey`, `SemTy*`

## `music_il`

Exports:

- SEAM artifact model + descriptors + ISA (instruction/opcode/operand tables)

## `music_assembly`

Exports:

- binary encode/decode
- text assemble/disassemble
- validation
- `CodecError`
