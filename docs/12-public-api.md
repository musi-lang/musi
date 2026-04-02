# Public API Map (`crates_new/`)

This document inventories the public Rust surface for the canonical compiler rewrite in `crates_new/`.

Policy:

- The `pub use ...` set from each crate root is the primary stability boundary.
- Diagnostics are user-facing: error kinds and message style are part of the API contract.
- Internal modules may change as long as the crate root surface stays coherent.

## Implemented (workspace members)

- `music_base`: `Span`, `Spanned`, `SourceId`, `SourceMap`, and `music_base::diag::*`
- `music_names`: `Symbol`, `Interner`, `Ident`, `NameSite`, `NameResolution`, `NameBinding*`, `KnownSymbols`
- `music_arena`: `Idx`, `Arena`, `ArenaIter*`, `SliceRange`, `SliceArena`
- `music_syntax`: token/trivia + parsing (`Token`, `TokenKind`, `Trivia`, `TriviaKind`, `Lexer`, `LexedSource`, `LexError*`, `ParseError*`, `ParsedSource`, `parse`, `SyntaxTree`, `SyntaxNode*`, `SyntaxToken*`, `SyntaxNodeKind`, `Program`, `canonical_name_text`)
- `music_module`: module/specifier + import environment (`ModuleSpecifier`, `ModuleKey`, `ImportMap`, `ImportEnv`, `ImportError*`, `collect_import_sites`, `collect_export_summary`, `ImportSite*`, `ModuleExportSummary`)
- `music_hir`: HIR model (`HirOrigin`, `HirStore`, `HirModule`, `HirExpr*`, `HirPat*`, `HirTy*`)

Notes:

- `music_base::Diag` / `DiagLabel` are accessor-driven (fields are not part of the public API).
- `music_base::diag::emit_to_stderr` returns `io::Result<()>`.
- `music_syntax::LexedSource<'src>` now retains source text so CST/AST views can slice token text without duplicating token payloads.

## Planned phase crates

These crates are part of the canonical phase DAG but are not implemented as workspace members yet:

- `music_resolve`, `music_sema`, `music_ir`, `music_bc`, `music_assembly`, `music_codegen`, `music_session`
- `musi_project`

## Legacy (`crates/`)

`crates/` is legacy reference-only. The rewrite does not add or stabilize new public API surface in legacy crates.
