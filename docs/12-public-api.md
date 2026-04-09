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
- `music_module`: module/specifier + import environment (`ModuleSpecifier`, `ModuleKey`, `ImportMap`, `ImportEnv`, `ImportError*`, `collect_import_sites`, `collect_export_summary`, `ImportSite*`, `ModuleExportSummary` (incl. exported instance tracking))
- `music_hir`: HIR model + authoritative semantic type arena (`HirOrigin`, `HirStore`, `HirModule`, `HirExpr*`, `HirPat*`, `HirTy*`)
- `music_resolve`: resolve + lowering (`ResolveOptions`, `ResolvedModule`, `ResolvedImport`, `resolve_module`)
- `music_sema`: semantic queries + cross-module semantic boundary (`SemaOptions`, `TargetInfo`, `DefinitionKey`, `ModuleSurface`, `ExportedValue`, `ClassSurface`, `ClassMemberSurface`, `EffectSurface`, `EffectOpSurface`, `InstanceSurface`, `ConstraintSurface`, `SurfaceTy`, `SurfaceTyKind`, `SurfaceTyId`, `SurfaceTyField`, `SurfaceDim`, `SurfaceEffectRow`, `SurfaceEffectItem`, `SemaEnv`, `SemaModule`, `SemaDiagList`, `EffectKey`, `EffectRow`, `check_module`)
- `music_ir`: codegen-facing lowered facts (`IrModule`, `IrCallable`, `IrGlobal`, `IrDataDef`, `IrForeignDef`, `IrEffectDef`, `IrClassDef`, `IrInstanceDef`, `IrExpr`, `IrExprKind`, `IrArg`, `IrLit`, `IrBinaryOp`, `IrOrigin`, `IrParam`, `IrDiagList`, `lower_module`)
- `music_bc`: SEAM contract model (`Artifact`, `ArtifactError`, `Table`, `StringRecord`, typed ids, `SectionTag`, `SEAM_MAGIC`, `BINARY_VERSION`, descriptor types, `Instruction`, `CodeEntry`, `Operand`, `OperandShape`, `Label`, `LabelId`, `Opcode`, `OpcodeFamily`)
- `music_assembly`: SEAM transport/validation (`AssemblyError`, `encode_binary`, `decode_binary`, `validate_binary`, `format_text`, `parse_text`, `validate_text`)
- `music_emit`: SEAM emission (`EmitOptions`, `EmitDiagList`, `EmitDiagKind`, `EmittedBinding`, `EmittedModule`, `EmittedProgram`, `emit_diag_kind`, `lower_ir_module`, `lower_ir_program`)
- `music_session`: session orchestration + cached compile entrypoints (`Session`, `SessionOptions`, `SessionStats`, `ParsedModule`, `SessionSyntaxErrors`, `CompiledOutput`, `SessionDiagList`, `SessionError`, `compile_*`/phase entrypoints through `Session` methods)
- `musi_project`: project/manifest integration over `music_session` (`Project`, `ProjectOptions`, `ProjectError`, `PackageId`, `PackageSource`, `ProjectEntry`, `ResolvedPackage`, `WorkspaceGraph`, `Lockfile`, `LockedPackage`, `LockedPackageSource`, `TaskSpec`, `PackageManifest`, `load_project`, plus namespaced manifest schema types under `musi_project::manifest::*`)

Notes:

- `music_base::Diag` / `DiagLabel` are accessor-driven (fields are not part of the public API).
- `music_base::diag::emit_to_stderr` returns `io::Result<()>`.
- `music_syntax::LexedSource<'src>` now retains source text so CST/AST views can slice token text without duplicating token payloads.
- `music_module::ImportEnv` is resolve-only in `crates_new`: it maps import specifiers to module keys and does not expose opened-export visibility.
- `music_module::ImportError` exposes stable typed failure identity through `ImportErrorKind` plus `ImportError::message()`; display formatting is no longer `Debug`-shaped.
- `music_resolve::ResolvedModule` carries both the current `ModuleKey` and syntax-level export summary so sema can build semantic module surfaces without reopening syntax.
- `music_sema` is query-oriented: semantic facts are accessed through `SemaModule` methods rather than public storage fields.
- `music_sema::ModuleSurface`, `SemaEffectDef`, and `SemaDataDef` now use accessor-based read APIs for exported collections, keys, variants, and effect ops instead of exposing those storage fields directly.
- `music_sema` construction-only builders are internal: crate-private build grouping stays behind `check_module`, and public reads use checked `try_*` accessors where semantic facts may be absent.
- `music_sema::SemaEnv` exchanges owned `ModuleSurface` snapshots rather than live foreign module references.
- `music_sema::EffectKey` / `EffectRow` and sema-owned effect definitions are string-backed rather than `Symbol`-backed, so they are safe across interner boundaries.
- `music_hir::HirTy*` is the authoritative typed boundary produced by sema. `music_sema` no longer exposes a parallel semantic type arena.
- `music_ir` is the frozen executable backend ADT for the pre-runtime pipeline: `IrModule`, `IrExprKind`, related node types, and `lower_module` are the stable codegen-facing contract.
- `music_ir` lowers sema facts into codegen-facing tables and owned executable bodies; `IrModule` now presents its top-level collections through accessors instead of public storage fields, so `music_emit` no longer depends on HIR shape or direct IR module storage layout.
- lowering and emission invariants return typed diagnostics through `IrDiagKind` / `EmitDiagKind` instead of aborting process execution.
- `music_bc` is the single source of truth for SEAM opcodes, operands, descriptor tables, section tags, and artifact validation.
- `music_assembly` is transport-only over `music_bc`: it does not redefine the artifact or ISA.
- `music_emit` lowers one IR module or a reachable IR module set into validated `music_bc::Artifact` values, and `emit_diag_kind` is code-based rather than message-based.
- `music_session` is the project-facing compiler shell below `musi_project`: it caches parse/resolve/sema/IR/emit products and can compile a module or reachable entry graph to artifact, bytes, or text. Syntax failures now flow through the single `SessionSyntaxErrors` shape in both `ParsedModule` and `SessionError::Parse`, and stage-failure propagation is covered through typed parse/resolve/sema/IR/emit session tests.
- `musi_project` loads `musi.json`, builds workspace/package graphs, resolves registry packages into a local cache, and constructs the exact `music_session` module/import view used for package-aware compilation.

## Planned phase crates

These crates are part of the canonical phase DAG but are not implemented as workspace members yet:

- `music_jit`

## Legacy (`crates/`)

`crates/` is legacy reference-only. The rewrite does not add or stabilize new public API surface in legacy crates.
