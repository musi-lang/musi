# Public API Map (`crates/`)

This document lists the public Rust API in `crates/`.

Use it when checking:

- crate-root stability boundaries
- which crate owns one public concept
- whether a change belongs in public API or behind internal modules

Rules:

- Each crate root `pub use ...` set is the primary stability boundary.
- Diagnostics are user-facing: `Error` / `ErrorKind` names and message style are part of the API contract.
- Internal modules may change as long as the crate root surface stays coherent.

## Core Language Pipeline

- `music_base`: `Span`, `Spanned`, `SourceId`, `SourceMap`, and `music_base::diag::*`
- `music_names`: `Symbol`, `Interner`, `Ident`, `NameSite`, `NameResolution`, `NameBinding*`, `KnownSymbols`
- `music_arena`: `Idx`, `Arena`, `ArenaIter*`, `SliceRange`, `SliceArena`
- `music_syntax`: token/trivia + parsing (`Token`, `TokenKind`, `Trivia`, `TriviaKind`, `Lexer`, `LexedSource`, `LexError*`, `ParseError*`, `ParsedSource`, `parse`, `SyntaxTree`, `SyntaxNode*`, `SyntaxToken*`, `SyntaxNodeKind`, `Program`, `canonical_name_text`)
- `music_module`: module/specifier + import environment (`ModuleSpecifier`, `ModuleKey`, `ImportMap`, `ImportEnv`, `ImportError*`, `collect_import_sites`, `collect_export_summary`, `ImportSite*`, `ModuleExportSummary` (incl. exported instance tracking))
- `music_hir`: HIR model + authoritative semantic type arena (`HirOrigin`, `HirStore`, `HirModule`, `HirExpr*`, `HirPat*`, `HirTy*`)
- `music_resolve`: resolve + lowering (`ResolveOptions`, `ResolvedModule`, `ResolvedImport`, `resolve_module`)
- `music_sema`: semantic queries + cross-module semantic boundary (`SemaOptions`, `TargetInfo`, `DefinitionKey`, `ModuleSurface`, `ExportedValue`, `ClassSurface`, `ClassMemberSurface`, `EffectSurface`, `EffectOpSurface`, `InstanceSurface`, `ConstraintSurface`, `SurfaceTy`, `SurfaceTyKind`, `SurfaceTyId`, `SurfaceTyField`, `SurfaceDim`, `SurfaceEffectRow`, `SurfaceEffectItem`, `SemaEnv`, `SemaModule`, `SemaDiagList`, `EffectKey`, `EffectRow`, `check_module`)
- `music_ir`: codegen-facing lowered facts (`IrModule`, `IrCallable`, `IrGlobal`, `IrDataDef`, `IrForeignDef`, `IrEffectDef`, `IrClassDef`, `IrInstanceDef`, `IrExpr`, `IrExprKind`, `IrArg`, `IrLit`, `IrBinaryOp`, `IrOrigin`, `IrParam`, `IrDiagList`, `lower_module`)

## Executable Contract And Runtime

- `music_seam`: SEAM executable contract (`Artifact`, `ArtifactError`, `Table`, `StringRecord`, typed ids, `SectionTag`, `SEAM_MAGIC`, `BINARY_VERSION`, descriptor types, `Instruction`, `CodeEntry`, `Operand`, `OperandShape`, `Label`, `LabelId`, `Opcode`, `OpcodeFamily`, `AssemblyError`, `encode_binary`, `decode_binary`, `validate_binary`, `format_text`, `parse_text`, `validate_text`)
- `music_emit`: SEAM emission (`EmitOptions`, `EmitDiagList`, `EmitDiagKind`, `EmittedBinding`, `EmittedModule`, `EmittedProgram`, `emit_diag_kind`, `lower_ir_module`, `lower_ir_program`)
- `musi_foundation`: source-visible foundation registry for first-party `musi:*` modules (`extend_import_map`, `resolve_spec`, `register_modules`, `module_source`, `test::*`, `syntax::*`)
- `musi_vm`: SEAM runtime + embedding surface (`Program`, `ProgramExport*`, `Vm`, `VmOptions`, `VmHost`, `RejectingHost`, `VmLoader`, `RejectingLoader`, `Value`, `ValueView`, `SeqView`, `RecordView`, `StringView`, `ForeignCall`, `EffectCall`, `VmError*`, `VmResult`)
- `musi_native`: repo-owned host/world integration layer (`NativeHost`, `NativeTestReport`, `NativeTestCaseResult`)
- `musi_rt`: source-aware runtime service (`Runtime`, `RuntimeOptions`, `RuntimeError*`)

## Service And Project Layer

- `music_session`: session orchestration + cached compile entrypoints (`Session`, `SessionOptions`, `SessionStats`, `ParsedModule`, `SessionSyntaxErrors`, `CompiledOutput`, `SessionDiagList`, `SessionError`, `compile_*`/phase entrypoints through `Session` methods)
- `musi_project`: project/manifest integration over `music_session` (`Project`, `ProjectOptions`, `ProjectError`, `PackageId`, `PackageSource`, `ProjectEntry`, `ResolvedPackage`, `WorkspaceGraph`, `Lockfile`, `LockedPackage`, `LockedPackageSource`, `TaskSpec`, `PackageManifest`, `load_project`, plus namespaced manifest schema types under `musi_project::manifest::*`)
- `musi_tooling`: shared external-tool support for direct-source loading, artifact I/O, structured diagnostics, and package-aware analysis (`DirectGraph`, `load_direct_graph`, `DiagnosticsFormat`, `CliDiagnostics*`, `session_error_report`, `project_error_report`, `read_artifact_bytes`, `write_artifact_bytes`, `collect_project_diagnostics*`, `hover_for_project_file*`, `ToolHover`)
- `musi_lsp`: LSP server binary crate for diagnostics and hover over package-owned Musi source files
- `music`: direct source and `.seam` binary crate
- `musi`: package-aware operator binary crate

Notes:

- `music_base::Diag` / `DiagLabel` are accessor-driven (fields are not part of the public API).
- `music_base::diag::emit_to_stderr` returns `io::Result<()>`.
- `music_syntax::LexedSource<'src>` retains source text so CST/AST views can slice token text without duplicating token payloads.
- `music_module::ImportEnv` is resolve-only in `crates`: it maps import specifiers to module keys and does not expose opened-export visibility.
- `music_module::ImportError` exposes stable typed failure identity through `ImportErrorKind` plus `ImportError::message()`; display formatting is no longer `Debug`-shaped.
- `music_resolve::ResolvedModule` carries the current `ModuleKey` and syntax-level export summary so sema can build semantic module data without reopening syntax.
- `music_sema` is query-oriented: semantic facts are read through `SemaModule` methods, not public storage fields.
- `music_sema::ModuleSurface`, `SemaEffectDef`, and `SemaDataDef` use accessors for exported collections, keys, variants, and effect ops instead of exposing storage fields directly.
- `music_sema` construction-only builders are internal; public reads use checked `try_*` accessors where semantic facts may be absent.
- `music_sema::SemaEnv` exchanges owned `ModuleSurface` snapshots rather than live foreign module references.
- `music_sema::EffectKey` / `EffectRow` and sema-owned effect definitions are string-backed rather than `Symbol`-backed, so they are safe across interner boundaries.
- `music_hir::HirTy*` is the typed boundary produced by sema. `music_sema` no longer exposes a parallel semantic type arena.
- `music_ir` is the frozen executable backend ADT for the pre-runtime pipeline: `IrModule`, `IrExprKind`, related node types, and `lower_module` are the stable backend contract.
- `music_ir` lowers sema facts into owned executable bodies; `IrModule` presents top-level collections through accessors instead of public storage fields.
- lowering and emission invariants return typed diagnostics through `IrDiagKind` / `EmitDiagKind` instead of aborting process execution.
- `music_seam` is the source of truth for SEAM opcodes, operands, descriptor tables, section tags, binary/text transport, and artifact validation.
- `music_emit` lowers one IR module or a reachable IR module set into validated `music_seam::Artifact` values, and `emit_diag_kind` is code-based.
- `music_session` is the project-facing compiler shell below `musi_project`: it caches parse/resolve/sema/IR/emit products and can compile a module or reachable entry graph to artifact, bytes, or text. Syntax failures flow through the single `SessionSyntaxErrors` shape in both `ParsedModule` and `SessionError::Parse`.
- `musi_vm::Vm` now includes runtime module operations (`load_module`, `lookup_module_export`, `call_module_export`) in addition to root-export execution, and `Value` includes first-class module and continuation runtime values.
- `musi_vm` splits host seams cleanly: `VmHost` owns foreign/effect edges, while `VmLoader` owns runtime program loading.
- `musi_vm::RejectingHost` / `RejectingLoader` are explicit reject-by-default seams, not practical runtime defaults.
- `musi_vm::ForeignCall` and `musi_vm::EffectCall` now expose typed signature metadata through `param_tys`, `result_ty`, and runtime type-name helpers backed by the originating `Program`.
- `musi_foundation` is the single Rust-side source of truth for the `musi:*` module registry; runtime and project layers consume it rather than redefining foundation modules locally.
- `musi_rt::Runtime` is the runtime layer above `musi_vm`: it registers source/program inputs, loads root modules, supports typed expression-syntax evaluation, compiles module syntax into runtime module handles, and runs source-aware package-style test modules over one explicit `NativeHost`.
- `musi_native::NativeHost` is the first-party host/world integration layer used by the repo-owned runtime path; it owns registered foreign/effect handlers, `musi:test` session collection, cfg-selected platform dispatch, and embedding fallback host delegation.
- the `musi:*` inventory is `musi:test` and `musi:syntax`; `musi:syntax` is the source-visible bridge for syntax evaluation and quoted module registration.
- `musi_project` loads `musi.json`, builds workspace/package graphs, resolves registry packages into a local cache, discovers co-located `*.test.ms` modules, constructs the `music_session` module/import view used for package-aware compilation, and can now load from the nearest ancestor manifest.
- `musi_tooling` sits above compiler/runtime crates and below external tool binaries: it owns direct-file graph loading, machine-readable diagnostics shaping, and package-aware editor analysis without pulling package policy into compiler-core crates.

## First-Party Package Families

- `@std`: first-party standard library root namespace under `packages/std`, with direct family re-exports such as `Assert`, `Bytes`, `Math`, `Option`, `Result`, and `Testing`
- `@std/*`: portable first-party family API under `packages/std`, with family imports as the standard package API
- `musi:*`: source-visible foundation namespace for capabilities that must not depend on hidden compiler/package magic

## Planned Phase Crates

These crates are part of the canonical phase DAG but are not implemented as workspace members yet:

- `music_jit`

## See Also

- `docs/where/workspace-map.md`
- `docs/where/phase-boundaries.md`
- `docs/status/frontend-stabilization-audit.md`
