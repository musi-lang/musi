use std::collections::HashMap;
use std::path::Path;

use music_ast::expr::{ExprKind, ImportKind};
use music_db::Db;
use music_shared::Symbol;
use music_hir::{lower, HirBundle};
use music_il::instruction::Operand;
use music_il::opcode::Opcode;
use music_resolve::graph::ModuleId;
use music_resolve::loader::{ModuleLoader, ResolvedImport};
use music_resolve::{ModuleGraph, ProjectResolution};
use music_sema::type_check;

use crate::emitter::{emit_with_context, MethodEntry, SeamModule};
use crate::error::EmitError;
use crate::pool::ConstantPool;

/// Per-module offset state tracked during merging.
struct ModuleOffsets {
    methods: u16,
    globals: u16,
    foreigns: u16,
}

/// The combined output of emitting an entire project.
pub struct ProjectEmitResult {
    pub module: SeamModule,
    /// Maps each `ModuleId` to the global indices of its exports in the
    /// combined module.
    pub module_exports: HashMap<ModuleId, Vec<u16>>,
}

/// Emit an entire project by processing modules in topological order.
///
/// Each module is type-checked, lowered, and emitted individually, then
/// all resulting `SeamModule`s are merged into a single combined module
/// with correctly offset indices.
///
/// The `loader` is used to resolve import paths to module identities so
/// that qualified imports can reference the correct globals.
///
/// # Panics
///
/// Panics if a `ModuleId` from the topological order is missing from the
/// project's module map, or if the combined tables exceed `u16::MAX` entries.
///
/// # Errors
///
/// Returns [`EmitError`] if any module fails during emission.
#[expect(
    clippy::unwrap_in_result,
    reason = "capacity overflow `expect`s guard structural invariants (>65535 tables) — \
              these are compiler ICEs, not recoverable errors that belong in EmitError"
)]
pub fn emit_project(
    mut project: ProjectResolution,
    loader: &ModuleLoader,
) -> Result<ProjectEmitResult, EmitError> {
    let order = project.order.clone();
    let mut combined = SeamModule {
        constants: ConstantPool::new(),
        methods: Vec::new(),
        globals: Vec::new(),
        types: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };
    let mut module_exports: HashMap<ModuleId, Vec<u16>> = HashMap::new();

    for &mod_id in &order {
        let module_result = project
            .modules
            .remove(&mod_id)
            .expect("module missing from project resolution");

        let current_path = project.graph.path(mod_id).to_path_buf();

        let (mut db, resolution, type_env, _sema_errors) =
            type_check(module_result.db, module_result.resolution, None);

        lower(&mut db.ast);

        // Build the import context: map each import path Symbol to the
        // already-computed global indices of the dependency module.
        let import_context =
            build_import_context(&db, &current_path, loader, &project.graph, &module_exports);

        let bundle = HirBundle::new(db, resolution, type_env);
        let emitted = emit_with_context(&bundle, import_context)?;

        let offsets = ModuleOffsets {
            methods: u16::try_from(combined.methods.len()).expect("combined method table overflow"),
            globals: u16::try_from(combined.globals.len()).expect("combined global table overflow"),
            foreigns: u16::try_from(combined.foreigns.len())
                .expect("combined foreign table overflow"),
        };

        // Track exported globals for this module (with offset applied).
        let exported_globals: Vec<u16> = emitted
            .globals
            .iter()
            .enumerate()
            .filter(|(_, g)| g.exported)
            .map(|(i, _)| offsets.globals + u16::try_from(i).expect("module global index overflow"))
            .collect();
        let _prev = module_exports.insert(mod_id, exported_globals);

        merge_into(&mut combined, emitted, &offsets);
    }

    Ok(ProjectEmitResult {
        module: combined,
        module_exports,
    })
}

/// Build the import context for a module: maps import path Symbols to the
/// global indices of the corresponding dependency module's exports.
fn build_import_context(
    db: &Db,
    current_path: &Path,
    loader: &ModuleLoader,
    graph: &ModuleGraph,
    module_exports: &HashMap<ModuleId, Vec<u16>>,
) -> HashMap<Symbol, Vec<u16>> {
    let mut context = HashMap::new();

    for &expr_id in &db.ast.root {
        let expr = db.ast.exprs.get(expr_id);
        if let ExprKind::Import { path, kind } = &expr.kind {
            if matches!(kind, ImportKind::Wildcard) {
                continue;
            }
            let path_str = db.interner.resolve(*path);
            let resolved = loader.resolve(path_str, current_path);
            if let Some(ResolvedImport::File(dep_path) | ResolvedImport::Git(dep_path)) = resolved {
                let canonical = dep_path.canonicalize().unwrap_or(dep_path);
                if let Some(dep_id) = graph.lookup(&canonical) {
                    if let Some(exports) = module_exports.get(&dep_id) {
                        let _prev = context.insert(*path, exports.clone());
                    }
                }
            }
        }
    }

    context
}

/// Merge a single module's output into the combined module, applying index offsets.
fn merge_into(combined: &mut SeamModule, source: SeamModule, offsets: &ModuleOffsets) {
    // Append constants. We re-add each entry to the combined pool, building
    // a mapping from old index to new index (the combined pool deduplicates).
    let const_remap: Vec<u16> = source
        .constants
        .entries()
        .iter()
        .map(|entry| combined.constants.add(entry.clone()))
        .collect();

    // Append globals.
    combined.globals.extend(source.globals);

    // Append foreigns.
    combined.foreigns.extend(source.foreigns);

    // Append types (no index remapping needed -- type IDs are stable).
    // Deduplicate by type ID to avoid repeated builtin entries.
    for ty in source.types {
        if !combined.types.iter().any(|t| t.id == ty.id) {
            combined.types.push(ty);
        }
    }

    // Append classes.
    combined.classes.extend(source.classes);

    // Rewrite and append methods.
    for mut method in source.methods {
        rewrite_instructions(&mut method, offsets, &const_remap);
        combined.methods.push(method);
    }
}

/// Rewrite operands in a method's instructions to account for index offsets
/// from merging multiple modules.
fn rewrite_instructions(method: &mut MethodEntry, offsets: &ModuleOffsets, const_remap: &[u16]) {
    for instr in &mut method.instructions {
        match instr.opcode {
            Opcode::LdConst => {
                if let Operand::U16(ref mut idx) = instr.operand {
                    *idx = const_remap.get(usize::from(*idx)).copied().unwrap_or(*idx);
                }
            }
            Opcode::LdGlob | Opcode::StGlob => {
                if let Operand::U16(ref mut idx) = instr.operand {
                    *idx = idx
                        .checked_add(offsets.globals)
                        .expect("global index overflow");
                }
            }
            Opcode::ClsNew => {
                if let Operand::Wide(ref mut method_idx, _) = instr.operand {
                    *method_idx = method_idx
                        .checked_add(offsets.methods)
                        .expect("method index overflow");
                }
            }
            Opcode::FfiCall => {
                if let Operand::U16(ref mut idx) = instr.operand {
                    *idx = idx
                        .checked_add(offsets.foreigns)
                        .expect("foreign index overflow");
                }
            }
            _ => {}
        }
    }
}
