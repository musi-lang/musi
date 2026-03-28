use std::collections::HashMap;
use std::path::Path;

use music_ast::expr::ExprKind;
use music_db::Db;
use music_hir::{HirBundle, lower};
use music_il::instruction::Operand;
use music_il::opcode::Opcode;
use music_resolve::graph::ModuleId;
use music_resolve::loader::{ModuleLoader, ResolvedImport};
use music_resolve::{ModuleGraph, ProjectResolution};
use music_sema::type_check;
use music_shared::Symbol;

use crate::emitter::{ImportedGlobal, MethodEntry, SeamModule, emit_with_context};
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
    pub module_exports: HashMap<ModuleId, Vec<ImportedGlobal>>,
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
        effects: Vec::new(),
        classes: Vec::new(),
        foreigns: Vec::new(),
    };
    let mut module_exports: HashMap<ModuleId, Vec<ImportedGlobal>> = HashMap::new();

    for &mod_id in &order {
        let mut module_result = project
            .modules
            .remove(&mod_id)
            .expect("module missing from project resolution");

        let current_path = project.graph.path(mod_id).to_path_buf();

        lower(&mut module_result.db.ast);

        let (db, resolution, type_env, _sema_errors) =
            type_check(module_result.db, module_result.resolution, None);

        // Build the import context: map each import path Symbol to the
        // already-computed global indices of the dependency module.
        let import_context =
            build_import_context(&db, &current_path, loader, &project.graph, &module_exports);

        let bundle = HirBundle::new(db, resolution, type_env);
        let mut emitted = emit_with_context(&bundle, import_context)?;
        annotate_effect_modules(&mut emitted, &current_path);

        let offsets = ModuleOffsets {
            methods: u16::try_from(combined.methods.len()).expect("combined method table overflow"),
            globals: u16::try_from(combined.globals.len()).expect("combined global table overflow"),
            foreigns: u16::try_from(combined.foreigns.len())
                .expect("combined foreign table overflow"),
        };

        // Track exported globals for this module (with offset applied).
        let exported_globals: Vec<ImportedGlobal> = emitted
            .globals
            .iter()
            .enumerate()
            .filter(|(_, g)| g.exported)
            .map(|(i, global)| ImportedGlobal {
                name: bundle.db.interner.resolve(global.name).to_owned(),
                index: offsets.globals + u16::try_from(i).expect("module global index overflow"),
            })
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
    module_exports: &HashMap<ModuleId, Vec<ImportedGlobal>>,
) -> HashMap<Symbol, Vec<ImportedGlobal>> {
    let mut context = HashMap::new();

    for &expr_id in &db.ast.root {
        let expr = db.ast.exprs.get(expr_id);
        if let ExprKind::Import { path, kind: _ } = &expr.kind {
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

    let effect_remap = merge_effects(combined, &source.effects);
    let type_remap = merge_types(combined, &source.types);
    let class_remap = merge_classes(combined, source.classes, offsets, &type_remap);

    // Rewrite and append methods.
    for mut method in source.methods {
        rewrite_instructions(
            &mut method,
            offsets,
            &const_remap,
            &effect_remap,
            &type_remap,
            &class_remap,
        );
        combined.methods.push(method);
    }
}

/// Rewrite operands in a method's instructions to account for index offsets
/// from merging multiple modules.
fn rewrite_instructions(
    method: &mut MethodEntry,
    offsets: &ModuleOffsets,
    const_remap: &[u16],
    effect_remap: &HashMap<u16, u16>,
    type_remap: &HashMap<u16, u16>,
    class_remap: &HashMap<u16, u16>,
) {
    for (instr_idx, instr) in method.instructions.iter_mut().enumerate() {
        match instr.opcode {
            Opcode::LdConst => {
                if let Operand::U16(ref mut idx) = instr.operand {
                    *idx = const_remap.get(usize::from(*idx)).copied().unwrap_or(*idx);
                }
            }
            Opcode::ArrNewT => {
                if let Operand::TypeTagged(ref mut type_id, ref mut tag_idx, _) = instr.operand {
                    *type_id = *type_remap.get(type_id).unwrap_or(type_id);
                    let remapped = const_remap
                        .get(usize::from(*tag_idx))
                        .copied()
                        .expect("variant tag constant missing from remap");
                    *tag_idx =
                        u8::try_from(remapped).expect("variant tag constant index overflow (>255)");
                }
            }
            Opcode::ArrNew => {
                if let Operand::TypeLen(ref mut type_id, _) = instr.operand {
                    *type_id = *type_remap.get(type_id).unwrap_or(type_id);
                }
            }
            Opcode::LdGlob | Opcode::StGlob => {
                if method.absolute_global_loads.contains(&instr_idx) {
                    continue;
                }
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
            Opcode::TyChk | Opcode::TyCast => {
                if let Operand::U16(ref mut type_id) = instr.operand {
                    *type_id = *type_remap.get(type_id).unwrap_or(type_id);
                }
            }
            Opcode::TyclDict => {
                if let Operand::U16(ref mut class_id) = instr.operand {
                    *class_id = *class_remap.get(class_id).unwrap_or(class_id);
                }
            }
            Opcode::EffInvk => {
                if let Operand::Effect(ref mut effect_id, _) = instr.operand {
                    *effect_id = *effect_remap
                        .get(effect_id)
                        .expect("effect remap missing for perf");
                }
            }
            Opcode::EffHdlPush => {
                if let Operand::EffectJump(ref mut effect_id, _, _) = instr.operand {
                    *effect_id = *effect_remap
                        .get(effect_id)
                        .expect("effect remap missing for hdl.push");
                }
            }
            _ => {}
        }
    }
}

fn merge_types(
    combined: &mut SeamModule,
    source_types: &[music_il::format::TypeDescriptor],
) -> HashMap<u16, u16> {
    let mut remap = HashMap::new();
    let mut next_type_id = combined
        .types
        .iter()
        .map(|ty| ty.id)
        .filter(|id| *id < music_il::format::BUILTIN_TYPE_TYPE)
        .max()
        .unwrap_or(music_il::format::FIRST_EMITTED_TYPE_ID.saturating_sub(1))
        .saturating_add(1);

    for ty in source_types {
        if let Some(existing) = combined.types.iter().find(|candidate| candidate.key == ty.key) {
            let _ = remap.insert(ty.id, existing.id);
            continue;
        }
        let new_id = if ty.id >= music_il::format::BUILTIN_TYPE_TYPE {
            ty.id
        } else {
            let fresh = next_type_id;
            next_type_id = next_type_id.saturating_add(1);
            fresh
        };
        let mut cloned = ty.clone();
        cloned.id = new_id;
        let _ = remap.insert(ty.id, new_id);
        combined.types.push(cloned);
    }

    remap
}

fn merge_classes(
    combined: &mut SeamModule,
    source_classes: Vec<music_il::format::ClassDescriptor>,
    offsets: &ModuleOffsets,
    type_remap: &HashMap<u16, u16>,
) -> HashMap<u16, u16> {
    let mut remap = HashMap::new();
    for mut class in source_classes {
        let new_id = u16::try_from(combined.classes.len()).expect("combined class table overflow");
        let _ = remap.insert(class.id, new_id);
        class.id = new_id;
        for instance in &mut class.instances {
            instance.type_id = *type_remap
                .get(&instance.type_id)
                .unwrap_or(&instance.type_id);
            for method in &mut instance.methods {
                if method.method_idx != u16::MAX {
                    method.method_idx = method
                        .method_idx
                        .checked_add(offsets.methods)
                        .expect("class method index overflow");
                }
            }
        }
        combined.classes.push(class);
    }
    remap
}

fn annotate_effect_modules(module: &mut SeamModule, current_path: &Path) {
    let module_name = infer_module_name(current_path);
    for effect in &mut module.effects {
        if effect.module_name.is_empty() {
            effect.module_name = module_name.clone();
        }
    }
}

fn infer_module_name(current_path: &Path) -> String {
    if current_path
        .components()
        .rev()
        .take(3)
        .collect::<Vec<_>>()
        .iter()
        .any(|component| component.as_os_str() == "modules")
    {
        if let Some(stem) = current_path.file_stem().and_then(|stem| stem.to_str()) {
            return format!("musi:{stem}");
        }
    }
    current_path.to_string_lossy().into_owned()
}

fn merge_effects(
    combined: &mut SeamModule,
    source_effects: &[music_il::format::EffectDescriptor],
) -> HashMap<u16, u16> {
    let mut remap = HashMap::new();
    for effect in source_effects {
        if let Some(existing) = combined.effects.iter().find(|candidate| {
            candidate.module_name == effect.module_name && candidate.name == effect.name
        }) {
            let _ = remap.insert(effect.id, existing.id);
            continue;
        }
        let new_id = u16::try_from(combined.effects.len()).expect("combined effect table overflow");
        let mut cloned = effect.clone();
        cloned.id = new_id;
        let _ = remap.insert(effect.id, new_id);
        combined.effects.push(cloned);
    }
    remap
}
