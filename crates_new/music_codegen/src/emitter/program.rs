use std::collections::HashMap;
use std::mem;
use std::path::Path;

use music_basic::{Source, SourceId, SourceMap, Span, path as import_path, string_lit};
use music_check::AnalyzedModule;
use music_hir::{HirExprKind, HirModule, HirPatKind};
use music_il::{
    ConstantPool, GlobalEntry, Instruction, MethodEntry, MethodName, Opcode, SeamArtifact,
    TypeDescriptor,
};
use music_names::{
    Interner, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol, SymbolSlice,
};

use crate::errors::EmitResult;
use crate::model::EmitModule;

use super::context::{EmitMaps, EmitPools, ModuleExportKey};
use super::function::FunctionEmitter;
use super::ty::builtin_ty_descriptors;

pub(super) struct ProgramEmitter<'a> {
    interner: &'a Interner,
    sources: &'a SourceMap,
    entry_path: &'a str,

    constants: ConstantPool,
    methods: Vec<MethodEntry>,
    globals: Vec<GlobalEntry>,
    types: Vec<TypeDescriptor>,

    global_by_binding: HashMap<NameBindingId, u16>,
    import_global_by_binding: HashMap<NameBindingId, u16>,
    module_export_globals: HashMap<ModuleExportKey, u16>,
}

impl<'a> ProgramEmitter<'a> {
    pub(super) fn new(interner: &'a Interner, sources: &'a SourceMap, entry_path: &'a str) -> Self {
        Self {
            interner,
            sources,
            entry_path,
            constants: ConstantPool::new(),
            methods: vec![],
            globals: vec![],
            types: builtin_ty_descriptors(),
            global_by_binding: HashMap::new(),
            import_global_by_binding: HashMap::new(),
            module_export_globals: HashMap::new(),
        }
    }

    pub(super) fn register_module(&mut self, path: &str, analyzed: &AnalyzedModule) {
        // Allocate global slots for top-level `let` bindings.
        let root = analyzed
            .module
            .store
            .exprs
            .get(analyzed.module.root)
            .clone();
        let HirExprKind::Sequence { exprs, .. } = root.kind else {
            return;
        };

        for expr_id in exprs.iter().copied() {
            let expr = analyzed.module.store.exprs.get(expr_id).clone();
            let HirExprKind::Let { mods, pat, .. } = expr.kind else {
                continue;
            };

            // Only bind-name top-level lets become globals.
            let pat = analyzed.module.store.pats.get(pat).clone();
            let HirPatKind::Bind { name, .. } = pat.kind else {
                continue;
            };

            let idx = self.alloc_global(path, name.name, mods.exported, mods.opaque);
            if let Some(binding) =
                find_def_binding(&analyzed.names, analyzed.module.source_id, name.span)
            {
                let _prev = self.global_by_binding.insert(binding, idx);
            }
            let _prev = self
                .module_export_globals
                .insert((path.to_owned(), name.name), idx);
        }

        // Map import bindings to their target export globals.
        self.register_import_globals(path, analyzed);
    }

    fn register_import_globals(&mut self, path: &str, analyzed: &AnalyzedModule) {
        let import_exprs = collect_import_exprs(&analyzed.module);

        let from_path = self
            .sources
            .get(analyzed.module.source_id)
            .map_or_else(|| Path::new(""), Source::path);

        for (import_span, import_path_span, exports) in import_exprs {
            let raw_import_path =
                decode_string_lit(self.sources, analyzed.module.source_id, import_path_span);
            let import_path = if raw_import_path.starts_with('@') {
                raw_import_path
            } else if raw_import_path.starts_with('.')
                || Path::new(raw_import_path.as_str()).is_absolute()
            {
                import_path::resolve_import_path(from_path, raw_import_path.as_str())
                    .to_string_lossy()
                    .into_owned()
            } else {
                raw_import_path
            };
            for export in exports {
                let target = self
                    .module_export_globals
                    .get(&(import_path.clone(), export))
                    .copied();
                let Some(target_idx) = target else {
                    continue;
                };

                // Resolver defines one import binding per exported name with site span == import span.
                for (binding_id, binding) in &analyzed.names.bindings {
                    let NameBindingKind::Import { .. } = binding.kind else {
                        continue;
                    };
                    if binding.site.span != import_span || binding.name != export {
                        continue;
                    }
                    let _prev = self.import_global_by_binding.insert(binding_id, target_idx);
                }
            }

            let _ = path;
        }
    }

    pub(super) fn emit_all(&mut self, modules: &[EmitModule<'_>]) -> EmitResult<SeamArtifact> {
        // Single entry method runs module top-levels in dependency order.
        let entry_index = self.alloc_entry_method();

        for module in modules {
            self.emit_module_toplevel(module.path, module.analyzed, entry_index)?;
        }

        Ok(SeamArtifact {
            constants: mem::take(&mut self.constants),
            methods: mem::take(&mut self.methods),
            globals: mem::take(&mut self.globals),
            types: mem::take(&mut self.types),
            effects: vec![],
            classes: vec![],
            foreigns: vec![],
        })
    }

    fn alloc_entry_method(&mut self) -> usize {
        let idx = self.methods.len();
        self.methods.push(MethodEntry {
            name: MethodName::Entry,
            instructions: vec![],
            locals_count: 0,
            absolute_global_loads: vec![],
        });
        idx
    }

    fn emit_module_toplevel(
        &mut self,
        path: &str,
        analyzed: &AnalyzedModule,
        entry_index: usize,
    ) -> EmitResult {
        let root = analyzed
            .module
            .store
            .exprs
            .get(analyzed.module.root)
            .clone();
        let HirExprKind::Sequence { exprs, .. } = root.kind else {
            return Ok(());
        };

        let mut entry_instructions = vec![];
        let mut entry_locals_max = 0usize;

        {
            let mut f = FunctionEmitter::new(
                self.interner,
                self.sources,
                analyzed,
                EmitMaps {
                    globals_by_binding: &self.global_by_binding,
                    import_globals_by_binding: &self.import_global_by_binding,
                    module_export_globals: &self.module_export_globals,
                },
                EmitPools {
                    constants: &mut self.constants,
                    methods: &mut self.methods,
                    types: &mut self.types,
                },
            );

            for expr_id in exprs.iter().copied() {
                let expr = analyzed.module.store.exprs.get(expr_id).clone();
                if let HirExprKind::Let {
                    has_params,
                    pat,
                    value,
                    ..
                } = expr.kind
                {
                    let pat = analyzed.module.store.pats.get(pat).clone();
                    let HirPatKind::Bind { name, .. } = pat.kind else {
                        continue;
                    };

                    let Some(binding) =
                        find_def_binding(&analyzed.names, analyzed.module.source_id, name.span)
                    else {
                        continue;
                    };
                    let global_idx = self
                        .global_by_binding
                        .get(&binding)
                        .copied()
                        .unwrap_or(u16::MAX);
                    if global_idx == u16::MAX {
                        continue;
                    }

                    if has_params {
                        // Compile function body into its own method; store closure in global.
                        let method_idx = f.emit_let_function_method(expr_id)?;
                        entry_instructions.push(Instruction::with_wide(
                            Opcode::ClsNew,
                            u16::try_from(method_idx).unwrap_or(u16::MAX),
                            0,
                        ));
                        entry_instructions.push(Instruction::with_u16(Opcode::StGlob, global_idx));
                    } else if let Some(value) = value {
                        let locals = f.emit_expr_into(&mut entry_instructions, value)?;
                        entry_locals_max = entry_locals_max.max(locals);
                        entry_instructions.push(Instruction::with_u16(Opcode::StGlob, global_idx));
                    }
                } else {
                    let locals = f.emit_expr_into(&mut entry_instructions, expr_id)?;
                    entry_locals_max = entry_locals_max.max(locals);
                    entry_instructions.push(Instruction::basic(Opcode::Pop));
                }
            }

            // Ensure an explicit `ret`.
            entry_instructions.push(Instruction::basic(Opcode::Ret));
        }

        let entry = self
            .methods
            .get_mut(entry_index)
            .expect("entry method exists");
        entry.instructions = entry_instructions;
        entry.locals_count = u16::try_from(entry_locals_max).unwrap_or(u16::MAX);

        let _ = path;
        Ok(())
    }

    fn alloc_global(
        &mut self,
        module_path: &str,
        name: Symbol,
        exported: bool,
        opaque: bool,
    ) -> u16 {
        let idx = u16::try_from(self.globals.len()).expect("global count fits in u16");
        let exported_here = exported && module_path == self.entry_path;
        let global_name = if exported_here {
            self.interner.resolve(name).to_owned()
        } else {
            format!("{module_path}::{}", self.interner.resolve(name))
        };
        self.globals.push(GlobalEntry {
            name: global_name,
            exported: exported_here,
            opaque: opaque && exported_here,
        });
        idx
    }
}

fn find_def_binding(
    names: &NameResolution,
    source_id: SourceId,
    span: Span,
) -> Option<NameBindingId> {
    let site = NameSite::new(source_id, span);
    names
        .bindings
        .iter()
        .find(|(_, binding)| binding.site == site)
        .map(|(id, _)| id)
}

fn collect_import_exprs(module: &HirModule) -> Vec<(Span, Span, SymbolSlice)> {
    let mut out = vec![];
    for (_id, expr) in &module.store.exprs {
        let HirExprKind::Import { path, exports } = &expr.kind else {
            continue;
        };
        out.push((expr.origin.span, path.span, exports.clone()));
    }
    out
}

fn decode_string_lit(sources: &SourceMap, source_id: SourceId, span: Span) -> String {
    let Some(source) = sources.get(source_id) else {
        return String::new();
    };
    let start = usize::try_from(span.start).unwrap_or(0);
    let end = usize::try_from(span.end).unwrap_or(start);
    let raw = source.text().get(start..end).unwrap_or("");
    string_lit::decode(raw)
}
