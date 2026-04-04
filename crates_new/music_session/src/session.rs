use std::collections::{BTreeMap, BTreeSet};

use music_assembly::{encode_binary, format_text};
use music_base::{SourceId, SourceMap};
use music_bc::Artifact;
use music_emit::{EmittedModule, EmittedProgram, lower_ir_module, lower_ir_program};
use music_ir::{IrModule, lower_module};
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportMap, ImportResolveResult, ModuleKey,
    ModuleSpecifier, collect_export_summary, collect_import_sites,
};
use music_names::Interner;
use music_resolve::{ResolveOptions, ResolvedModule, resolve_module};
use music_sema::{ModuleSurface, SemaEnv, SemaModule, SemaOptions, check_module};
use music_syntax::{Lexer, ParsedSource as SyntaxParsedSource, parse};

use crate::api::{CompiledOutput, ParsedModule, SessionError, SessionOptions, SessionStats};

#[derive(Default)]
struct ModuleRecord {
    text: String,
    revision: u64,
    source_id: Option<SourceId>,
    parsed: Option<ParsedModule>,
    parsed_source: Option<SyntaxParsedSource>,
    resolved: Option<ResolvedModule>,
    sema: Option<SemaModule>,
    ir: Option<IrModule>,
    emitted: Option<EmittedModule>,
    static_imports: BTreeSet<ModuleKey>,
}

struct SessionImportEnv<'session> {
    import_map: &'session ImportMap,
    module_keys: &'session BTreeSet<ModuleKey>,
}

impl ImportEnv for SessionImportEnv<'_> {
    fn resolve(&self, from: &ModuleKey, spec: &ModuleSpecifier) -> ImportResolveResult {
        if let Some(mapped) = self.import_map.resolve(from, spec) {
            let target = ModuleKey::new(mapped.as_str());
            if self.module_keys.contains(&target) {
                return Ok(target);
            }
        }
        let target = ModuleKey::new(spec.as_str());
        if self.module_keys.contains(&target) {
            Ok(target)
        } else {
            Err(ImportError::new(
                ImportErrorKind::NotFound,
                format!("unknown module `{}`", spec.as_str()),
            ))
        }
    }
}

#[derive(Default)]
struct SurfaceMap {
    surfaces: BTreeMap<ModuleKey, ModuleSurface>,
}

impl SemaEnv for SurfaceMap {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface> {
        self.surfaces.get(key).cloned()
    }
}

pub struct Session {
    options: SessionOptions,
    interner: Interner,
    sources: SourceMap,
    modules: BTreeMap<ModuleKey, ModuleRecord>,
    reverse_deps: BTreeMap<ModuleKey, BTreeSet<ModuleKey>>,
    entry_programs: BTreeMap<ModuleKey, EmittedProgram>,
    next_revision: u64,
    stats: SessionStats,
}

impl Session {
    #[must_use]
    pub fn new(options: SessionOptions) -> Self {
        Self {
            options,
            interner: Interner::new(),
            sources: SourceMap::new(),
            modules: BTreeMap::new(),
            reverse_deps: BTreeMap::new(),
            entry_programs: BTreeMap::new(),
            next_revision: 1,
            stats: SessionStats::default(),
        }
    }

    #[must_use]
    pub const fn stats(&self) -> &SessionStats {
        &self.stats
    }

    pub fn set_import_map(&mut self, import_map: ImportMap) {
        self.options.import_map = import_map;
        self.clear_all_resolve_caches();
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the source text cannot be registered in the session source map.
    pub fn set_module_text(
        &mut self,
        key: &ModuleKey,
        text: impl Into<String>,
    ) -> Result<(), SessionError> {
        let record = self.modules.entry(key.clone()).or_default();
        record.text = text.into();
        record.revision = self.next_revision;
        self.next_revision = self.next_revision.saturating_add(1);
        record.source_id = None;
        record.parsed = None;
        record.parsed_source = None;
        record.resolved = None;
        record.sema = None;
        record.ir = None;
        record.emitted = None;
        self.invalidate_dependents(key);
        self.entry_programs.clear();
        let _ = self.ensure_source_id(key)?;
        Ok(())
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the module is unknown or parsing fails.
    ///
    /// # Panics
    ///
    /// Panics if the parsed-module cache is not populated after successful construction.
    pub fn parse_module(&mut self, key: &ModuleKey) -> Result<&ParsedModule, SessionError> {
        if self
            .modules
            .get(key)
            .and_then(|record| record.parsed.as_ref())
            .is_none()
        {
            let parsed = self.build_parsed_module(key)?;
            self.module_record_mut(key)?.parsed = Some(parsed);
            self.stats.parse_runs = self.stats.parse_runs.saturating_add(1);
        }
        let parsed = self
            .module_record(key)?
            .parsed
            .as_ref()
            .expect("parsed cache missing after construction");
        if parsed.lex_errors.is_empty() && parsed.parse_errors.is_empty() {
            Ok(parsed)
        } else {
            Err(SessionError::Parse {
                module: key.clone(),
                lex_errors: parsed.lex_errors.clone(),
                parse_errors: parsed.parse_errors.clone(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if parsing or resolving the module fails.
    ///
    /// # Panics
    ///
    /// Panics if the resolved-module cache is not populated after successful construction.
    pub fn resolve_module(&mut self, key: &ModuleKey) -> Result<&ResolvedModule, SessionError> {
        if self
            .modules
            .get(key)
            .and_then(|record| record.resolved.as_ref())
            .is_none()
        {
            let resolved = self.build_resolved_module(key)?;
            self.update_dependency_graph(key, &resolved);
            self.module_record_mut(key)?.resolved = Some(resolved);
            self.stats.resolve_runs = self.stats.resolve_runs.saturating_add(1);
        }
        let resolved = self
            .module_record(key)?
            .resolved
            .as_ref()
            .expect("resolved cache missing after construction");
        if resolved.diags.is_empty() {
            Ok(resolved)
        } else {
            Err(SessionError::Resolve {
                module: key.clone(),
                diags: resolved.diags.clone().into_boxed_slice(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if parsing, resolving, or semantic analysis fails.
    ///
    /// # Panics
    ///
    /// Panics if the semantic-module cache is not populated after successful construction.
    pub fn check_module(&mut self, key: &ModuleKey) -> Result<&SemaModule, SessionError> {
        if self
            .modules
            .get(key)
            .and_then(|record| record.sema.as_ref())
            .is_none()
        {
            let sema = self.build_sema_module(key)?;
            self.module_record_mut(key)?.sema = Some(sema);
            self.stats.sema_runs = self.stats.sema_runs.saturating_add(1);
        }
        let sema = self
            .module_record(key)?
            .sema
            .as_ref()
            .expect("sema cache missing after construction");
        if sema.diags().is_empty() {
            Ok(sema)
        } else {
            Err(SessionError::Sema {
                module: key.clone(),
                diags: sema.diags().to_vec().into_boxed_slice(),
            })
        }
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if any earlier phase or IR lowering fails.
    ///
    /// # Panics
    ///
    /// Panics if the semantic or IR cache is not populated after successful construction.
    pub fn lower_module(&mut self, key: &ModuleKey) -> Result<&IrModule, SessionError> {
        if self
            .modules
            .get(key)
            .and_then(|record| record.ir.as_ref())
            .is_none()
        {
            let _ = self.check_module(key)?;
            let ir = {
                let sema = self
                    .module_record(key)?
                    .sema
                    .as_ref()
                    .expect("sema cache missing after successful check");
                lower_module(sema, &self.interner).map_err(|diags| SessionError::Ir {
                    module: key.clone(),
                    diags: diags.into_boxed_slice(),
                })?
            };
            self.module_record_mut(key)?.ir = Some(ir);
            self.stats.ir_runs = self.stats.ir_runs.saturating_add(1);
        }
        Ok(self
            .module_record(key)?
            .ir
            .as_ref()
            .expect("ir cache missing after construction"))
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if any earlier phase or SEAM emission fails.
    ///
    /// # Panics
    ///
    /// Panics if the IR or emitted-module cache is not populated after successful construction.
    pub fn emit_module(&mut self, key: &ModuleKey) -> Result<&EmittedModule, SessionError> {
        if self
            .modules
            .get(key)
            .and_then(|record| record.emitted.as_ref())
            .is_none()
        {
            let _ = self.lower_module(key)?;
            let emit_options = self.options.emit;
            let emitted = {
                let ir = self
                    .module_record(key)?
                    .ir
                    .as_ref()
                    .expect("ir cache missing after successful lowering");
                lower_ir_module(ir, emit_options).map_err(|diags| SessionError::Emit {
                    module: key.clone(),
                    diags: diags.into_boxed_slice(),
                })?
            };
            self.module_record_mut(key)?.emitted = Some(emitted);
            self.stats.emit_runs = self.stats.emit_runs.saturating_add(1);
        }
        Ok(self
            .module_record(key)?
            .emitted
            .as_ref()
            .expect("emit cache missing after construction"))
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if module emission or binary/text assembly fails.
    pub fn compile_module(&mut self, key: &ModuleKey) -> Result<CompiledOutput, SessionError> {
        let artifact = self.compile_module_artifact(key)?;
        Self::build_output(artifact)
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if module emission fails.
    pub fn compile_module_artifact(&mut self, key: &ModuleKey) -> Result<Artifact, SessionError> {
        Ok(self.emit_module(key)?.artifact.clone())
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if module emission or binary encoding fails.
    pub fn compile_module_bytes(&mut self, key: &ModuleKey) -> Result<Vec<u8>, SessionError> {
        Ok(self.compile_module(key)?.bytes)
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if module emission or text formatting fails.
    pub fn compile_module_text(&mut self, key: &ModuleKey) -> Result<String, SessionError> {
        Ok(self.compile_module(key)?.text)
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the reachable entry graph cannot be resolved, checked, lowered,
    /// emitted, or assembled.
    pub fn compile_entry(&mut self, key: &ModuleKey) -> Result<CompiledOutput, SessionError> {
        let artifact = self.compile_entry_artifact(key)?;
        Self::build_output(artifact)
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the reachable entry graph cannot be emitted.
    ///
    /// # Panics
    ///
    /// Panics if the emitted entry-program cache is not populated after successful construction.
    pub fn compile_entry_artifact(&mut self, key: &ModuleKey) -> Result<Artifact, SessionError> {
        if !self.entry_programs.contains_key(key) {
            let modules = self.collect_reachable_ir_modules(key)?;
            let program = lower_ir_program(&modules, key, self.options.emit).map_err(|diags| {
                SessionError::Emit {
                    module: key.clone(),
                    diags: diags.into_boxed_slice(),
                }
            })?;
            let _ = self.entry_programs.insert(key.clone(), program);
            self.stats.emit_runs = self.stats.emit_runs.saturating_add(1);
        }
        Ok(self
            .entry_programs
            .get(key)
            .expect("entry program cache missing after construction")
            .artifact
            .clone())
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the reachable entry graph cannot be emitted or encoded.
    pub fn compile_entry_bytes(&mut self, key: &ModuleKey) -> Result<Vec<u8>, SessionError> {
        Ok(self.compile_entry(key)?.bytes)
    }

    /// # Errors
    ///
    /// Returns [`SessionError`] if the reachable entry graph cannot be emitted or formatted.
    pub fn compile_entry_text(&mut self, key: &ModuleKey) -> Result<String, SessionError> {
        Ok(self.compile_entry(key)?.text)
    }

    fn build_output(artifact: Artifact) -> Result<CompiledOutput, SessionError> {
        Ok(CompiledOutput {
            bytes: encode_binary(&artifact)?,
            text: format_text(&artifact),
            artifact,
        })
    }

    fn build_parsed_module(&mut self, key: &ModuleKey) -> Result<ParsedModule, SessionError> {
        let source_id = self.ensure_source_id(key)?;
        let text = self.module_record(key)?.text.clone();
        let lexed = Lexer::new(&text).lex();
        let lex_errors = lexed.errors().to_vec().into_boxed_slice();
        let parsed = parse(lexed);
        self.module_record_mut(key)?.parsed_source = Some(parsed.clone());
        let import_sites = collect_import_sites(source_id, parsed.tree()).into_boxed_slice();
        let export_summary = collect_export_summary(source_id, parsed.tree());
        let parse_errors = parsed.errors().to_vec().into_boxed_slice();
        Ok(ParsedModule {
            module_key: key.clone(),
            source_id,
            import_sites,
            export_summary,
            lex_errors,
            parse_errors,
        })
    }

    fn build_resolved_module(&mut self, key: &ModuleKey) -> Result<ResolvedModule, SessionError> {
        let source_id = self.parse_module(key)?.source_id;
        let parsed = self
            .module_record(key)?
            .parsed_source
            .as_ref()
            .expect("parsed source cache missing after successful parse")
            .clone();
        let module_keys = self.modules.keys().cloned().collect::<BTreeSet<_>>();
        let import_map = self.options.import_map.clone();
        let import_env = SessionImportEnv {
            import_map: &import_map,
            module_keys: &module_keys,
        };
        Ok(resolve_module(
            source_id,
            key,
            parsed.tree(),
            &mut self.interner,
            ResolveOptions {
                prelude: Vec::new(),
                import_env: Some(&import_env),
            },
        ))
    }

    fn build_sema_module(&mut self, key: &ModuleKey) -> Result<SemaModule, SessionError> {
        let resolved = self.resolve_module(key)?.clone();
        let mut surfaces = SurfaceMap::default();
        let imports = resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect::<Vec<_>>();
        for imported in imports {
            let sema = self.check_module(&imported)?;
            let _ = surfaces.surfaces.insert(imported, sema.surface().clone());
        }
        Ok(check_module(
            resolved,
            &mut self.interner,
            SemaOptions {
                target: self.options.target.clone(),
                env: Some(&surfaces),
            },
        ))
    }

    fn collect_reachable_ir_modules(
        &mut self,
        key: &ModuleKey,
    ) -> Result<Vec<IrModule>, SessionError> {
        let mut order = Vec::new();
        let mut seen = BTreeSet::new();
        self.collect_reachable_keys(key, &mut seen, &mut order)?;
        order
            .into_iter()
            .map(|module_key| self.lower_module(&module_key).cloned())
            .collect::<Result<Vec<_>, _>>()
    }

    fn collect_reachable_keys(
        &mut self,
        key: &ModuleKey,
        seen: &mut BTreeSet<ModuleKey>,
        order: &mut Vec<ModuleKey>,
    ) -> Result<(), SessionError> {
        if !seen.insert(key.clone()) {
            return Ok(());
        }
        let imports = self
            .resolve_module(key)?
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect::<Vec<_>>();
        for imported in imports {
            self.collect_reachable_keys(&imported, seen, order)?;
        }
        order.push(key.clone());
        Ok(())
    }

    fn ensure_source_id(&mut self, key: &ModuleKey) -> Result<SourceId, SessionError> {
        if let Some(source_id) = self.module_record(key)?.source_id {
            return Ok(source_id);
        }
        let text = self.module_record(key)?.text.clone();
        let source_id = self.sources.add(key.as_str(), text)?;
        self.module_record_mut(key)?.source_id = Some(source_id);
        Ok(source_id)
    }

    fn update_dependency_graph(&mut self, key: &ModuleKey, resolved: &ResolvedModule) {
        let new_imports = resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect::<BTreeSet<_>>();
        if let Some(record) = self.modules.get(key) {
            for previous in &record.static_imports {
                if let Some(dependents) = self.reverse_deps.get_mut(previous) {
                    let _ = dependents.remove(key);
                }
            }
        }
        for imported in &new_imports {
            let _ = self
                .reverse_deps
                .entry(imported.clone())
                .or_default()
                .insert(key.clone());
        }
        if let Ok(record) = self.module_record_mut(key) {
            record.static_imports = new_imports;
        }
    }

    fn invalidate_dependents(&mut self, key: &ModuleKey) {
        let dependents = self
            .reverse_deps
            .get(key)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect::<Vec<_>>();
        for dependent in dependents {
            if let Some(record) = self.modules.get_mut(&dependent) {
                record.resolved = None;
                record.sema = None;
                record.ir = None;
                record.emitted = None;
            }
            let _ = self.entry_programs.remove(&dependent);
            self.invalidate_dependents(&dependent);
        }
    }

    fn clear_all_resolve_caches(&mut self) {
        for record in self.modules.values_mut() {
            record.resolved = None;
            record.sema = None;
            record.ir = None;
            record.emitted = None;
        }
        self.entry_programs.clear();
        self.reverse_deps.clear();
    }

    fn module_record(&self, key: &ModuleKey) -> Result<&ModuleRecord, SessionError> {
        self.modules
            .get(key)
            .ok_or_else(|| SessionError::UnknownModule { key: key.clone() })
    }

    fn module_record_mut(&mut self, key: &ModuleKey) -> Result<&mut ModuleRecord, SessionError> {
        self.modules
            .get_mut(key)
            .ok_or_else(|| SessionError::UnknownModule { key: key.clone() })
    }
}
