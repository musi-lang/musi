use std::collections::BTreeSet;

use music_emit::{EmittedModule, lower_ir_module};
use music_ir::{IrModule, lower_module};
use music_module::{ImportEnv, ModuleKey, collect_export_summary, collect_import_sites};
use music_resolve::{ResolveOptions, ResolvedModule, resolve_module};
use music_sema::{SemaEnv, SemaModule, SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::api::{ParsedModule, SessionError, SessionSyntaxErrors};

use super::Session;
use super::graph::{SessionImportEnv, SurfaceMap};

impl Session {
    #[must_use]
    pub fn parsed_module_cached(
        &self,
        key: &ModuleKey,
    ) -> Result<Option<&ParsedModule>, SessionError> {
        Ok(self.module_record(key)?.parsed.as_ref())
    }

    #[must_use]
    pub fn resolved_module_cached(
        &self,
        key: &ModuleKey,
    ) -> Result<Option<&ResolvedModule>, SessionError> {
        Ok(self.module_record(key)?.resolved.as_ref())
    }

    #[must_use]
    pub fn sema_module_cached(&self, key: &ModuleKey) -> Result<Option<&SemaModule>, SessionError> {
        Ok(self.module_record(key)?.sema.as_ref())
    }

    /// Parses the module source and returns the cached parse summary.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered, or
    /// [`SessionError::ModuleParseFailed`] if lexing or parsing produced errors.
    ///
    /// # Panics
    ///
    /// Panics if the parse cache is missing immediately after successful cache construction.
    pub fn parse_module(&mut self, key: &ModuleKey) -> Result<&ParsedModule, SessionError> {
        if self
            .store
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
        if parsed.syntax.is_empty() {
            Ok(parsed)
        } else {
            Err(SessionError::ModuleParseFailed {
                module: key.clone(),
                syntax: parsed.syntax.clone(),
            })
        }
    }

    /// Resolves names and imports for a module and returns the cached resolved product.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered,
    /// [`SessionError::ModuleParseFailed`] if parsing failed, or [`SessionError::ModuleResolveFailed`] if
    /// resolution diagnostics were produced.
    ///
    /// # Panics
    ///
    /// Panics if the resolve cache is missing immediately after successful cache construction.
    pub fn resolve_module(&mut self, key: &ModuleKey) -> Result<&ResolvedModule, SessionError> {
        if self
            .store
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
            Err(SessionError::ModuleResolveFailed {
                module: key.clone(),
                diags: resolved.diags.clone().into_boxed_slice(),
            })
        }
    }

    /// Runs semantic analysis for a module and returns the cached semantic facts.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered,
    /// [`SessionError::ModuleParseFailed`] or [`SessionError::ModuleResolveFailed`] if earlier phases failed,
    /// or [`SessionError::ModuleSemanticCheckFailed`] if semantic diagnostics were produced.
    ///
    /// # Panics
    ///
    /// Panics if the semantic cache is missing immediately after successful cache construction.
    pub fn check_module(&mut self, key: &ModuleKey) -> Result<&SemaModule, SessionError> {
        if self
            .store
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
            Err(SessionError::ModuleSemanticCheckFailed {
                module: key.clone(),
                diags: sema.diags().to_vec().into_boxed_slice(),
            })
        }
    }

    /// Lowers a checked module into codegen-facing IR and returns the cached result.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered,
    /// any earlier phase error, or [`SessionError::ModuleLoweringFailed`] if IR lowering fails.
    ///
    /// # Panics
    ///
    /// Panics if the semantic cache is missing immediately after successful checking,
    /// or if the IR cache is missing immediately after successful cache construction.
    pub fn lower_module(&mut self, key: &ModuleKey) -> Result<&IrModule, SessionError> {
        if self
            .store
            .modules
            .get(key)
            .and_then(|record| record.ir.as_ref())
            .is_none()
        {
            #[cfg(test)]
            if let Some(diags) = self.test_hooks.ir_failure.take() {
                return Err(SessionError::ModuleLoweringFailed {
                    module: key.clone(),
                    diags,
                });
            }
            let _ = self.check_module(key)?;
            let ir = {
                let sema = self
                    .module_record(key)?
                    .sema
                    .as_ref()
                    .expect("sema cache missing after successful check");
                lower_module(sema, &self.interner).map_err(|diags| {
                    SessionError::ModuleLoweringFailed {
                        module: key.clone(),
                        diags: diags.into_boxed_slice(),
                    }
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

    /// Emits a single module artifact and returns the cached emitted product.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered,
    /// any earlier phase error, or [`SessionError::ModuleEmissionFailed`] if artifact emission fails.
    ///
    /// # Panics
    ///
    /// Panics if the IR cache is missing immediately after successful lowering,
    /// or if the emitted cache is missing immediately after successful cache construction.
    pub fn emit_module(&mut self, key: &ModuleKey) -> Result<&EmittedModule, SessionError> {
        if self
            .store
            .modules
            .get(key)
            .and_then(|record| record.emitted.as_ref())
            .is_none()
        {
            #[cfg(test)]
            if let Some(diags) = self.test_hooks.emit_failure.take() {
                return Err(SessionError::ModuleEmissionFailed {
                    module: key.clone(),
                    diags,
                });
            }
            let _ = self.lower_module(key)?;
            let emit_options = self.options.emit;
            let emitted = {
                let ir = self
                    .module_record(key)?
                    .ir
                    .as_ref()
                    .expect("ir cache missing after successful lowering");
                lower_ir_module(ir, emit_options).map_err(|diags| {
                    SessionError::ModuleEmissionFailed {
                        module: key.clone(),
                        diags: diags.into_boxed_slice(),
                    }
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
        let syntax_diags = lex_errors
            .iter()
            .copied()
            .map(|error| error.to_diag(source_id, &text))
            .chain(
                parse_errors
                    .iter()
                    .copied()
                    .map(|error| error.to_diag(source_id, &text)),
            )
            .collect::<Vec<_>>()
            .into_boxed_slice();
        Ok(ParsedModule {
            module_key: key.clone(),
            source_id,
            import_sites,
            export_summary,
            syntax: SessionSyntaxErrors::new(lex_errors, parse_errors, syntax_diags),
        })
    }

    fn build_resolved_module(&mut self, key: &ModuleKey) -> Result<ResolvedModule, SessionError> {
        let (source_id, has_imports) = {
            let parsed = self.parse_module(key)?;
            (parsed.source_id, !parsed.import_sites.is_empty())
        };
        let parsed = self
            .module_record(key)?
            .parsed_source
            .as_ref()
            .expect("parsed source cache missing after successful parse")
            .clone();
        let module_keys = self.store.modules.keys().cloned().collect::<BTreeSet<_>>();
        let import_map = self.options.import_map.clone();
        let import_env = SessionImportEnv {
            import_map: &import_map,
            module_keys: &module_keys,
        };
        let import_env: Option<&dyn ImportEnv> = if has_imports { Some(&import_env) } else { None };
        Ok(resolve_module(
            source_id,
            key,
            parsed.tree(),
            &mut self.interner,
            ResolveOptions {
                prelude: Vec::new(),
                import_env,
            },
        ))
    }

    fn build_sema_module(&mut self, key: &ModuleKey) -> Result<SemaModule, SessionError> {
        let resolved = self.resolve_module(key)?.clone();
        let mut surfaces = SurfaceMap::default();
        let mut seen = BTreeSet::new();
        self.collect_import_surfaces(&resolved, &mut seen, &mut surfaces)?;
        let env: Option<&dyn SemaEnv> = if surfaces.surfaces.is_empty() {
            None
        } else {
            Some(&surfaces)
        };
        Ok(check_module(
            resolved,
            &mut self.interner,
            SemaOptions {
                target: self.options.target.clone(),
                env,
            },
        ))
    }

    fn collect_import_surfaces(
        &mut self,
        resolved: &ResolvedModule,
        seen: &mut BTreeSet<ModuleKey>,
        surfaces: &mut SurfaceMap,
    ) -> Result<(), SessionError> {
        for imported in resolved.imports.iter().map(|import| import.to.clone()) {
            if !seen.insert(imported.clone()) {
                continue;
            }
            let sema = self.check_module(&imported)?;
            let _ = surfaces
                .surfaces
                .insert(imported.clone(), sema.surface().clone());
            let imported_resolved = self.resolve_module(&imported)?.clone();
            self.collect_import_surfaces(&imported_resolved, seen, surfaces)?;
        }
        Ok(())
    }
}
