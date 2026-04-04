use std::collections::BTreeSet;

use music_emit::{EmittedModule, lower_ir_module};
use music_ir::{IrModule, lower_module};
use music_module::{ImportEnv, ModuleKey, collect_export_summary, collect_import_sites};
use music_resolve::{ResolveOptions, ResolvedModule, resolve_module};
use music_sema::{SemaEnv, SemaModule, SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::api::{ParsedModule, SessionError};

use super::Session;
use super::graph::{SessionImportEnv, SurfaceMap};

impl Session {
    /// Parses the module source and returns the cached parse summary.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::UnknownModule`] if the module is not registered, or
    /// [`SessionError::Parse`] if lexing or parsing produced errors.
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

    /// Resolves names and imports for a module and returns the cached resolved product.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::UnknownModule`] if the module is not registered,
    /// [`SessionError::Parse`] if parsing failed, or [`SessionError::Resolve`] if
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
            Err(SessionError::Resolve {
                module: key.clone(),
                diags: resolved.diags.clone().into_boxed_slice(),
            })
        }
    }

    /// Runs semantic analysis for a module and returns the cached semantic facts.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::UnknownModule`] if the module is not registered,
    /// [`SessionError::Parse`] or [`SessionError::Resolve`] if earlier phases failed,
    /// or [`SessionError::Sema`] if semantic diagnostics were produced.
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
            Err(SessionError::Sema {
                module: key.clone(),
                diags: sema.diags().to_vec().into_boxed_slice(),
            })
        }
    }

    /// Lowers a checked module into codegen-facing IR and returns the cached result.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::UnknownModule`] if the module is not registered,
    /// any earlier phase error, or [`SessionError::Ir`] if IR lowering fails.
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

    /// Emits a single module artifact and returns the cached emitted product.
    ///
    /// # Errors
    ///
    /// Returns [`SessionError::UnknownModule`] if the module is not registered,
    /// any earlier phase error, or [`SessionError::Emit`] if artifact emission fails.
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
        let import_env: Option<&dyn ImportEnv> = if has_imports {
            Some(&import_env)
        } else {
            None
        };
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
        let imports = resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect::<Vec<_>>();
        for imported in imports {
            let sema = self.check_module(&imported)?;
            let _ = surfaces.surfaces.insert(imported, sema.surface().clone());
        }
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
}
