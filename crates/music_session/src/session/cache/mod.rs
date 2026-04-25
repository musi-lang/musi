use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use music_base::{SourceId, Span};
use music_emit::{EmittedModule, lower_ir_module};
use music_ir::IrModule;
use music_ir_lower::lower_module;
use music_module::{ImportEnv, ModuleKey, collect_export_summary, collect_import_sites};
use music_module::{ImportSiteKind, ModuleSpecifier};
use music_names::Symbol;
use music_resolve::{ResolveOptions, ResolvedImport, ResolvedModule, resolve_module};
use music_sema::{ModuleSurface, SemaEnv, SemaModule, SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::api::{ParsedModule, SessionError, SessionSyntaxErrors};

use super::Session;
use super::graph::{SessionImportEnv, SurfaceMap};

mod expansion;
mod module_expansion;
mod pipeline;
mod prelude;

use expansion::{
    ModuleExpansionContext, SyntaxFactory, collect_import_aliases, collect_syntax_bindings,
    collect_syntax_factories, expand_comptime_module_quote_pass,
};

const MAX_COMPTIME_EXPANSION_PASSES: usize = 32;

type ExpansionSeenSet = BTreeSet<ModuleKey>;

impl Session {
    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered.
    pub fn parsed_module_cached(
        &self,
        key: &ModuleKey,
    ) -> Result<Option<&ParsedModule>, SessionError> {
        Ok(self.module_record(key)?.parsed.as_ref())
    }

    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered.
    pub fn resolved_module_cached(
        &self,
        key: &ModuleKey,
    ) -> Result<Option<&ResolvedModule>, SessionError> {
        Ok(self.module_record(key)?.resolved.as_ref())
    }

    /// # Errors
    ///
    /// Returns [`SessionError::ModuleNotRegistered`] if the module is not registered.
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
}
