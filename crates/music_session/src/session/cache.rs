use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use music_base::{SourceId, Span};
use music_emit::{EmittedModule, lower_ir_module};
use music_ir::{IrModule, lower_module};
use music_module::{ImportEnv, ModuleKey, collect_export_summary, collect_import_sites};
use music_module::{ImportSiteKind, ModuleSpecifier};
use music_names::Symbol;
use music_resolve::{ResolveOptions, ResolvedImport, ResolvedModule, resolve_module};
use music_sema::{ModuleSurface, SemaEnv, SemaModule, SemaOptions, check_module};
use music_syntax::{Lexer, parse};

use crate::api::{ParsedModule, SessionError, SessionSyntaxErrors};

use super::Session;
use super::graph::{SessionImportEnv, SurfaceMap};

const MAX_COMPTIME_EXPANSION_PASSES: usize = 32;

type ExpansionSeenSet = BTreeSet<ModuleKey>;

impl Session {
    fn is_std_prelude_module_key(key: &str) -> bool {
        key.starts_with("@@std@") && Path::new(key).ends_with(Path::new("prelude").join("index.ms"))
    }

    fn auto_prelude_target(&self, key: &ModuleKey) -> Option<ModuleKey> {
        if key.as_str().starts_with("musi:") || key.as_str().starts_with("@@std@") {
            return None;
        }
        let spec = ModuleSpecifier::new("@std/prelude");
        let target = self
            .options
            .import_map
            .resolve(key, &spec)
            .map(|resolved| ModuleKey::new(resolved.as_str()))
            .or_else(|| {
                self.store.modules.keys().find_map(|candidate| {
                    let key = candidate.as_str();
                    Self::is_std_prelude_module_key(key).then_some(candidate.clone())
                })
            })?;
        (target != *key).then_some(target)
    }

    fn auto_prelude_surface(
        &mut self,
        key: &ModuleKey,
    ) -> Result<Option<(ModuleKey, ModuleSurface)>, SessionError> {
        let Some(target) = self.auto_prelude_target(key) else {
            return Ok(None);
        };
        let sema = self.check_module(&target)?;
        Ok(Some((target, sema.surface().clone())))
    }

    fn auto_prelude_symbols(
        &mut self,
        key: &ModuleKey,
    ) -> Result<Option<(ModuleKey, Vec<Symbol>)>, SessionError> {
        let Some((target, surface)) = self.auto_prelude_surface(key)? else {
            return Ok(None);
        };
        let names = surface
            .exported_values()
            .iter()
            .map(|export| self.interner.intern(export.name.as_ref()))
            .collect::<Vec<_>>();
        Ok(Some((target, names)))
    }

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

    fn build_parsed_module(&mut self, key: &ModuleKey) -> Result<ParsedModule, SessionError> {
        let (source_id, text) = self.ensure_expanded_source(key)?;
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
        Ok(ParsedModule::new(key.clone(), source_id)
            .with_import_sites(import_sites)
            .with_export_summary(export_summary)
            .with_syntax(SessionSyntaxErrors::new(
                lex_errors,
                parse_errors,
                syntax_diags,
            )))
    }

    fn ensure_expanded_source(
        &mut self,
        key: &ModuleKey,
    ) -> Result<(SourceId, String), SessionError> {
        if let Some(source_id) = self.module_record(key)?.expanded_source_id {
            let text = self
                .module_record(key)?
                .expanded_text
                .as_ref()
                .expect("expanded source text missing")
                .clone();
            return Ok((source_id, text));
        }
        let raw = self.module_record(key)?.text.clone();
        let expanded = self.expand_comptime_module_items(key, &raw, &mut BTreeSet::new());
        let source_name = if expanded == raw {
            key.as_str().to_owned()
        } else {
            format!("{}#expanded", key.as_str())
        };
        let source_id = self.store.sources.add(source_name, expanded.clone())?;
        let record = self.module_record_mut(key)?;
        record.expanded_text = Some(expanded.clone());
        record.expanded_source_id = Some(source_id);
        Ok((source_id, expanded))
    }

    fn expand_comptime_module_items(
        &mut self,
        key: &ModuleKey,
        source: &str,
        seen: &mut ExpansionSeenSet,
    ) -> String {
        let mut expanded = source.to_owned();
        for _ in 0..MAX_COMPTIME_EXPANSION_PASSES {
            let context = self.module_expansion_context(key, &expanded, seen);
            let (next, changed) = expand_comptime_module_quote_pass(&expanded, &context);
            expanded = next;
            if !changed {
                return expanded;
            }
        }
        expanded
    }

    fn module_expansion_context(
        &mut self,
        key: &ModuleKey,
        source: &str,
        seen: &mut ExpansionSeenSet,
    ) -> ModuleExpansionContext {
        let local_syntax = collect_syntax_bindings(source, false);
        let local_factories = collect_syntax_factories(source, false);
        let mut imported_syntax = BTreeMap::<String, BTreeMap<String, String>>::new();
        let mut imported_factories = BTreeMap::<String, BTreeMap<String, SyntaxFactory>>::new();
        for (alias, target) in collect_import_aliases(self, key, source) {
            let Some(raw) = self.module_text(&target).map(ToOwned::to_owned) else {
                continue;
            };
            let expanded = if seen.insert(target.clone()) {
                self.expand_comptime_module_items(&target, &raw, seen)
            } else {
                raw
            };
            let _ = seen.remove(&target);
            let exports = collect_syntax_bindings(&expanded, true);
            if !exports.is_empty() {
                let _ = imported_syntax.insert(alias.clone(), exports);
            }
            let factories = collect_syntax_factories(&expanded, true);
            if !factories.is_empty() {
                let _ = imported_factories.insert(alias, factories);
            }
        }
        ModuleExpansionContext {
            local_syntax,
            imported_syntax,
            local_factories,
            imported_factories,
        }
    }

    fn build_resolved_module(&mut self, key: &ModuleKey) -> Result<ResolvedModule, SessionError> {
        let (source_id, import_sites, has_imports) = {
            let parsed = self.parse_module(key)?;
            (
                parsed.source_id,
                parsed.import_sites.clone(),
                !parsed.import_sites.is_empty(),
            )
        };
        let parsed = self
            .module_record(key)?
            .parsed_source
            .as_ref()
            .expect("parsed source cache missing after successful parse")
            .clone();
        let module_keys = self.store.modules.keys().cloned().collect::<BTreeSet<_>>();
        let import_map = self.options.import_map.clone();
        let session_import_env = SessionImportEnv {
            import_map: &import_map,
            module_keys: &module_keys,
        };
        let import_env: Option<&dyn ImportEnv> = if has_imports {
            Some(&session_import_env)
        } else {
            None
        };
        let mut import_export_summaries = BTreeMap::new();
        for site in &import_sites {
            let ImportSiteKind::Static { spec } = &site.kind else {
                continue;
            };
            let Ok(target) = session_import_env.resolve(key, spec) else {
                continue;
            };
            let target_summary = self.parse_module(&target)?.export_summary.clone();
            let _prev = import_export_summaries.insert(target, target_summary);
        }
        let import_export_summaries = import_export_summaries.into_iter().collect();
        let auto_prelude = self.auto_prelude_symbols(key)?;
        let inject_compiler_prelude = auto_prelude.is_none();
        let prelude = auto_prelude
            .as_ref()
            .map(|(_, names)| names.clone())
            .unwrap_or_default();
        let mut resolved = resolve_module(
            source_id,
            key,
            parsed.tree(),
            &mut self.interner,
            ResolveOptions {
                inject_compiler_prelude,
                prelude,
                import_env,
                import_export_summaries,
            },
        );
        if let Some((target, _)) = auto_prelude {
            resolved.imports.push(ResolvedImport {
                span: Span::DUMMY,
                spec: ModuleSpecifier::new("@std/prelude"),
                to: target,
            });
        }
        Ok(resolved)
    }

    fn build_sema_module(&mut self, key: &ModuleKey) -> Result<SemaModule, SessionError> {
        let resolved = self.resolve_module(key)?.clone();
        let mut surfaces = SurfaceMap::default();
        let mut seen = BTreeSet::new();
        self.collect_import_surfaces(&resolved, &mut seen, &mut surfaces)?;
        let prelude = self.auto_prelude_surface(key)?.map(|(target, surface)| {
            let _ = surfaces.surfaces.insert(target, surface.clone());
            surface
        });
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
                prelude: prelude.as_ref(),
            },
        ))
        .and_then(|mut sema| {
            if self.options.enable_ctfe && sema.diags().is_empty() {
                self.evaluate_module_comptime(key, &mut sema)?;
            }
            Ok(sema)
        })
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

struct ModuleExpansionContext {
    local_syntax: BTreeMap<String, String>,
    imported_syntax: BTreeMap<String, BTreeMap<String, String>>,
    local_factories: BTreeMap<String, SyntaxFactory>,
    imported_factories: BTreeMap<String, BTreeMap<String, SyntaxFactory>>,
}

#[derive(Clone)]
struct SyntaxFactory {
    params: Box<[String]>,
    body: String,
}

fn expand_comptime_module_quote_pass(
    source: &str,
    context: &ModuleExpansionContext,
) -> (String, bool) {
    let mut out = String::with_capacity(source.len());
    let mut cursor = 0;
    let mut changed = false;
    while let Some(relative_start) = source_tail(source, cursor).find("comptime") {
        let start = cursor + relative_start;
        if !is_token_start_boundary(source, start)
            || !is_token_end_boundary(source, start.saturating_add("comptime".len()))
            || !is_module_statement_start(source, start)
        {
            out.push_str(source_range(source, cursor, start + "comptime".len()));
            cursor = start + "comptime".len();
            continue;
        }
        let Some(expansion) = parse_comptime_module_expansion(source, start, context) else {
            out.push_str(source_range(source, cursor, start + "comptime".len()));
            cursor = start + "comptime".len();
            continue;
        };
        out.push_str(source_range(source, cursor, start));
        out.push_str(expansion.body.trim());
        out.push('\n');
        cursor = expansion.end;
        changed = true;
    }
    out.push_str(source_tail(source, cursor));
    (out, changed)
}

struct ModuleExpansion {
    body: String,
    end: usize,
}

fn parse_comptime_module_expansion(
    source: &str,
    start: usize,
    context: &ModuleExpansionContext,
) -> Option<ModuleExpansion> {
    if let Some(expansion) = parse_comptime_quote_block(source, start) {
        return Some(expansion);
    }
    parse_comptime_syntax_name(source, start, context)
}

fn parse_comptime_quote_block(source: &str, start: usize) -> Option<ModuleExpansion> {
    let mut index = skip_ws(source, start + "comptime".len());
    if !source_tail(source, index).starts_with("quote") {
        return None;
    }
    index = skip_ws(source, index + "quote".len());
    let open = source_tail(source, index).chars().next()?;
    if open != '{' {
        return None;
    }
    let body_start = index + open.len_utf8();
    let close = find_matching_brace(source, index)?;
    let mut end = skip_ws(source, close + 1);
    if source_tail(source, end).starts_with(';') {
        end += 1;
    }
    Some(ModuleExpansion {
        body: source_range(source, body_start, close).to_owned(),
        end,
    })
}

fn parse_comptime_syntax_name(
    source: &str,
    start: usize,
    context: &ModuleExpansionContext,
) -> Option<ModuleExpansion> {
    let expr_start = skip_ws(source, start + "comptime".len());
    let semicolon = find_statement_semicolon(source, expr_start)?;
    let expr = source_range(source, expr_start, semicolon).trim();
    let body = context
        .local_syntax
        .get(expr)
        .cloned()
        .or_else(|| imported_syntax_body(context, expr))
        .or_else(|| syntax_factory_body(context, expr))?;
    Some(ModuleExpansion {
        body,
        end: semicolon + 1,
    })
}

fn imported_syntax_body(context: &ModuleExpansionContext, expr: &str) -> Option<String> {
    let (alias, name) = expr.split_once('.')?;
    if !is_ident(alias) || !is_ident(name) {
        return None;
    }
    context.imported_syntax.get(alias)?.get(name).cloned()
}

fn syntax_factory_body(context: &ModuleExpansionContext, expr: &str) -> Option<String> {
    let call = parse_call_expr_text(expr)?;
    let factory = call.import.as_ref().map_or_else(
        || context.local_factories.get(&call.name),
        |alias| context.imported_factories.get(alias)?.get(&call.name),
    )?;
    render_syntax_factory(factory, &call.args)
}

struct ModuleCallExpr {
    import: Option<String>,
    name: String,
    args: Box<[String]>,
}

fn parse_call_expr_text(expr: &str) -> Option<ModuleCallExpr> {
    let open = expr.find('(')?;
    let close = expr.rfind(')')?;
    if close + 1 != expr.len() {
        return None;
    }
    let callee = source_range(expr, 0, open).trim();
    let (import, name) = callee
        .split_once('.')
        .map_or((None, callee), |(alias, name)| (Some(alias), name));
    if !is_ident(name) || import.is_some_and(|alias| !is_ident(alias)) {
        return None;
    }
    let args = split_arg_text(source_range(expr, open + 1, close));
    Some(ModuleCallExpr {
        import: import.map(ToOwned::to_owned),
        name: name.to_owned(),
        args: args.into_boxed_slice(),
    })
}

fn render_syntax_factory(factory: &SyntaxFactory, args: &[String]) -> Option<String> {
    if factory.params.len() != args.len() {
        return None;
    }
    let mut body = factory.body.clone();
    for (param, arg) in factory.params.iter().zip(args) {
        body = body.replace(&format!("#({param})"), arg);
    }
    Some(body)
}

fn collect_syntax_bindings(source: &str, exported_only: bool) -> BTreeMap<String, String> {
    let mut bindings = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        if exported_only && !has_export_prefix(source, start) {
            continue;
        }
        let Some((name, value_start)) = parse_let_value_start(source, cursor) else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(expansion) = parse_comptime_quote_block(source, value_start) else {
            continue;
        };
        let _ = bindings.insert(name, expansion.body);
    }
    bindings
}

fn collect_syntax_factories(source: &str, exported_only: bool) -> BTreeMap<String, SyntaxFactory> {
    let mut factories = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        if exported_only && !has_export_prefix(source, start) {
            continue;
        }
        let Some((name, params, value_start)) = parse_let_callable_value_start(source, cursor)
        else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(expansion) = parse_comptime_quote_block(source, value_start) else {
            continue;
        };
        let _ = factories.insert(
            name,
            SyntaxFactory {
                params: params.into_boxed_slice(),
                body: expansion.body,
            },
        );
    }
    factories
}

fn collect_import_aliases(
    session: &Session,
    key: &ModuleKey,
    source: &str,
) -> BTreeMap<String, ModuleKey> {
    let mut aliases = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        let Some((name, value_start)) = parse_let_value_start(source, cursor) else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(spec) = parse_import_spec(source, value_start) else {
            continue;
        };
        let target = session
            .options
            .import_map
            .resolve(key, &ModuleSpecifier::new(spec.as_str()))
            .map_or_else(
                || ModuleKey::new(spec.as_str()),
                |resolved| ModuleKey::new(resolved.as_str()),
            );
        let _ = aliases.insert(name, target);
    }
    aliases
}

fn parse_let_value_start(source: &str, mut index: usize) -> Option<(String, usize)> {
    index = skip_ws(source, index);
    let (name, next) = parse_ident_at(source, index)?;
    let value_marker = source_tail(source, next).find(":=")?;
    Some((name, next + value_marker + ":=".len()))
}

fn parse_let_callable_value_start(
    source: &str,
    mut index: usize,
) -> Option<(String, Vec<String>, usize)> {
    index = skip_ws(source, index);
    let (name, next) = parse_ident_at(source, index)?;
    let open = skip_ws(source, next);
    if !source_tail(source, open).starts_with('(') {
        return None;
    }
    let close = find_matching_paren(source, open)?;
    let params = parse_param_names(source_range(source, open + 1, close));
    let value_marker = source_tail(source, close + 1).find(":=")?;
    Some((name, params, close + 1 + value_marker + ":=".len()))
}

fn parse_param_names(text: &str) -> Vec<String> {
    split_arg_text(text)
        .into_iter()
        .filter_map(|param| {
            let trimmed = param.trim();
            let after_comptime = trimmed.strip_prefix("comptime").map_or(trimmed, str::trim);
            let name = after_comptime.split(':').next()?.trim();
            is_ident(name).then(|| name.to_owned())
        })
        .collect()
}

fn split_arg_text(text: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut start = 0;
    let mut depth = 0_u32;
    let mut index = 0;
    while let Some(ch) = source_tail(text, index).chars().next() {
        match ch {
            '(' | '[' | '{' => depth = depth.saturating_add(1),
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            '"' => {
                if let Some(end) = skip_string(text, index) {
                    index = end;
                }
            }
            ',' if depth == 0 => {
                args.push(source_range(text, start, index).trim().to_owned());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
        index += ch.len_utf8();
    }
    let tail = source_tail(text, start).trim();
    if !tail.is_empty() {
        args.push(tail.to_owned());
    }
    args
}

fn parse_import_spec(source: &str, start: usize) -> Option<String> {
    let mut index = start;
    if !source_tail(source, index).starts_with("import") {
        return None;
    }
    index = skip_ws(source, index + "import".len());
    let text = source_tail(source, index);
    let inner = text.strip_prefix('"')?;
    let mut spec = String::new();
    let mut escaped = false;
    for ch in inner.chars() {
        if escaped {
            spec.push(ch);
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == '"' {
            return Some(spec);
        } else {
            spec.push(ch);
        }
    }
    None
}

fn find_matching_paren(source: &str, open: usize) -> Option<usize> {
    find_matching_delimiter(source, open, '(', ')')
}

fn parse_ident_at(source: &str, start: usize) -> Option<(String, usize)> {
    let mut chars = source_tail(source, start).char_indices();
    let (_, first) = chars.next()?;
    if !is_ident_start(first) {
        return None;
    }
    let mut end = start + first.len_utf8();
    for (offset, ch) in chars {
        if !is_ident_char(ch) {
            return Some((source_range(source, start, end).to_owned(), start + offset));
        }
        end = start + offset + ch.len_utf8();
    }
    Some((source_tail(source, start).to_owned(), source.len()))
}

fn find_statement_semicolon(source: &str, start: usize) -> Option<usize> {
    let mut index = start;
    while let Some(ch) = source_tail(source, index).chars().next() {
        match ch {
            ';' => return Some(index),
            '"' => index = skip_string(source, index)?,
            _ => {}
        }
        index += ch.len_utf8();
    }
    None
}

fn skip_ws(source: &str, mut index: usize) -> usize {
    while let Some(ch) = source_tail(source, index).chars().next() {
        if !ch.is_whitespace() {
            break;
        }
        index += ch.len_utf8();
    }
    index
}

fn find_matching_brace(source: &str, open: usize) -> Option<usize> {
    find_matching_delimiter(source, open, '{', '}')
}

fn find_matching_delimiter(source: &str, open: usize, left: char, right: char) -> Option<usize> {
    let mut depth = 0_u32;
    let mut index = open;
    while let Some(ch) = source_tail(source, index).chars().next() {
        if ch == left {
            depth = depth.saturating_add(1);
        } else if ch == right {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(index);
            }
        } else if ch == '"' {
            index = skip_string(source, index)?;
        }
        index += ch.len_utf8();
    }
    None
}

fn skip_string(source: &str, start: usize) -> Option<usize> {
    let mut escaped = false;
    let mut index = start + 1;
    while let Some(ch) = source_tail(source, index).chars().next() {
        if escaped {
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == '"' {
            return Some(index);
        }
        index += ch.len_utf8();
    }
    None
}

fn is_token_start_boundary(source: &str, index: usize) -> bool {
    source_head(source, index)
        .chars()
        .next_back()
        .is_none_or(|ch| !is_ident_char(ch))
}

fn is_module_statement_start(source: &str, index: usize) -> bool {
    source_head(source, index)
        .chars()
        .rev()
        .find(|ch| !ch.is_whitespace())
        .is_none_or(|ch| matches!(ch, ';' | '{' | '}'))
}

fn has_export_prefix(source: &str, index: usize) -> bool {
    let before = source_head(source, index).trim_end();
    let Some(export_start) = before.rfind("export") else {
        return false;
    };
    source_tail(before, export_start).trim() == "export"
        && is_token_start_boundary(before, export_start)
        && is_token_end_boundary(before, export_start + "export".len())
}

fn source_head(source: &str, end: usize) -> &str {
    source.get(..end).unwrap_or_default()
}

fn source_tail(source: &str, start: usize) -> &str {
    source.get(start..).unwrap_or_default()
}

fn source_range(source: &str, start: usize, end: usize) -> &str {
    source.get(start..end).unwrap_or_default()
}

fn is_token_end_boundary(source: &str, index: usize) -> bool {
    source
        .get(index..)
        .and_then(|text| text.chars().next())
        .is_none_or(|ch| !is_ident_char(ch))
}

const fn is_ident_char(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
}

const fn is_ident_start(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | 'A'..='Z')
}

fn is_ident(text: &str) -> bool {
    let mut chars = text.chars();
    chars.next().is_some_and(is_ident_start) && chars.all(is_ident_char)
}
