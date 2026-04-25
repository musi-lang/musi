use super::*;

impl Session {
    pub(super) fn build_parsed_module(
        &mut self,
        key: &ModuleKey,
    ) -> Result<ParsedModule, SessionError> {
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

    pub(super) fn build_resolved_module(
        &mut self,
        key: &ModuleKey,
    ) -> Result<ResolvedModule, SessionError> {
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

    pub(super) fn build_sema_module(
        &mut self,
        key: &ModuleKey,
    ) -> Result<SemaModule, SessionError> {
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

    pub(super) fn collect_import_surfaces(
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
