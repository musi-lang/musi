use super::*;

impl Session {
    pub(super) fn ensure_expanded_source(
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

    pub(super) fn expand_comptime_module_items(
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

    pub(super) fn module_expansion_context(
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
}
