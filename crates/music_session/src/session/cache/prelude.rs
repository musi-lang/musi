use super::*;

impl Session {
    pub(super) fn is_std_prelude_module_key(key: &str) -> bool {
        key.starts_with("@@std@")
            && (Path::new(key).ends_with("prelude.ms")
                || Path::new(key).ends_with(Path::new("prelude").join("std.ms")))
    }

    pub(super) fn auto_prelude_target(&self, key: &ModuleKey) -> Option<ModuleKey> {
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

    pub(super) fn auto_prelude_surface(
        &mut self,
        key: &ModuleKey,
    ) -> Result<Option<(ModuleKey, ModuleSurface)>, SessionError> {
        let Some(target) = self.auto_prelude_target(key) else {
            return Ok(None);
        };
        let sema = self.check_module(&target)?;
        Ok(Some((target, sema.surface().clone())))
    }

    pub(super) fn auto_prelude_symbols(
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
}
