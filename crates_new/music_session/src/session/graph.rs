use std::collections::{BTreeMap, BTreeSet};

use music_emit::EmittedProgram;
use music_ir::IrModule;
use music_module::{
    ImportEnv, ImportError, ImportErrorKind, ImportMap, ImportResolveResult, ModuleKey,
    ModuleSpecifier,
};
use music_resolve::ResolvedModule;
use music_sema::{ModuleSurface, SemaEnv};

use crate::api::SessionError;

use super::Session;

#[derive(Default)]
pub(super) struct SessionGraph {
    pub(super) reverse_deps: BTreeMap<ModuleKey, BTreeSet<ModuleKey>>,
    pub(super) entry_programs: BTreeMap<ModuleKey, EmittedProgram>,
}

pub(super) struct SessionImportEnv<'session> {
    pub(super) import_map: &'session ImportMap,
    pub(super) module_keys: &'session BTreeSet<ModuleKey>,
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
pub(super) struct SurfaceMap {
    pub(super) surfaces: BTreeMap<ModuleKey, ModuleSurface>,
}

impl SemaEnv for SurfaceMap {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface> {
        self.surfaces.get(key).cloned()
    }
}

impl Session {
    pub fn set_import_map(&mut self, import_map: ImportMap) {
        self.options.import_map = import_map;
        self.clear_all_resolve_caches();
    }

    pub(super) fn collect_reachable_ir_modules(
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

    pub(super) fn update_dependency_graph(&mut self, key: &ModuleKey, resolved: &ResolvedModule) {
        let new_imports = resolved
            .imports
            .iter()
            .map(|import| import.to.clone())
            .collect::<BTreeSet<_>>();
        if let Some(record) = self.store.modules.get(key) {
            for previous in &record.static_imports {
                if let Some(dependents) = self.graph.reverse_deps.get_mut(previous) {
                    let _ = dependents.remove(key);
                }
            }
        }
        for imported in &new_imports {
            let _ = self
                .graph
                .reverse_deps
                .entry(imported.clone())
                .or_default()
                .insert(key.clone());
        }
        if let Ok(record) = self.module_record_mut(key) {
            record.static_imports = new_imports;
        }
    }

    pub(super) fn invalidate_dependents(&mut self, key: &ModuleKey) {
        let dependents = self
            .graph
            .reverse_deps
            .get(key)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .collect::<Vec<_>>();
        for dependent in dependents {
            if let Some(record) = self.store.modules.get_mut(&dependent) {
                record.resolved = None;
                record.sema = None;
                record.ir = None;
                record.emitted = None;
            }
            let _ = self.graph.entry_programs.remove(&dependent);
            self.invalidate_dependents(&dependent);
        }
    }

    pub(super) fn clear_all_resolve_caches(&mut self) {
        for record in self.store.modules.values_mut() {
            record.resolved = None;
            record.sema = None;
            record.ir = None;
            record.emitted = None;
        }
        self.graph.entry_programs.clear();
        self.graph.reverse_deps.clear();
    }
}
