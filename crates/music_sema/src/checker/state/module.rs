use music_names::{NameBindingId, NameSite};
use music_resolve::ResolvedModule;

use super::aliases::{BindingIdMap, ImportTargetMap};

pub struct ModuleState {
    pub resolved: ResolvedModule,
    pub binding_ids: BindingIdMap,
    pub import_targets: ImportTargetMap,
}

impl ModuleState {
    pub const fn new(
        resolved: ResolvedModule,
        binding_ids: BindingIdMap,
        import_targets: ImportTargetMap,
    ) -> Self {
        Self {
            resolved,
            binding_ids,
            import_targets,
        }
    }
}

impl ModuleState {
    pub fn binding_id_at_site(&self, site: NameSite) -> Option<NameBindingId> {
        self.binding_ids.get(&site).copied()
    }
}
