use std::collections::BTreeSet;

use crate::api::ModuleSurface;

use super::surface_exports::{
    collect_exported_classes, collect_exported_data, collect_exported_effects,
    collect_exported_instances, collect_exported_values, collect_module_exports,
};
use super::surface_types::SurfaceTyBuilder;
pub use super::surface_types::{canonical_surface_ty, import_surface_ty, surface_key};
use super::{DeclState, ModuleState, RuntimeEnv, TypingState};

pub fn build_module_surface(
    module: &ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: &DeclState,
) -> ModuleSurface {
    let mut tys = SurfaceTyBuilder::new(&module.resolved.module.store, runtime.interner());
    let exports = collect_module_exports(module, runtime.interner());
    let static_imports = module
        .resolved
        .imports
        .iter()
        .map(|import| import.to.clone())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let exported_values = collect_exported_values(module, typing, decls, &exports, &mut tys);
    let exported_data = collect_exported_data(module, decls, &exports, &mut tys);
    let exported_classes = collect_exported_classes(module, decls, &exports, &mut tys);
    let exported_effects = collect_exported_effects(module, decls, &exports, &mut tys);
    let exported_instances = collect_exported_instances(module, decls, &exports, &mut tys);

    ModuleSurface {
        module_key: module.resolved.module_key.clone(),
        static_imports,
        tys: tys.finish(),
        exported_values,
        exported_data,
        exported_classes,
        exported_effects,
        exported_instances,
    }
}
