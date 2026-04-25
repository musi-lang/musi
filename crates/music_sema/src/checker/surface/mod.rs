mod attrs;
mod exports;
mod types;

use std::collections::BTreeSet;

use crate::api::ModuleSurface;

use self::exports::{ExportSurfaceCollector, collect_module_exports};
use self::types::SurfaceTyBuilder;
pub use self::types::{canonical_surface_ty, import_surface_ty, surface_key};
use super::{DeclState, ModuleState, RuntimeEnv, TypingState};

pub fn build_module_surface(
    module: &ModuleState,
    runtime: &RuntimeEnv<'_, '_>,
    typing: &TypingState,
    decls: &DeclState,
) -> ModuleSurface {
    let tys = SurfaceTyBuilder::new(&module.resolved.module.store, runtime.interner());
    let exports = collect_module_exports(module, runtime.interner());
    let mut collector = ExportSurfaceCollector::new(module, &exports, tys);
    let static_imports = module
        .resolved
        .imports
        .iter()
        .map(|import| import.to.clone())
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let exported_values = collector.collect_exported_values(typing, decls);
    let exported_data = collector.collect_exported_data(decls);
    let exported_shapes = collector.collect_exported_shapes(decls);
    let exported_effects = collector.collect_exported_effects(decls);
    let exported_givens = collector.collect_exported_givens(decls);

    ModuleSurface::from_collected(
        module.resolved.module_key.clone(),
        static_imports,
        collector.finish(),
        (
            exported_values,
            exported_data,
            exported_shapes,
            exported_effects,
            exported_givens,
        ),
    )
}
