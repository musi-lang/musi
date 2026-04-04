mod api;
mod attrs;
mod collect;
mod context;
mod decls;
mod effects;
mod engine;
mod exprs;
mod normalize;
mod patterns;
mod surface;

pub use api::{
    ClassMemberSurface, ClassSurface, ConstraintSurface, DefinitionKey, EffectOpSurface,
    EffectSurface, ExportedValue, InstanceSurface, ModuleSurface, SemaDiagList, SemaEnv,
    SemaModule, SemaOptions, SurfaceDim, SurfaceTy, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
    TargetInfo,
};
pub use effects::{EffectKey, EffectRow};
pub use engine::check_module;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
