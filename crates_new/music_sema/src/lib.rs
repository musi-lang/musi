mod api;
mod checker;
mod effects;

pub use api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ClassMemberSurface, ClassSurface, ConstraintKind,
    ConstraintSurface, DataSurface, DataVariantSurface, DefinitionKey, EffectOpSurface,
    EffectSurface, ExportedValue, InstanceSurface, ModuleSurface, SemaDataDef, SemaDataVariantDef,
    SemaDiagList, SemaEffectDef, SemaEffectOpDef, SemaEnv, SemaModule, SemaOptions, SurfaceDim,
    SurfaceEffectItem, SurfaceEffectRow, SurfaceTy, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
    TargetInfo,
};
pub use checker::check_module;
pub use effects::{EffectKey, EffectRow};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
