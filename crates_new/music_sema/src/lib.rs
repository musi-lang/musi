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

pub use api::{SemaDiagList, SemaModule, SemaOptions, TargetInfo};
pub use effects::{EffectKey, EffectRow};
pub use engine::check_module;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
