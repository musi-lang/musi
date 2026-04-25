use music_module::ModuleKey;

use super::{ModuleSurface, TargetInfo};

pub trait SemaEnv {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface>;
}

#[derive(Clone, Default)]
pub struct SemaOptions<'env> {
    pub target: Option<TargetInfo>,
    pub env: Option<&'env dyn SemaEnv>,
    pub prelude: Option<&'env ModuleSurface>,
}
