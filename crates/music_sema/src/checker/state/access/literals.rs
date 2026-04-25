use music_hir::{HirLitId, HirLitKind};

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn lit_kind(&self, lit: HirLitId) -> HirLitKind {
        self.lit(lit).kind
    }

    pub fn lit_is_string(&self, lit: HirLitId) -> bool {
        matches!(self.lit_kind(lit), HirLitKind::String { .. })
    }

    pub fn lit_string_value(&self, lit: HirLitId) -> Option<String> {
        match self.lit_kind(lit) {
            HirLitKind::String { value } => Some(value.into()),
            _ => None,
        }
    }
}
