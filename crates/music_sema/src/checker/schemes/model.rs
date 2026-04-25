use std::collections::HashMap;

use music_hir::HirTyId;
use music_names::Symbol;

use crate::api::{ConstraintFacts, ConstraintKey, ConstraintKind, DefinitionKey};
use crate::effects::EffectRow;

pub(super) type TypeSubstMap = HashMap<Symbol, HirTyId>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingScheme {
    pub type_params: Box<[Symbol]>,
    pub type_param_kinds: Box<[HirTyId]>,
    pub param_names: Box<[Symbol]>,
    pub comptime_params: Box<[bool]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub ty: HirTyId,
    pub effects: EffectRow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintObligation {
    pub kind: ConstraintKind,
    pub subject: HirTyId,
    pub value: HirTyId,
    pub shape_key: Option<DefinitionKey>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstantiatedBinding {
    pub ty: HirTyId,
    pub effects: EffectRow,
    pub obligations: Box<[ConstraintObligation]>,
}

impl ConstraintObligation {
    #[must_use]
    pub fn key(&self) -> ConstraintKey {
        ConstraintKey::new(self.kind, self.subject, self.value, self.shape_key.clone())
    }
}
