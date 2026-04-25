use std::collections::{BTreeMap, HashMap};

use music_hir::HirTyId;
use music_names::{NameBindingId, Symbol};

use crate::api::{DefinitionKey, SemaDataDef, SemaDataVariantDef};
use crate::checker::state::aliases::SYNTH_SUM_PREFIX;

use crate::checker::state::{DataDef, EffectDef, PassBase};

impl PassBase<'_, '_, '_> {
    pub fn effect_def(&self, name: &str) -> Option<&EffectDef> {
        self.decls.effect_defs.get(name)
    }

    pub fn insert_effect_def(&mut self, name: impl Into<Box<str>>, def: EffectDef) {
        let _prev = self.decls.effect_defs.insert(name.into(), def);
    }

    pub fn data_def(&self, name: &str) -> Option<&DataDef> {
        self.decls.data_defs.get(name)
    }

    pub const fn data_defs(&self) -> &HashMap<Box<str>, DataDef> {
        &self.decls.data_defs
    }

    pub fn insert_data_def(&mut self, name: impl Into<Box<str>>, def: DataDef) {
        let _prev = self.decls.data_defs.insert(name.into(), def);
    }

    pub fn insert_attached_method(&mut self, name: Symbol, binding: NameBindingId) {
        self.typing
            .attached_methods
            .entry(name)
            .or_default()
            .push(binding);
    }

    pub fn visible_attached_methods_named(&self, name: Symbol) -> Box<[NameBindingId]> {
        self.typing
            .attached_methods
            .get(&name)
            .cloned()
            .unwrap_or_default()
            .into_boxed_slice()
    }

    pub fn visible_callable_bindings_named(&self, name: Symbol) -> Box<[NameBindingId]> {
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .filter_map(|(id, binding)| {
                (binding.name == name && self.typing.binding_schemes.contains_key(&id))
                    .then_some(id)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub fn ensure_sum_data_def(&mut self, left: HirTyId, right: HirTyId) -> Box<str> {
        let name: Box<str> = format!("{SYNTH_SUM_PREFIX}{}_{}", left.raw(), right.raw()).into();
        if self.decls.data_defs.contains_key(name.as_ref()) {
            return name;
        }

        let key = DefinitionKey::new(self.module_key().clone(), name.clone());
        let variants = BTreeMap::from([
            (
                "Left".into(),
                SemaDataVariantDef::new(
                    0,
                    Some(left),
                    None,
                    vec![left].into_boxed_slice(),
                    Box::default(),
                ),
            ),
            (
                "Right".into(),
                SemaDataVariantDef::new(
                    1,
                    Some(right),
                    None,
                    vec![right].into_boxed_slice(),
                    Box::default(),
                ),
            ),
        ]);
        let _prev = self.decls.data_defs.insert(
            name.clone(),
            SemaDataDef::new(key, variants, None, None, None, false),
        );
        name
    }
}
