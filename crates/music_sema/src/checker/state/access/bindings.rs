use music_hir::HirTyId;
use music_module::ModuleKey;
use music_names::NameBindingId;

use crate::api::{ComptimeValue, ConstraintKey, DefinitionKey, ForeignLinkInfo};
use crate::checker::schemes::BindingScheme;
use crate::effects::EffectRow;

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn binding_type(&self, id: NameBindingId) -> Option<HirTyId> {
        self.typing.binding_types.get(&id).copied()
    }

    pub fn insert_binding_type(&mut self, id: NameBindingId, ty: HirTyId) {
        let _prev = self.typing.binding_types.insert(id, ty);
    }

    pub fn binding_effects(&self, id: NameBindingId) -> Option<EffectRow> {
        self.typing.binding_effects.get(&id).cloned()
    }

    pub fn insert_binding_effects(&mut self, id: NameBindingId, effects: EffectRow) {
        let _prev = self.typing.binding_effects.insert(id, effects);
    }

    pub fn binding_scheme(&self, id: NameBindingId) -> Option<&BindingScheme> {
        self.typing.binding_schemes.get(&id)
    }

    pub fn insert_binding_scheme(&mut self, id: NameBindingId, scheme: BindingScheme) {
        let _prev = self.typing.binding_schemes.insert(id, scheme);
    }

    pub fn set_binding_constraint_keys(
        &mut self,
        id: NameBindingId,
        keys: impl Into<Box<[ConstraintKey]>>,
    ) {
        let _prev = self.typing.binding_constraint_keys.insert(id, keys.into());
    }

    pub fn binding_import_record_target(&self, id: NameBindingId) -> Option<&ModuleKey> {
        self.typing.binding_import_record_targets.get(&id)
    }

    pub fn binding_comptime_value(&self, id: NameBindingId) -> Option<&ComptimeValue> {
        self.typing.binding_comptime_values.get(&id)
    }

    pub fn insert_binding_const_int(&mut self, id: NameBindingId, value: i64) {
        let _prev = self.typing.binding_const_ints.insert(id, value);
        let _prev = self
            .typing
            .binding_comptime_values
            .insert(id, ComptimeValue::Int(value));
    }

    pub fn insert_binding_comptime_value(&mut self, id: NameBindingId, value: ComptimeValue) {
        match value {
            ComptimeValue::Int(int) => self.insert_binding_const_int(id, int),
            other => {
                let _prev = self.typing.binding_comptime_values.insert(id, other);
            }
        }
    }

    pub fn insert_binding_import_record_target(&mut self, id: NameBindingId, target: ModuleKey) {
        let _prev = self.typing.binding_import_record_targets.insert(id, target);
    }

    pub fn mark_sealed_shape(&mut self, key: DefinitionKey) {
        let _ = self.typing.sealed_shapes.insert(key);
    }

    pub fn is_sealed_shape(&self, key: &DefinitionKey) -> bool {
        self.typing.sealed_shapes.contains(key)
    }

    pub fn mark_gated_binding(&mut self, id: NameBindingId) {
        let _ = self.typing.gated_bindings.insert(id);
    }

    pub fn is_gated_binding(&self, id: NameBindingId) -> bool {
        self.typing.gated_bindings.contains(&id)
    }

    pub fn set_foreign_link(&mut self, binding: NameBindingId, link: ForeignLinkInfo) {
        let _prev = self.typing.foreign_links.insert(binding, link);
    }

    pub fn mark_unsafe_binding(&mut self, binding: NameBindingId) {
        let _ = self.typing.unsafe_bindings.insert(binding);
    }

    pub fn is_unsafe_binding(&self, binding: NameBindingId) -> bool {
        self.typing.unsafe_bindings.contains(&binding)
    }
}
