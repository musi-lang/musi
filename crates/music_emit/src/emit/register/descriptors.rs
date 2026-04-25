use super::*;
use music_ir::lower_surface_type_term;
use music_seam::descriptor::{DataVariantDescriptor, ShapeDescriptor};

pub(super) fn register_types(state: &mut ProgramState, module: &IrModule) {
    let mut seen = BTreeSet::<String>::new();
    for ty in module.types() {
        let term = lower_surface_type_term(module.types(), ty);
        let name = format!("{term}");
        if seen.insert(name.clone()) {
            let name_id = state.artifact.intern_string(&name);
            let term_json = term.to_json();
            let term_id = state.artifact.intern_string(&term_json);
            let _ = state
                .artifact
                .types
                .alloc(TypeDescriptor::new(name_id, term_id));
        }
    }
}

pub(super) fn register_data_defs(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for data in module.data_defs() {
        let name = qualified_name(&data.key.module, &data.key.name);
        let _ty = ensure_type(state, layout, name.as_ref());
        let repr_kind = data
            .repr_kind
            .as_deref()
            .map(|kind| state.artifact.intern_string(kind));
        let name_id = state.artifact.intern_string(name.as_ref());
        let variants = data
            .variants
            .iter()
            .map(|variant| {
                DataVariantDescriptor::new(
                    state.artifact.intern_string(variant.name.as_ref()),
                    variant.tag,
                    variant
                        .field_tys
                        .iter()
                        .map(|ty| ensure_type(state, layout, ty.as_ref()))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                )
            })
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let mut descriptor = DataDescriptor::new(name_id, variants);
        if let Some(repr_kind) = repr_kind {
            descriptor = descriptor.with_repr_kind(repr_kind);
        }
        if let Some(layout_align) = data.layout_align {
            descriptor = descriptor.with_layout_align(layout_align);
        }
        if let Some(layout_pack) = data.layout_pack {
            descriptor = descriptor.with_layout_pack(layout_pack);
        }
        if data.frozen {
            descriptor = descriptor.with_frozen(true);
        }
        let _ = state.artifact.data.alloc(descriptor);
    }
}

pub(super) fn register_effects(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for effect in module.effects() {
        let effect_id = ensure_effect(state, effect, layout);
        let _ = layout.effects.insert(effect.key.clone(), effect_id);
    }
}

pub(super) fn register_shapes(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for shape in module.shapes() {
        let name = qualified_name(&shape.key.module, &shape.key.name);
        let name_id = state.artifact.intern_string(name.as_ref());
        let id = state.artifact.shapes.alloc(ShapeDescriptor::new(name_id));
        let _ = layout.shapes.insert(shape.key.clone(), id);
    }
}

pub(super) fn register_meta(state: &mut ProgramState, module: &IrModule) {
    for record in module.meta() {
        let target = state.artifact.intern_string(record.target.as_ref());
        let key = state.artifact.intern_string(record.key.as_ref());
        let values = record
            .values
            .iter()
            .map(|value| state.artifact.intern_string(value.as_ref()))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let _ = state
            .artifact
            .meta
            .alloc(MetaDescriptor::new(target, key, values));
    }
}

pub(super) fn register_foreigns(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for foreign in module.foreigns() {
        let qualified = qualified_name(module.module_key(), &foreign.name);
        let name_id = state.artifact.intern_string(qualified.as_ref());
        let abi_id = state.artifact.intern_string(&foreign.abi);
        let symbol_id = state.artifact.intern_string(&foreign.symbol);
        let link_id = foreign
            .link
            .as_deref()
            .map(|link| state.artifact.intern_string(link));
        let param_tys = foreign
            .param_tys
            .iter()
            .map(|ty| ensure_type(state, layout, ty.as_ref()))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let result_ty = ensure_type(state, layout, foreign.result_ty.as_ref());
        let mut descriptor =
            ForeignDescriptor::new(name_id, param_tys, result_ty, abi_id, symbol_id)
                .with_export(foreign.exported)
                .with_hot(foreign.hot)
                .with_cold(foreign.cold);
        if let Some(link_id) = link_id {
            descriptor = descriptor.with_link(link_id);
        }
        let foreign_id = state.artifact.foreigns.alloc(descriptor);
        if let Some(binding) = foreign.binding {
            let _ = layout.foreigns.insert(binding, foreign_id);
        }
    }
}

pub(super) fn register_callables(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for callable in module.callables() {
        let name = qualified_name(module.module_key(), &callable.name);
        let params = u16::try_from(callable.params.len()).unwrap_or(u16::MAX);
        let procedure_id = alloc_procedure(
            &mut state.artifact,
            name.as_ref(),
            callable.exported,
            callable.hot,
            callable.cold,
            params,
        );
        let _ = layout
            .callables_by_name
            .insert(callable.name.clone(), procedure_id);
        if let Some(binding) = callable.binding {
            let _ = layout.callables.insert(binding, procedure_id);
        }
    }
}

pub(super) fn register_globals(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for global in module.globals() {
        let name = qualified_name(module.module_key(), &global.name);
        let init_name = format!("{name}::init");
        let init_procedure =
            alloc_procedure(&mut state.artifact, &init_name, false, false, false, 0);
        let name_id = state.artifact.intern_string(name.as_ref());
        let global_id = state.artifact.globals.alloc(
            GlobalDescriptor::new(name_id)
                .with_export(global.exported)
                .with_initializer(init_procedure),
        );
        let _ = layout
            .global_init_procedures
            .insert(global.name.clone(), init_procedure);
        if let Some(binding) = global.binding {
            let _ = layout.globals.insert(binding, global_id);
        }
    }
}
