use super::*;
use music_term::{TypeModuleRef, TypeTerm, TypeTermKind, parse_type_term};

pub(super) fn ensure_type(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    ty_name: &str,
) -> TypeId {
    if let Some(id) = layout.types.get(ty_name).copied() {
        return id;
    }
    if let Some(id) = state.types_by_name.get(ty_name).copied() {
        let _ = layout.types.insert(ty_name.into(), id);
        return id;
    }
    let name_id = state.artifact.intern_string(ty_name);
    let term = parse_type_term(ty_name).unwrap_or_else(|_| lower_named_term(ty_name, Box::new([])));
    let term_json = term.to_json();
    let term_id = state.artifact.intern_string(&term_json);
    let type_id = state
        .artifact
        .types
        .alloc(TypeDescriptor::new(name_id, term_id));
    let _ = state.types_by_name.insert(ty_name.into(), type_id);
    let _ = layout.types.insert(ty_name.into(), type_id);
    type_id
}

pub(super) fn ensure_effect(
    state: &mut ProgramState,
    effect: &IrEffectDef,
    layout: &mut ModuleLayout,
) -> EffectId {
    if let Some(id) = state.effects_by_key.get(&effect.key).copied() {
        return id;
    }
    let name = qualified_name(&effect.key.module, &effect.key.name);
    let name_id = state.artifact.intern_string(name.as_ref());
    let ops = effect
        .ops
        .iter()
        .map(|op| {
            EffectOpDescriptor::new(
                state.artifact.intern_string(op.name.as_ref()),
                op.param_tys
                    .iter()
                    .map(|ty| ensure_type(state, layout, ty.as_ref()))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
                ensure_type(state, layout, op.result_ty.as_ref()),
            )
            .with_comptime_safe(op.is_comptime_safe)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let id = state
        .artifact
        .effects
        .alloc(EffectDescriptor::new(name_id, ops));
    let _ = state.effects_by_key.insert(effect.key.clone(), id);
    id
}

fn lower_named_term(name: &str, args: Box<[TypeTerm]>) -> TypeTerm {
    let (module, local_name) = name
        .rsplit_once("::")
        .map_or((None, name), |(module, tail)| {
            (
                Some(TypeModuleRef {
                    spec: module.into(),
                }),
                tail,
            )
        });
    TypeTerm::new(TypeTermKind::Named {
        module,
        name: local_name.into(),
        args,
    })
}
