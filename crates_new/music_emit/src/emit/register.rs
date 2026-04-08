use super::*;

pub(super) fn register_module(
    state: &mut ProgramState,
    module: &IrModule,
    _options: EmitOptions,
) -> ModuleLayout {
    let mut layout = ModuleLayout::default();
    register_types(state, module);
    register_data_defs(state, module, &mut layout);
    register_effects(state, module, &mut layout);
    register_classes(state, module, &mut layout);
    register_foreigns(state, module, &mut layout);
    register_callables(state, module, &mut layout);
    register_globals(state, module, &mut layout);
    register_exports(state, module, &mut layout);
    register_meta(state, module);
    register_expr_types(state, module, &mut layout);
    layout
}

fn register_types(state: &mut ProgramState, module: &IrModule) {
    let mut seen = BTreeSet::<Box<str>>::new();
    for index in 0..module.types.len() {
        let name: Box<str> = format!("{}::type::{index}", module.module_key.as_str()).into();
        if seen.insert(name.clone()) {
            let name_id = state.artifact.intern_string(name.as_ref());
            let _ = state.artifact.types.alloc(TypeDescriptor { name: name_id });
        }
    }
}

fn register_data_defs(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for data in &module.data_defs {
        let name = qualified_name(&data.key.module, &data.key.name);
        let _ty = ensure_type(state, layout, name.as_ref());
        let repr_kind = data
            .repr_kind
            .as_deref()
            .map(|kind| state.artifact.intern_string(kind));
        let name_id = state.artifact.intern_string(name.as_ref());
        let _ = state.artifact.data.alloc(DataDescriptor {
            name: name_id,
            variant_count: data.variant_count,
            field_count: data.field_count,
            repr_kind,
            layout_align: data.layout_align,
            layout_pack: data.layout_pack,
        });
    }
}

fn register_effects(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for effect in &module.effects {
        let effect_id = ensure_effect(state, effect);
        let _ = layout.effects.insert(effect.key.clone(), effect_id);
    }
}

fn register_classes(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for class in &module.classes {
        let name = qualified_name(&class.key.module, &class.key.name);
        let name_id = state.artifact.intern_string(name.as_ref());
        let id = state
            .artifact
            .classes
            .alloc(ClassDescriptor { name: name_id });
        let _ = layout.classes.insert(class.key.clone(), id);
    }
}

fn register_exports(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for export in &module.exports {
        let name = qualified_name(&module.module_key, export.name.as_ref());
        let name_id = state.artifact.intern_string(name.as_ref());
        let opaque = export.opaque;

        let target = module
            .callables
            .iter()
            .find(|callable| callable.name.as_ref() == export.name.as_ref())
            .and_then(|callable| callable.binding)
            .and_then(|binding| layout.callables.get(&binding).copied())
            .map(ExportTarget::Method)
            .or_else(|| {
                module
                    .globals
                    .iter()
                    .find(|global| global.name.as_ref() == export.name.as_ref())
                    .and_then(|global| global.binding)
                    .and_then(|binding| layout.globals.get(&binding).copied())
                    .map(ExportTarget::Global)
            })
            .or_else(|| {
                module
                    .foreigns
                    .iter()
                    .find(|foreign| foreign.name.as_ref() == export.name.as_ref())
                    .and_then(|foreign| foreign.binding)
                    .and_then(|binding| layout.foreigns.get(&binding).copied())
                    .map(ExportTarget::Foreign)
            })
            .or_else(|| {
                export.data_key.as_ref().map(|key| {
                    let ty_name = qualified_name(&key.module, key.name.as_ref());
                    ExportTarget::Type(ensure_type(state, layout, ty_name.as_ref()))
                })
            })
            .or_else(|| {
                export
                    .effect_key
                    .as_ref()
                    .and_then(|key| layout.effects.get(key).copied())
                    .map(ExportTarget::Effect)
            })
            .or_else(|| {
                export
                    .class_key
                    .as_ref()
                    .and_then(|key| layout.classes.get(key).copied())
                    .map(ExportTarget::Class)
            });

        let Some(target) = target else {
            state.diags.push(Diag::error("export target missing"));
            continue;
        };

        let _ = state.artifact.exports.alloc(ExportDescriptor {
            name: name_id,
            opaque,
            target,
        });
    }
}

fn register_meta(state: &mut ProgramState, module: &IrModule) {
    for record in &module.meta {
        let target = state.artifact.intern_string(record.target.as_ref());
        let key = state.artifact.intern_string(record.key.as_ref());
        let values = record
            .values
            .iter()
            .map(|value| state.artifact.intern_string(value.as_ref()))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let _ = state.artifact.meta.alloc(MetaDescriptor { target, key, values });
    }
}

fn register_foreigns(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for foreign in &module.foreigns {
        let qualified = qualified_name(&module.module_key, &foreign.name);
        let name_id = state.artifact.intern_string(qualified.as_ref());
        let abi_id = state.artifact.intern_string(&foreign.abi);
        let symbol_id = state.artifact.intern_string(&foreign.symbol);
        let link_id = foreign
            .link
            .as_deref()
            .map(|link| state.artifact.intern_string(link));
        let foreign_id = state.artifact.foreigns.alloc(ForeignDescriptor {
            name: name_id,
            abi: abi_id,
            symbol: symbol_id,
            link: link_id,
            export: foreign.exported,
        });
        if let Some(binding) = foreign.binding {
            let _ = layout.foreigns.insert(binding, foreign_id);
        }
    }
}

fn register_callables(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for callable in &module.callables {
        let name = qualified_name(&module.module_key, &callable.name);
        let method_id = alloc_method(&mut state.artifact, name.as_ref(), callable.exported);
        let _ = layout
            .callables_by_name
            .insert(callable.name.clone(), method_id);
        if let Some(binding) = callable.binding {
            let _ = layout.callables.insert(binding, method_id);
        }
    }
}

fn register_globals(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for global in &module.globals {
        let name = qualified_name(&module.module_key, &global.name);
        let init_name = format!("{name}::init");
        let init_method = alloc_method(&mut state.artifact, &init_name, false);
        let name_id = state.artifact.intern_string(name.as_ref());
        let global_id = state.artifact.globals.alloc(GlobalDescriptor {
            name: name_id,
            export: global.exported,
            initializer: Some(init_method),
        });
        layout.init_methods.push(init_method);
        if let Some(binding) = global.binding {
            let _ = layout.globals.insert(binding, global_id);
        }
    }
}

fn register_expr_types(state: &mut ProgramState, module: &IrModule, layout: &mut ModuleLayout) {
    for callable in &module.callables {
        collect_expr_types(state, layout, &callable.body);
    }
    for global in &module.globals {
        collect_expr_types(state, layout, &global.body);
    }
}

fn collect_expr_types(state: &mut ProgramState, layout: &mut ModuleLayout, expr: &IrExpr) {
    match &expr.kind {
        IrExprKind::Unit
        | IrExprKind::Name { .. }
        | IrExprKind::Temp { .. }
        | IrExprKind::Lit(_)
        | IrExprKind::Unsupported { .. } => {}
        IrExprKind::Sequence { exprs } => collect_expr_types_slice(state, layout, exprs),
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_slice(state, layout, items);
        }
        IrExprKind::Record { ty_name, fields, .. } => {
            collect_expr_types_record_fields(state, layout, ty_name, fields);
        }
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_expr_types(state, layout, value);
        }
        IrExprKind::Assign { target, value } => {
            collect_assign_target_types(state, layout, target);
            collect_expr_types(state, layout, value);
        }
        IrExprKind::Index { base, index } => {
            collect_expr_types(state, layout, base);
            collect_expr_types(state, layout, index);
        }
        IrExprKind::RecordGet { base, .. } => collect_expr_types(state, layout, base),
        IrExprKind::RecordUpdate {
            ty_name,
            base,
            updates,
            ..
        } => collect_expr_types_record_update(state, layout, ty_name, base, updates),
        IrExprKind::ClosureNew { captures, .. } => collect_expr_types_slice(state, layout, captures),
        IrExprKind::Binary { left, right, .. } => {
            collect_expr_types(state, layout, left);
            collect_expr_types(state, layout, right);
        }
        IrExprKind::Case { scrutinee, arms } => collect_expr_types_case(state, layout, scrutinee, arms),
        IrExprKind::Call { callee, args } => collect_expr_types_call(state, layout, callee, args),
        IrExprKind::VariantNew { data_key, args, .. } => {
            collect_expr_types_variant_new(state, layout, data_key, args);
        }
        IrExprKind::Perform { args, .. } => collect_expr_types_slice(state, layout, args),
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
            ..
        } => collect_expr_types_handle(state, layout, effect_key, value, ops, body),
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr.as_deref() {
                collect_expr_types(state, layout, expr);
            }
        }
    }
}

fn collect_expr_types_slice(state: &mut ProgramState, layout: &mut ModuleLayout, exprs: &[IrExpr]) {
    for expr in exprs {
        collect_expr_types(state, layout, expr);
    }
}

fn collect_expr_types_record_fields(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    ty_name: &str,
    fields: &[IrRecordField],
) {
    let _ = ensure_type(state, layout, ty_name);
    for field in fields {
        collect_expr_types(state, layout, &field.expr);
    }
}

fn collect_expr_types_record_update(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    ty_name: &str,
    base: &IrExpr,
    updates: &[IrRecordField],
) {
    let _ = ensure_type(state, layout, ty_name);
    collect_expr_types(state, layout, base);
    for update in updates {
        collect_expr_types(state, layout, &update.expr);
    }
}

fn collect_expr_types_case(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    scrutinee: &IrExpr,
    arms: &[IrCaseArm],
) {
    collect_expr_types(state, layout, scrutinee);
    for arm in arms {
        if let Some(guard) = &arm.guard {
            collect_expr_types(state, layout, guard);
        }
        collect_expr_types(state, layout, &arm.expr);
    }
}

fn collect_expr_types_call(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    callee: &IrExpr,
    args: &[IrArg],
) {
    collect_expr_types(state, layout, callee);
    for arg in args {
        collect_expr_types(state, layout, &arg.expr);
    }
}

fn collect_expr_types_variant_new(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    data_key: &DefinitionKey,
    args: &[IrExpr],
) {
    let name = qualified_name(&data_key.module, &data_key.name);
    let _ = ensure_type(state, layout, name.as_ref());
    collect_expr_types_slice(state, layout, args);
}

fn collect_expr_types_handle(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    effect_key: &DefinitionKey,
    value: &IrExpr,
    ops: &[IrHandleOp],
    body: &IrExpr,
) {
    let handler_ty = handler_type_name(effect_key);
    let _ = ensure_type(state, layout, handler_ty.as_ref());
    collect_expr_types(state, layout, value);
    for op in ops {
        collect_expr_types(state, layout, &op.closure);
    }
    collect_expr_types(state, layout, body);
}

fn collect_assign_target_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    target: &IrAssignTarget,
) {
    if let IrAssignTarget::Index { base, index } = target {
        collect_expr_types(state, layout, base);
        collect_expr_types(state, layout, index);
    }
}

fn ensure_type(state: &mut ProgramState, layout: &mut ModuleLayout, ty_name: &str) -> TypeId {
    if let Some(id) = layout.types.get(ty_name).copied() {
        return id;
    }
    if let Some(id) = state.types_by_name.get(ty_name).copied() {
        let _ = layout.types.insert(ty_name.into(), id);
        return id;
    }
    let name_id = state.artifact.intern_string(ty_name);
    let type_id = state.artifact.types.alloc(TypeDescriptor { name: name_id });
    let _ = state.types_by_name.insert(ty_name.into(), type_id);
    let _ = layout.types.insert(ty_name.into(), type_id);
    type_id
}

fn ensure_effect(state: &mut ProgramState, effect: &IrEffectDef) -> EffectId {
    if let Some(id) = state.effects_by_key.get(&effect.key).copied() {
        return id;
    }
    let name = qualified_name(&effect.key.module, &effect.key.name);
    let name_id = state.artifact.intern_string(name.as_ref());
    let ops = effect
        .ops
        .iter()
        .map(|op| EffectOpDescriptor {
            name: state.artifact.intern_string(op),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let id = state
        .artifact
        .effects
        .alloc(EffectDescriptor { name: name_id, ops });
    let _ = state.effects_by_key.insert(effect.key.clone(), id);
    id
}
