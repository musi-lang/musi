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
        let target = export_target(
            state,
            module,
            layout,
            export.name.as_ref(),
            export.data_key.as_ref(),
            export.effect_key.as_ref(),
            export.class_key.as_ref(),
        );

        let Some(target) = target else {
            state.diags.push(Diag::error("export target missing"));
            continue;
        };

        let _ = state.artifact.exports.alloc(ExportDescriptor {
            name: name_id,
            opaque: export.opaque,
            target,
        });
    }
}

fn export_target(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
    export_name: &str,
    data_key: Option<&DefinitionKey>,
    effect_key: Option<&DefinitionKey>,
    class_key: Option<&DefinitionKey>,
) -> Option<ExportTarget> {
    if let Some(binding) = export_binding(module, export_name) {
        if let Some(method) = layout.callables.get(&binding).copied() {
            return Some(ExportTarget::Method(method));
        }
        if let Some(global) = layout.globals.get(&binding).copied() {
            return Some(ExportTarget::Global(global));
        }
        if let Some(foreign) = layout.foreigns.get(&binding).copied() {
            return Some(ExportTarget::Foreign(foreign));
        }
    }

    if let Some(key) = data_key {
        let ty_name = qualified_name(&key.module, key.name.as_ref());
        return Some(ExportTarget::Type(ensure_type(
            state,
            layout,
            ty_name.as_ref(),
        )));
    }

    if let Some(effect) = effect_key.and_then(|key| layout.effects.get(key).copied()) {
        return Some(ExportTarget::Effect(effect));
    }

    class_key
        .and_then(|key| layout.classes.get(key).copied())
        .map(ExportTarget::Class)
}

pub(super) fn export_binding(module: &IrModule, export_name: &str) -> Option<NameBindingId> {
    module
        .callables
        .iter()
        .find(|callable| callable.name.as_ref() == export_name)
        .and_then(|callable| callable.binding)
        .or_else(|| {
            module
                .globals
                .iter()
                .find(|global| global.name.as_ref() == export_name)
                .and_then(|global| global.binding)
        })
        .or_else(|| {
            module
                .foreigns
                .iter()
                .find(|foreign| foreign.name.as_ref() == export_name)
                .and_then(|foreign| foreign.binding)
        })
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
        let _ = state.artifact.meta.alloc(MetaDescriptor {
            target,
            key,
            values,
        });
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
        let params = u16::try_from(foreign.param_count).unwrap_or(u16::MAX);
        let foreign_id = state.artifact.foreigns.alloc(ForeignDescriptor {
            name: name_id,
            params,
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
        let params = u16::try_from(callable.params.len()).unwrap_or(u16::MAX);
        let method_id = alloc_method(
            &mut state.artifact,
            name.as_ref(),
            callable.exported,
            params,
        );
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
        let init_method = alloc_method(&mut state.artifact, &init_name, false, 0);
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
        | IrExprKind::SyntaxValue { .. }
        | IrExprKind::Unsupported { .. } => {}
        IrExprKind::Sequence { exprs } => collect_expr_types_iter(state, layout, exprs),
        IrExprKind::Tuple { ty_name, items } | IrExprKind::Array { ty_name, items } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_iter(state, layout, items);
        }
        IrExprKind::ArrayCat { ty_name, parts } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_seq_parts(state, layout, parts);
        }
        IrExprKind::Record {
            ty_name, fields, ..
        } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types_iter(state, layout, fields.iter().map(|field| &field.expr));
        }
        IrExprKind::Let { value, .. } | IrExprKind::TempLet { value, .. } => {
            collect_expr_types(state, layout, value);
        }
        IrExprKind::Assign { target, value } => {
            collect_assign_target_types(state, layout, target);
            collect_expr_types(state, layout, value);
        }
        IrExprKind::Index { base, indices } => {
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, indices.iter());
        }
        IrExprKind::DynamicImport { spec } => collect_expr_types(state, layout, spec),
        IrExprKind::RecordGet { base, .. } => collect_expr_types(state, layout, base),
        IrExprKind::RecordUpdate {
            ty_name,
            base,
            updates,
            ..
        } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, updates.iter().map(|update| &update.expr));
        }
        IrExprKind::ClosureNew { captures, .. } => collect_expr_types_iter(state, layout, captures),
        IrExprKind::Binary { left, right, .. } => {
            collect_expr_types(state, layout, left);
            collect_expr_types(state, layout, right);
        }
        IrExprKind::Not { expr } => collect_expr_types(state, layout, expr),
        IrExprKind::TyTest { base, ty_name } | IrExprKind::TyCast { base, ty_name } => {
            let _ = ensure_type(state, layout, ty_name);
            collect_expr_types(state, layout, base);
        }
        IrExprKind::Case { scrutinee, arms } => collect_case_expr_types(state, layout, scrutinee, arms),
        IrExprKind::Call { callee, args } => collect_call_expr_types(state, layout, callee, args),
        IrExprKind::CallSeq { callee, args } => {
            collect_call_seq_expr_types(state, layout, callee, args);
        }
        IrExprKind::VariantNew { data_key, args, .. } => {
            let name = qualified_name(&data_key.module, &data_key.name);
            let _ = ensure_type(state, layout, name.as_ref());
            collect_expr_types_iter(state, layout, args);
        }
        IrExprKind::Perform { args, .. } => collect_expr_types_iter(state, layout, args),
        IrExprKind::PerformSeq { args, .. } => {
            let _ = ensure_type(state, layout, "[]Any");
            collect_expr_types_seq_parts(state, layout, args);
        }
        IrExprKind::Handle {
            effect_key,
            value,
            ops,
            body,
            ..
        } => collect_handle_expr_types(state, layout, effect_key, value, ops, body),
        IrExprKind::Resume { expr } => {
            if let Some(expr) = expr.as_deref() {
                collect_expr_types(state, layout, expr);
            }
        }
    }
}

fn collect_expr_types_seq_parts(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    parts: &[IrSeqPart],
) {
    collect_expr_types_iter(
        state,
        layout,
        parts.iter().map(|part| match part {
            IrSeqPart::Expr(expr) | IrSeqPart::Spread(expr) => expr,
        }),
    );
}

fn collect_case_expr_types(
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

fn collect_call_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    callee: &IrExpr,
    args: &[IrArg],
) {
    collect_expr_types(state, layout, callee);
    collect_expr_types_iter(state, layout, args.iter().map(|arg| &arg.expr));
}

fn collect_call_seq_expr_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    callee: &IrExpr,
    args: &[IrSeqPart],
) {
    let _ = ensure_type(state, layout, "[]Any");
    collect_expr_types(state, layout, callee);
    collect_expr_types_seq_parts(state, layout, args);
}

fn collect_handle_expr_types(
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
    collect_expr_types_iter(state, layout, ops.iter().map(|op| &op.closure));
    collect_expr_types(state, layout, body);
}

fn collect_expr_types_iter<'a, I>(state: &mut ProgramState, layout: &mut ModuleLayout, exprs: I)
where
    I: IntoIterator<Item = &'a IrExpr>,
{
    for expr in exprs {
        collect_expr_types(state, layout, expr);
    }
}

fn collect_assign_target_types(
    state: &mut ProgramState,
    layout: &mut ModuleLayout,
    target: &IrAssignTarget,
) {
    match target {
        IrAssignTarget::Index { base, indices } => {
            collect_expr_types(state, layout, base);
            collect_expr_types_iter(state, layout, indices.iter());
        }
        IrAssignTarget::RecordField { base, .. } => collect_expr_types(state, layout, base),
        IrAssignTarget::Binding { .. } => {}
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
            name: state.artifact.intern_string(op.name.as_ref()),
            params: op.params,
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
