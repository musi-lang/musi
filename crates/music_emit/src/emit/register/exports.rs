use super::*;

pub(super) fn register_exports(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
) {
    for export in module.exports() {
        let name = qualified_name(module.module_key(), export.name.as_ref());
        let name_id = state.artifact.intern_string(name.as_ref());
        let target = export_target(
            state,
            module,
            layout,
            export.name.as_ref(),
            export.data_key.as_ref(),
            export.effect_key.as_ref(),
            export.shape_key.as_ref(),
        );

        let Some(target) = target else {
            let context = DiagContext::new().with("export", export.name.as_ref());
            state.diags.push(
                Diag::error(EmitDiagKind::MissingExportTarget.message_with(&context))
                    .with_code(EmitDiagKind::MissingExportTarget.code())
                    .with_note(EmitDiagKind::MissingExportTarget.label_with(&context)),
            );
            continue;
        };

        let _ = state
            .artifact
            .exports
            .alloc(ExportDescriptor::new(name_id, export.opaque, target));
    }
}

fn export_target(
    state: &mut ProgramState,
    module: &IrModule,
    layout: &mut ModuleLayout,
    export_name: &str,
    data_key: Option<&DefinitionKey>,
    effect_key: Option<&DefinitionKey>,
    shape_key: Option<&DefinitionKey>,
) -> Option<ExportTarget> {
    if let Some(binding) = export_binding(module, export_name) {
        if let Some(procedure) = layout.callables.get(&binding).copied() {
            return Some(ExportTarget::Procedure(procedure));
        }
        if let Some(global) = layout.globals.get(&binding).copied() {
            return Some(ExportTarget::Global(global));
        }
        if let Some(foreign) = layout.foreigns.get(&binding).copied() {
            return Some(ExportTarget::Foreign(foreign));
        }
    }
    if let Some(procedure) = layout.callables_by_name.get(export_name).copied() {
        return Some(ExportTarget::Procedure(procedure));
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

    shape_key
        .and_then(|key| layout.shapes.get(key).copied())
        .map(ExportTarget::Shape)
}

pub(in crate::emit) fn export_binding(
    module: &IrModule,
    export_name: &str,
) -> Option<NameBindingId> {
    module
        .callables()
        .iter()
        .find(|callable| callable.name.as_ref() == export_name)
        .and_then(|callable| callable.binding)
        .or_else(|| {
            module
                .globals()
                .iter()
                .find(|global| global.name.as_ref() == export_name)
                .and_then(|global| global.binding)
        })
        .or_else(|| {
            module
                .foreigns()
                .iter()
                .find(|foreign| foreign.name.as_ref() == export_name)
                .and_then(|foreign| foreign.binding)
        })
}
