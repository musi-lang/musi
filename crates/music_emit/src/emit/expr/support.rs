use super::super::*;
use crate::EmitDiagKind;

pub(super) fn ensure_local_slot(
    emitter: &mut MethodEmitter<'_, '_>,
    binding: NameBindingId,
) -> u16 {
    if let Some(slot) = emitter.locals.get(&binding).copied() {
        return slot;
    }
    let slot = reserve_temp_slot(emitter);
    let _ = emitter.locals.insert(binding, slot);
    slot
}

pub(super) fn ensure_temp_slot(emitter: &mut MethodEmitter<'_, '_>, temp: IrTempId) -> u16 {
    if let Some(slot) = emitter.temps.get(&temp).copied() {
        return slot;
    }
    let slot = reserve_temp_slot(emitter);
    let _ = emitter.temps.insert(temp, slot);
    slot
}

pub(super) const fn reserve_temp_slot(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let slot = emitter.next_local;
    emitter.next_local = emitter.next_local.saturating_add(1);
    slot
}

pub(super) const fn scratch_slot(emitter: &MethodEmitter<'_, '_>) -> u16 {
    emitter.next_local
}

pub(super) fn alloc_label(emitter: &mut MethodEmitter<'_, '_>) -> u16 {
    let id = u16::try_from(emitter.labels.len()).unwrap_or(u16::MAX);
    let name = format!("L{id}");
    emitter.labels.push(emitter.artifact.intern_string(&name));
    id
}

pub(super) fn push_expr_diag(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    origin: &IrOrigin,
    kind: &EmitDiagKind,
    label: impl Into<String>,
) {
    push_span_diag(
        diags,
        module_key,
        origin.source_id,
        origin.span,
        kind,
        label,
    );
}

pub(super) fn push_span_diag(
    diags: &mut EmitDiagList,
    _module_key: &ModuleKey,
    source_id: SourceId,
    span: Span,
    kind: &EmitDiagKind,
    label: impl Into<String>,
) {
    diags.push(
        Diag::error(kind.message())
            .with_code(kind.code())
            .with_label(span, source_id, label),
    );
}
