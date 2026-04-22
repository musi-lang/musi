use super::super::*;
use music_base::diag::DiagContext;

use crate::EmitDiagKind;

impl ProcedureEmitter<'_, '_> {
    pub(super) fn ensure_local_slot(&mut self, binding: NameBindingId) -> u16 {
        if let Some(slot) = self.locals.get(&binding).copied() {
            return slot;
        }
        let slot = Self::reserve_temp_slot(self);
        let _ = self.locals.insert(binding, slot);
        slot
    }

    pub(super) fn ensure_temp_slot(&mut self, temp: IrTempId) -> u16 {
        if let Some(slot) = self.temps.get(&temp).copied() {
            return slot;
        }
        let slot = Self::reserve_temp_slot(self);
        let _ = self.temps.insert(temp, slot);
        slot
    }

    pub(super) const fn reserve_temp_slot(emitter: ExprEmitterMut<'_, '_, '_>) -> u16 {
        let slot = emitter.next_local;
        emitter.next_local = emitter.next_local.saturating_add(1);
        slot
    }

    pub(super) const fn scratch_slot(emitter: ExprEmitterRef<'_, '_, '_>) -> u16 {
        emitter.next_local
    }

    pub(super) fn alloc_label(&mut self) -> u16 {
        let id = u16::try_from(self.labels.len()).unwrap_or(u16::MAX);
        let name = format!("L{id}");
        self.labels.push(self.artifact.intern_string(&name));
        id
    }
}

pub(super) fn push_expr_diag_with(
    diags: &mut EmitDiagList,
    module_key: &ModuleKey,
    origin: &IrOrigin,
    kind: EmitDiagKind,
    context: DiagContext,
) {
    push_span_diag_with(
        diags,
        module_key,
        origin.source_id,
        origin.span,
        kind,
        context,
    );
}

#[allow(clippy::needless_pass_by_value)]
pub(super) fn push_span_diag_with(
    diags: &mut EmitDiagList,
    _module_key: &ModuleKey,
    source_id: SourceId,
    span: Span,
    kind: EmitDiagKind,
    context: DiagContext,
) {
    diags.push(
        Diag::error(kind.message_with(&context))
            .with_code(kind.code())
            .with_label(span, source_id, kind.label_with(&context)),
    );
}
