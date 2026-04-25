use music_base::Span;
use music_base::diag::{Diag, DiagContext};

use crate::checker::DiagKind;

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn diag_builder(&self, span: Span, kind: DiagKind, label: &str) -> Diag {
        let label = if label.is_empty() {
            kind.label()
        } else {
            label
        };
        let mut diag = Diag::error(kind.message())
            .with_code(kind.code())
            .with_label(span, self.source_id(), label);
        if let Some(hint) = kind.hint() {
            diag = diag.with_hint(hint);
        }
        diag
    }

    pub fn diag_with_builder(&self, span: Span, kind: DiagKind, context: &DiagContext) -> Diag {
        let mut diag = Diag::error(kind.message_with(context))
            .with_code(kind.code())
            .with_label(span, self.source_id(), kind.label_with(context));
        if let Some(hint) = kind.hint() {
            diag = diag.with_hint(context.render(hint));
        }
        diag
    }

    pub fn push_diag(&mut self, diag: Diag) {
        self.facts.diags.push(diag);
    }

    pub fn diag(&mut self, span: Span, kind: DiagKind, label: &str) {
        self.push_diag(self.diag_builder(span, kind, label));
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn diag_with(&mut self, span: Span, kind: DiagKind, context: DiagContext) {
        self.push_diag(self.diag_with_builder(span, kind, &context));
    }

    #[allow(clippy::needless_pass_by_value)]
    pub fn diag_with_previous(
        &mut self,
        span: Span,
        previous_span: Span,
        kind: DiagKind,
        context: DiagContext,
    ) {
        let mut diag = self.diag_with_builder(span, kind, &context);
        if let Some(previous) = kind.secondary_with(&context) {
            diag = diag.with_label(previous_span, self.source_id(), previous);
        }
        self.push_diag(diag);
    }

    pub fn fresh_open_row_name(&mut self, base: &str) -> Box<str> {
        let next = self.typing.next_open_row_id;
        self.typing.next_open_row_id = self.typing.next_open_row_id.saturating_add(1);
        format!("{base}#{next}").into_boxed_str()
    }
}
