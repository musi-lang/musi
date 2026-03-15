//! Code actions: offer quick fixes and refactors at the cursor position.
//!
//! Currently implements:
//! - "Add type annotation" for `let`/`var` bindings that lack an explicit `: Type`.

use music_ast::Expr;
use music_sema::Type;
use lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, Range, TextEdit,
    Url, WorkspaceEdit,
};

use crate::analysis::{AnalyzedDoc, find_name_token, offset_to_position, position_to_offset};
use crate::hover::fmt_type_lsp;

/// Compute code actions for the given cursor range.
pub fn code_actions(
    doc: &AnalyzedDoc,
    params: &CodeActionParams,
    uri: &Url,
) -> Vec<CodeActionOrCommand> {
    let Some(sema) = &doc.sema else {
        return vec![];
    };

    let cursor_offset = position_to_offset(
        &doc.source,
        params.range.start.line,
        params.range.start.character,
    );

    let mut actions = Vec::new();

    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = music_shared::Idx::from_raw(u32::try_from(idx).unwrap_or(0));
        let Expr::Binding { fields, span, .. } = &doc.module.arenas.exprs[idx] else {
            continue;
        };

        if cursor_offset < span.start || cursor_offset > span.end() {
            continue;
        }

        if fields.ty.is_some() {
            continue;
        }

        let pat_span = doc.module.arenas.pats[fields.pat].span();
        let def = sema.resolution.pat_defs.iter().find_map(|(s, &def_id)| {
            if s.start == pat_span.start {
                sema.defs.get(def_id.0 as usize)
            } else {
                None
            }
        });

        let Some(def) = def else { continue };

        let Some(ty) = def.ty_info.ty else { continue };

        if matches!(&sema.types[ty], Type::Var(_) | Type::Error) {
            continue;
        }

        let ty_str = fmt_type_lsp(ty, doc, sema);

        let name_span =
            find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(pat_span);
        let insert_offset = name_span.end();
        let insert_position = offset_to_position(doc.file_id, insert_offset, &doc.source_db);

        let text_edit = TextEdit {
            range: Range {
                start: insert_position,
                end: insert_position,
            },
            new_text: format!(": {ty_str}"),
        };

        let mut changes = std::collections::HashMap::new();
        let _prev = changes.insert(uri.clone(), vec![text_edit]);

        let action = CodeAction {
            title: format!("Add type annotation: {ty_str}"),
            kind: Some(CodeActionKind::REFACTOR_REWRITE),
            edit: Some(WorkspaceEdit {
                changes: Some(changes),
                ..WorkspaceEdit::default()
            }),
            is_preferred: Some(true),
            ..CodeAction::default()
        };

        actions.push(CodeActionOrCommand::CodeAction(action));
        break;
    }

    actions
}

/// Extension trait to extract a span from a `Pat`.
trait PatSpanExt {
    fn span(&self) -> music_shared::Span;
}

impl PatSpanExt for music_ast::Pat {
    fn span(&self) -> music_shared::Span {
        use music_ast::Pat;
        match self {
            Pat::Wild { span, .. }
            | Pat::Lit { span, .. }
            | Pat::Bind { span, .. }
            | Pat::Variant { span, .. }
            | Pat::Record { span, .. }
            | Pat::Tuple { span, .. }
            | Pat::Array { span, .. }
            | Pat::Or { span, .. }
            | Pat::Error { span } => *span,
        }
    }
}
