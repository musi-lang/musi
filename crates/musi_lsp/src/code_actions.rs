//! Code actions: offer quick fixes and refactors at the cursor position.
//!
//! Currently implements:
//! - "Add type annotation" for `const`/`var` bindings that lack an explicit `: Type`.

use musi_parse::ast::{BindKind, Expr};
use musi_sema::Type;
use tower_lsp_server::ls_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, Range, TextEdit, Uri,
    WorkspaceEdit,
};

use crate::analysis::{AnalyzedDoc, find_name_token, offset_to_position, position_to_offset};
use crate::hover::fmt_type;

/// Compute code actions for the given cursor range.
pub fn code_actions(
    doc: &AnalyzedDoc,
    params: &CodeActionParams,
    uri: &Uri,
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

    // Walk all top-level and nested Bind expressions looking for const/var bindings
    // at the cursor that have no explicit type annotation.
    for expr in doc.module.ctx.exprs.iter() {
        let Expr::Bind {
            kind,
            pat,
            ty,
            span,
            ..
        } = expr
        else {
            continue;
        };

        // Only consider bindings that contain the cursor.
        if cursor_offset < span.start || cursor_offset > span.start + span.length {
            continue;
        }

        // Skip bindings that already have an explicit type annotation.
        if ty.is_some() {
            continue;
        }

        let def_kind = match kind {
            BindKind::Const => musi_sema::DefKind::Const,
            BindKind::Var => musi_sema::DefKind::Var,
        };

        // Find the def for this binding's pattern span.
        let pat_span = pat.span();
        let def = sema.pat_defs.iter().find_map(|(s, &def_id)| {
            if s.start == pat_span.start {
                sema.defs.get(def_id.0 as usize)
            } else {
                None
            }
        });

        let Some(def) = def else { continue };
        if def.kind != def_kind {
            continue;
        }

        let Some(ty_val) = &def.ty else { continue };
        let resolved = sema.unify_table.resolve(ty_val.clone());

        // Skip unknown / error types — nothing useful to insert.
        if matches!(resolved, Type::Var(_) | Type::Error) {
            continue;
        }

        let ty_str = fmt_type(&resolved, doc, sema);

        // Insertion point: immediately after the name token.
        let name_span =
            find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(pat_span);
        let insert_offset = name_span.start + name_span.length;
        let insert_position = offset_to_position(doc.file_id, insert_offset, &doc.source_db);

        let text_edit = TextEdit {
            range: Range {
                start: insert_position,
                end: insert_position,
            },
            new_text: format!(": {ty_str}"),
        };

        let mut changes = std::collections::HashMap::new();
        changes.insert(uri.clone(), vec![text_edit]);

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
        // Stop after the first (innermost) matching binding.
        break;
    }

    actions
}

/// Extension trait to extract a span from a `Pat`.
trait PatSpanExt {
    fn span(&self) -> musi_shared::Span;
}

impl PatSpanExt for musi_parse::ast::Pat {
    fn span(&self) -> musi_shared::Span {
        use musi_parse::ast::Pat;
        match self {
            Pat::Ident { span, .. }
            | Pat::DotPrefix { span, .. }
            | Pat::Lit { span, .. }
            | Pat::Wild { span }
            | Pat::Prod { span, .. }
            | Pat::Arr { span, .. }
            | Pat::AnonRec { span, .. }
            | Pat::Or { span, .. }
            | Pat::Error { span } => *span,
        }
    }
}
