//! Completion provider: keywords + sema-derived user symbols.

use std::collections::HashSet;

use lsp_types::{CompletionItem, CompletionItemKind};
use music_sema::{DefKind, Type};

use crate::analysis::{AnalyzedDoc, expr_span};
use crate::hover;

/// Produce completion items for the given document, optional trigger character, and cursor offset.
///
/// When triggered by `.`, attempts record-field completion for the expression
/// immediately before the dot. Falls back to global completions if the type cannot
/// be determined or is not a record.
pub fn complete(doc: &AnalyzedDoc, trigger: Option<char>, offset: u32) -> Vec<CompletionItem> {
    if trigger == Some('.')
        && let Some(items) = dot_completions(doc, offset)
    {
        return items;
    }

    global_completions(doc)
}

/// Attempt to produce field completions for the record-typed expression before `offset`.
///
/// Returns `None` if the expression type cannot be resolved to a record.
fn dot_completions(doc: &AnalyzedDoc, offset: u32) -> Option<Vec<CompletionItem>> {
    let sema = doc.sema.as_ref()?;

    // The dot was just typed at `offset - 1`; we want the expr that ends there.
    let dot_pos = offset.saturating_sub(1);

    // Find the smallest expr whose span contains `dot_pos` (end-exclusive).
    let expr_idx = sema
        .resolution
        .expr_defs
        .keys()
        .filter_map(|&idx| {
            let span = expr_span(idx, &doc.module)?;
            if span.start < dot_pos && dot_pos <= span.end() {
                Some((idx, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(idx, _)| idx)?;

    let raw_ty = *sema.expr_types.get(&expr_idx)?;
    let resolved_ty = sema.unify.resolve(raw_ty, &sema.types);

    match &sema.types[resolved_ty] {
        Type::Record { fields, .. } => {
            let items = fields
                .iter()
                .filter_map(|field| {
                    let name = doc.interner.try_resolve(field.name)?;
                    let detail = hover::fmt_type_lsp(field.ty, doc, sema);
                    Some(CompletionItem {
                        label: name.to_owned(),
                        kind: Some(CompletionItemKind::FIELD),
                        detail: Some(detail),
                        ..CompletionItem::default()
                    })
                })
                .collect();
            Some(items)
        }
        _ => None,
    }
}

fn global_completions(doc: &AnalyzedDoc) -> Vec<CompletionItem> {
    let mut items = keyword_completions();

    let mut seen: HashSet<String> = items.iter().map(|i| i.label.clone()).collect();

    if let Some(sema) = &doc.sema {
        for def in &sema.defs {
            let Some(name) = doc.interner.try_resolve(def.name) else {
                continue;
            };

            if name.starts_with('_') || !seen.insert(name.to_owned()) {
                continue;
            }

            if matches!(def.kind, DefKind::Import) {
                continue;
            }

            let kind = match def.kind {
                DefKind::Fn => CompletionItemKind::FUNCTION,
                DefKind::Let => {
                    let is_fn = def
                        .ty_info
                        .ty
                        .map(|t| matches!(&sema.types[t], Type::Fn { .. }))
                        .unwrap_or(false);
                    if is_fn {
                        CompletionItemKind::FUNCTION
                    } else {
                        CompletionItemKind::CONSTANT
                    }
                }
                DefKind::Var => CompletionItemKind::VARIABLE,
                DefKind::Param => CompletionItemKind::VARIABLE,
                DefKind::Type | DefKind::OpaqueType | DefKind::Primitive => CompletionItemKind::CLASS,
                DefKind::Variant => CompletionItemKind::ENUM_MEMBER,
                DefKind::Class => CompletionItemKind::INTERFACE,
                DefKind::Given => CompletionItemKind::MODULE,
                DefKind::Effect => CompletionItemKind::INTERFACE,
                DefKind::EffectOp | DefKind::ForeignFn => CompletionItemKind::FUNCTION,
                DefKind::Law => CompletionItemKind::PROPERTY,
                DefKind::LawVar => CompletionItemKind::VARIABLE,
                DefKind::Import => unreachable!(),
            };

            let detail = def.ty_info.ty.map(|ty| hover::fmt_type_lsp(ty, doc, sema));

            items.push(CompletionItem {
                label: name.to_owned(),
                kind: Some(kind),
                detail,
                ..CompletionItem::default()
            });
        }
    }

    items
}

fn keyword_completions() -> Vec<CompletionItem> {
    [
        ("let", "Immutable binding"),
        ("var", "Mutable binding"),
        ("if", "Conditional expression"),
        ("match", "Pattern match"),
        ("return", "Return from function"),
        ("import", "Import module"),
        ("export", "Export declaration"),
        ("as", "Type alias / rename"),
        ("class", "Type class definition"),
        ("given", "Type class instance"),
        ("effect", "Effect definition"),
        ("foreign", "Foreign function block"),
        ("forall", "Universal quantification"),
        ("exists", "Existential quantification"),
        ("where", "Constraint clause"),
        ("over", "Type class type parameter"),
        ("of", "Variant payload"),
        ("in", "Membership / iteration"),
        ("and", "Logical conjunction"),
        ("or", "Logical disjunction"),
        ("not", "Logical negation"),
        ("xor", "Logical exclusive or"),
        ("defer", "Deferred computation"),
        ("try", "Try / error propagation"),
        ("type", "Type definition"),
        ("ref", "Reference parameter mode"),
        ("inout", "In-out parameter mode"),
        ("law", "Type class law"),
    ]
    .iter()
    .map(|(kw, doc)| CompletionItem {
        label: (*kw).to_owned(),
        kind: Some(CompletionItemKind::KEYWORD),
        detail: Some((*doc).to_owned()),
        ..CompletionItem::default()
    })
    .collect()
}
