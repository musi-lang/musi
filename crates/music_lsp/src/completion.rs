//! Completion provider: keywords + sema-derived user symbols.

use std::collections::HashSet;

use music_sema::{DefKind, Type};
use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind};

use crate::analysis::AnalyzedDoc;
use crate::hover;

/// Produce completion items for the given document and optional trigger character.
pub fn complete(doc: &AnalyzedDoc, _trigger: Option<char>) -> Vec<CompletionItem> {
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
                DefKind::Type | DefKind::OpaqueType => CompletionItemKind::CLASS,
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
