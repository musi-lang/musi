//! Completion provider: keywords + sema-derived user symbols.

use std::collections::HashSet;

use musi_sema::{DefKind, Type};
use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind};

use crate::analysis::AnalyzedDoc;
use crate::hover;

/// Produce completion items for the given document, position, and optional trigger character.
pub fn complete(doc: &AnalyzedDoc, _trigger: Option<char>) -> Vec<CompletionItem> {
    let mut items = keyword_completions();

    // Track which names we have already emitted to avoid duplicates (e.g. the same
    // prelude symbol imported under two paths ends up in sema.defs twice).
    let mut seen: HashSet<String> = items.iter().map(|i| i.label.clone()).collect();

    if let Some(sema) = &doc.sema {
        for def in &sema.defs {
            let name = doc.interner.resolve(def.name);

            // Skip underscore-prefixed names (conventionally private), and duplicates.
            if name.starts_with('_') || !seen.insert(name.to_owned()) {
                continue;
            }

            // Skip namespaces in general completions.
            if matches!(def.kind, DefKind::Namespace) {
                continue;
            }

            let kind = match def.kind {
                DefKind::Fn => CompletionItemKind::FUNCTION,
                DefKind::Const => {
                    let is_fn = def
                        .ty
                        .as_ref()
                        .map(|t| matches!(sema.unify_table.resolve(t.clone()), Type::Arrow(..)))
                        .unwrap_or(false);
                    if is_fn {
                        CompletionItemKind::FUNCTION
                    } else {
                        CompletionItemKind::CONSTANT
                    }
                }
                DefKind::Var => CompletionItemKind::VARIABLE,
                DefKind::Param => CompletionItemKind::VARIABLE,
                DefKind::Type => CompletionItemKind::CLASS,
                DefKind::Variant => CompletionItemKind::ENUM_MEMBER,
                DefKind::Class => CompletionItemKind::INTERFACE,
                DefKind::Given => CompletionItemKind::MODULE,
                DefKind::Namespace => unreachable!(),
            };

            let detail = def
                .ty
                .as_ref()
                .map(|ty| hover::fmt_type(&sema.unify_table.resolve(ty.clone()), doc, sema));

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
        ("fn", "Function definition"),
        ("const", "Immutable binding"),
        ("var", "Mutable binding"),
        ("if", "Conditional expression"),
        ("then", "Then-branch of if"),
        ("else", "Else-branch of if"),
        ("match", "Pattern match"),
        ("with", "Match arms delimiter"),
        ("for", "For loop"),
        ("while", "While loop"),
        ("loop", "Loop body keyword"),
        ("import", "Import symbols from a module"),
        ("export", "Export declaration"),
        ("from", "Import path (used after import { } from)"),
        ("record", "Record / struct type"),
        ("choice", "Choice / enum type"),
        ("class", "Type class definition"),
        ("given", "Type class instance"),
        ("extrin", "Extrinsic (external) function"),
        ("opaque", "Opaque type wrapper"),
        ("and", "Logical conjunction (short-circuit)"),
        ("or", "Logical disjunction (short-circuit)"),
        ("not", "Logical negation"),
        ("in", "For-in loop keyword"),
        ("case", "if-case / while-case destructuring"),
        ("satisfies", "Type class superclass constraint"),
        ("cycle", "Skip to next loop iteration (continue)"),
        ("break", "Exit loop early"),
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
