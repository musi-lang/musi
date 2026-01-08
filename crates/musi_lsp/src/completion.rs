use std::collections::HashMap;

use musi_core::Symbol;
use tower_lsp_server::ls_types::{CompletionItem, CompletionItemKind};

use crate::bindings::{BindingInfo, BindingKind};

const KEYWORDS: &[&str] = &[
    "val", "var", "fn", "if", "else", "match", "case", "for", "while", "break", "cycle", "return",
    "record", "choice", "type", "import", "defer", "unsafe", "true", "false", "and", "or", "not",
    "as", "in", "is", "with", "export", "extern",
];

pub fn build_completions(
    bindings: &HashMap<Symbol, BindingInfo>,
    interner: &musi_core::Interner,
) -> Vec<CompletionItem> {
    let mut completions = build_keyword_completions();
    completions.extend(build_binding_completions(bindings, interner));
    completions
}

fn build_keyword_completions() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .map(|kw| CompletionItem {
            label: (*kw).to_owned(),
            kind: Some(CompletionItemKind::KEYWORD),
            detail: Some("keyword".to_owned()),
            ..Default::default()
        })
        .collect()
}

fn build_binding_completions(
    bindings: &HashMap<Symbol, BindingInfo>,
    interner: &musi_core::Interner,
) -> Vec<CompletionItem> {
    bindings
        .iter()
        .map(|(sym, info)| {
            let label = interner.resolve(sym.id).to_owned();
            let (kind, detail) = binding_kind_to_completion(info.kind);
            CompletionItem {
                label,
                kind: Some(kind),
                detail: Some(detail.to_owned()),
                ..Default::default()
            }
        })
        .collect()
}

const fn binding_kind_to_completion(kind: BindingKind) -> (CompletionItemKind, &'static str) {
    match kind {
        BindingKind::ValBinding => (CompletionItemKind::CONSTANT, "val (readonly)"),
        BindingKind::VarBinding => (CompletionItemKind::VARIABLE, "var (mutable)"),
        BindingKind::Function => (CompletionItemKind::FUNCTION, "function"),
        BindingKind::TypeDef => (CompletionItemKind::CLASS, "type"),
        BindingKind::Parameter { mutable } => {
            if mutable {
                (CompletionItemKind::VARIABLE, "parameter (mutable)")
            } else {
                (CompletionItemKind::CONSTANT, "parameter")
            }
        }
    }
}
