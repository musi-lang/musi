//! CodeLens provider: emits "Run test" / "Run" lenses above annotated fns.

use lsp_types::{CodeLens, Command, Url};
use music_ast::{Expr, Lit, attr::AttrValue};

use crate::analysis::{AnalyzedDoc, span_to_range};

/// Produce CodeLens items for `#[test]` and `#[entrypoint]` top-level bindings.
pub fn code_lens(doc: &AnalyzedDoc, uri: &Url) -> Vec<CodeLens> {
    let mut lenses = vec![];
    let uri_str = uri.to_string();

    for stmt in &doc.module.stmts {
        let expr = &doc.module.arenas.exprs[stmt.expr];

        let (attrs, span) = match expr {
            Expr::Annotated { attrs, inner, span } => {
                // Check if inner is a Binding
                let inner_expr = &doc.module.arenas.exprs[*inner];
                match inner_expr {
                    Expr::Binding { .. } => (attrs, *span),
                    _ => continue,
                }
            }
            _ => continue,
        };

        let range = span_to_range(doc.file_id, span, &doc.source_db);

        for attr in attrs {
            let Some(attr_name) = doc.interner.try_resolve(attr.name) else {
                continue;
            };

            if attr_name == "test" {
                let label = attr.value.as_ref().and_then(|v| match v {
                    AttrValue::Lit {
                        lit: Lit::Str { value, .. },
                        ..
                    } => doc
                        .interner
                        .try_resolve(*value)
                        .map(|s| s.trim_matches('"').to_owned()),
                    _ => None,
                });
                let title = match label {
                    Some(ref l) => format!("▶ Run test: {l}"),
                    None => "▶ Run test".to_owned(),
                };
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title,
                        command: "musi.runTest".to_owned(),
                        arguments: Some(vec![
                            serde_json::Value::String(uri_str.clone()),
                            serde_json::Value::String(label.unwrap_or_default()),
                        ]),
                    }),
                    data: None,
                });
            } else if attr_name == "entrypoint" {
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: "▶ Run".to_owned(),
                        command: "musi.runFile".to_owned(),
                        arguments: Some(vec![serde_json::Value::String(uri_str.clone())]),
                    }),
                    data: None,
                });
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: "⚙ Debug".to_owned(),
                        command: "musi.debugFile".to_owned(),
                        arguments: Some(vec![serde_json::Value::String(uri_str.clone())]),
                    }),
                    data: None,
                });
            }
        }
    }

    lenses
}
