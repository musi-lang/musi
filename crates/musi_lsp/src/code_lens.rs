//! CodeLens provider: emits "▷ Run test" / "▷ Run" lenses above annotated fns.

use musi_parse::ast::{AttrArg, Expr, LitValue, Modifier};
use tower_lsp_server::ls_types::{CodeLens, Command, Uri};

use crate::analysis::{AnalyzedDoc, span_to_range};

/// Produce CodeLens items for `#[test("label")]` and `#[main]` top-level fns.
pub fn code_lens(doc: &AnalyzedDoc, uri: &Uri) -> Vec<CodeLens> {
    let mut lenses = Vec::new();
    let uri_str = uri.to_string();

    for &item_idx in doc.module.ctx.expr_lists.get_slice(doc.module.items) {
        let Expr::FnDef {
            attrs,
            modifiers,
            body,
            span,
            ..
        } = doc.module.ctx.exprs.get(item_idx)
        else {
            continue;
        };

        // Skip extrin / bodyless fns
        if body.is_none() || modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_))) {
            continue;
        }

        let range = span_to_range(doc.file_id, *span, &doc.source_db);

        for attr in attrs {
            let attr_name = doc.interner.resolve(attr.name);

            if attr_name == "test" {
                // #[test("label")] → "▷ Run test: <label>"
                let label = attr.args.first().and_then(|arg| match arg {
                    AttrArg::Value {
                        value: LitValue::Str(sym),
                        ..
                    } => Some(doc.interner.resolve(*sym).trim_matches('"').to_owned()),
                    _ => None,
                });
                let title = match label {
                    Some(ref l) => format!("▷ Run test: {l}"),
                    None => "▷ Run test".to_owned(),
                };
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title,
                        command: "musi.runTest".to_owned(),
                        arguments: Some(vec![
                            serde_json::Value::String(uri_str.clone()),
                            serde_json::Value::String(
                                label.unwrap_or_default(),
                            ),
                        ]),
                    }),
                    data: None,
                });
            } else if attr_name == "main" {
                // #[main] → "▷ Run"
                lenses.push(CodeLens {
                    range,
                    command: Some(Command {
                        title: "▷ Run".to_owned(),
                        command: "musi.runFile".to_owned(),
                        arguments: Some(vec![serde_json::Value::String(
                            uri_str.clone(),
                        )]),
                    }),
                    data: None,
                });
            }
        }
    }

    lenses
}
