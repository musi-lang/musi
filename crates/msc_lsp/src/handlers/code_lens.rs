//! CodeLens provider: emits "Run" / "Debug" at file level, per-`#[test]` lenses,
//! and per-`describe`/`it` lenses for vitest-style test files.

use std::ops::ControlFlow;

use lsp_types::{CodeLens, Command, Position, Range, Url};
use msc_ast::expr::{Arg, FieldKey};
use msc_ast::visitor::{walk_expr, AstVisitor};
use msc_ast::{AstArenas, Expr, ExprIdx, Lit};
use msc_shared::{Interner, Span};

use crate::analysis::AnalyzedDoc;
use crate::to_proto::span_to_range;

/// Collected `describe` / `it` call site.
struct TestCall {
    span: Span,
    label: String,
    kind: &'static str,
}

struct TestCallCollector<'a> {
    interner: &'a Interner,
    calls: Vec<TestCall>,
}

impl AstVisitor for TestCallCollector<'_> {
    type Break = ();

    fn visit_expr(&mut self, idx: ExprIdx, ctx: &AstArenas) -> ControlFlow<()> {
        if let Expr::Call {
            callee, args, span, ..
        } = &ctx.exprs[idx]
        {
            if let Expr::Field {
                field: FieldKey::Name { name, .. },
                ..
            } = &ctx.exprs[*callee]
            {
                let field_name = self.interner.try_resolve(*name).unwrap_or("");
                let kind: Option<&'static str> = match field_name {
                    "describe" => Some("describe"),
                    "it" => Some("it"),
                    _ => None,
                };
                if let Some(kind) = kind {
                    if let Some(Arg::Pos { expr, .. }) = args.first() {
                        if let Expr::Lit {
                            lit: Lit::Str { value, .. },
                            ..
                        } = &ctx.exprs[*expr]
                        {
                            let label = self
                                .interner
                                .try_resolve(*value)
                                .unwrap_or("")
                                .trim_matches('"')
                                .to_owned();
                            self.calls.push(TestCall {
                                span: *span,
                                label,
                                kind,
                            });
                        }
                    }
                }
            }
        }
        walk_expr(self, idx, ctx)
    }
}

/// Produce CodeLens items for the given document.
///
/// For all files: file-level Run/Debug lenses.
/// For `.test.ms` files: per-`describe`/`it` run lenses via AST walk.
/// For legacy `#[test]` bindings: per-test run lenses.
pub fn code_lens(doc: &AnalyzedDoc, uri: &Url) -> Vec<CodeLens> {
    let mut lenses = vec![];
    let uri_str = uri.to_string();
    let is_test_file = uri_str.ends_with(".test.ms");

    let file_range = Range::new(Position::new(0, 0), Position::new(0, 0));
    lenses.push(CodeLens {
        range: file_range,
        command: Some(Command {
            title: "▶ Run".to_owned(),
            command: "musi.runFile".to_owned(),
            arguments: Some(vec![serde_json::Value::String(uri_str.clone())]),
        }),
        data: None,
    });
    lenses.push(CodeLens {
        range: file_range,
        command: Some(Command {
            title: "⚙ Debug".to_owned(),
            command: "musi.debugTest".to_owned(),
            arguments: Some(vec![serde_json::Value::String(uri_str.clone())]),
        }),
        data: None,
    });

    // Vitest-style describe/it lenses for test files
    if is_test_file {
        let mut collector = TestCallCollector {
            interner: &doc.interner,
            calls: vec![],
        };
        for stmt in &doc.module.stmts {
            let _ = collector.visit_expr(stmt.expr, &doc.module.arenas);
        }
        for call in &collector.calls {
            let range = span_to_range(doc.file_id, call.span, &doc.source_db);
            let title = if call.kind == "describe" {
                format!("▶ Run suite: {}", call.label)
            } else {
                format!("▶ Run test: {}", call.label)
            };
            lenses.push(CodeLens {
                range,
                command: Some(Command {
                    title,
                    command: "musi.runTest".to_owned(),
                    arguments: Some(vec![
                        serde_json::Value::String(uri_str.clone()),
                        serde_json::Value::String(call.label.clone()),
                    ]),
                }),
                data: None,
            });
        }
    }

    lenses
}
