//! Custom `musi/discoverTests` LSP request: returns structured test item data
//! for the VS Code Test Explorer. Replaces CodeLens for test discovery.

use std::ops::ControlFlow;

use lsp_types::{Range, Url};
use msc_ast::expr::{Arg, FieldKey};
use msc_ast::visitor::{AstVisitor, walk_expr};
use msc_ast::{AstArenas, Expr, ExprIdx, Lit};
use msc_shared::{Interner, Span};
use serde::{Deserialize, Serialize};

use crate::analysis::AnalyzedDoc;
use crate::to_proto::span_to_range;

// ── Wire types ────────────────────────────────────────────────────────────────

#[derive(Debug, Serialize, Deserialize)]
pub struct DiscoverTestsParams {
    pub uri: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TestItemData {
    pub id: String,
    pub label: String,
    /// `"suite"` for `describe`, `"test"` for `it`
    pub kind: String,
    pub range: Range,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct DiscoverTestsResult {
    pub tests: Vec<TestItemData>,
}

// ── Custom LSP request type ───────────────────────────────────────────────────

pub enum DiscoverTests {}

impl lsp_types::request::Request for DiscoverTests {
    type Params = DiscoverTestsParams;
    type Result = DiscoverTestsResult;
    const METHOD: &'static str = "musi/discoverTests";
}

// ── AST visitor ──────────────────────────────────────────────────────────────

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

// ── Public handler ────────────────────────────────────────────────────────────

/// Build a `DiscoverTestsResult` from an analyzed document.
///
/// Non-test files return an empty list. `describe` calls map to kind `"suite"`,
/// `it` calls map to kind `"test"`. Each item's `id` is `"<uri>::<label>"`.
pub fn discover_tests(doc: &AnalyzedDoc, uri: &Url) -> DiscoverTestsResult {
    let uri_str = uri.as_str();

    if !uri_str.ends_with(".test.ms") {
        return DiscoverTestsResult { tests: vec![] };
    }

    let mut collector = TestCallCollector {
        interner: &doc.interner,
        calls: vec![],
    };
    for stmt in &doc.module.stmts {
        let _ = collector.visit_expr(stmt.expr, &doc.module.arenas);
    }

    let tests = collector
        .calls
        .into_iter()
        .map(|call| {
            let range = span_to_range(doc.file_id, call.span, &doc.source_db);
            let kind = if call.kind == "describe" {
                "suite".to_owned()
            } else {
                "test".to_owned()
            };
            let id = format!("{}::{}", uri_str, call.label);
            TestItemData {
                id,
                label: call.label,
                kind,
                range,
            }
        })
        .collect();

    DiscoverTestsResult { tests }
}
