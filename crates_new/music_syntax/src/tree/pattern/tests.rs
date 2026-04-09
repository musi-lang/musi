use crate::{Lexer, SyntaxNodeKind, canonical_name_text, parse};

use super::pattern_binder_tokens;

fn binders_in(text: &str) -> Vec<String> {
    let parsed = parse(Lexer::new(text).lex());
    assert!(parsed.errors().is_empty(), "{:?}", parsed.errors());
    let pattern = parsed
        .tree()
        .root()
        .child_nodes()
        .find(|node| node.kind() == SyntaxNodeKind::SequenceExpr)
        .and_then(|node| {
            node.child_nodes()
                .find(|child| child.kind() == SyntaxNodeKind::LetExpr)
        })
        .and_then(|node| node.child_nodes().find(|child| child.kind().is_pat()))
        .expect("pattern expected");

    pattern_binder_tokens(pattern)
        .into_iter()
        .filter_map(|token| {
            token
                .text()
                .map(|raw| canonical_name_text(token.kind(), raw).to_owned())
        })
        .collect()
}

#[test]
fn record_pattern_without_colon_yields_field_names() {
    assert_eq!(binders_in("let {x, y} = value;"), ["x", "y"]);
}

#[test]
fn record_pattern_with_colon_yields_inner_pattern_binders() {
    assert_eq!(binders_in("let {x: y, z: .Some(a)} = value;"), ["y", "a"]);
}

#[test]
fn as_and_or_patterns_preserve_source_order() {
    assert_eq!(
        binders_in("let (.Some(x) as y) or {mut z} = value;"),
        ["x", "y", "z"]
    );
}
