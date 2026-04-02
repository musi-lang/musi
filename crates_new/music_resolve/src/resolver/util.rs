use super::*;

pub(super) fn stmt_inner_expr<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> Option<SyntaxNode<'tree, 'src>> {
    if node.kind() != music_syntax::SyntaxNodeKind::SequenceExpr {
        return None;
    }
    let mut children = node.children();
    match (children.next(), children.next(), children.next()) {
        (Some(music_syntax::SyntaxElement::Node(expr)), Some(music_syntax::SyntaxElement::Token(tok)), None)
            if tok.kind() == music_syntax::TokenKind::Semicolon =>
        {
            Some(expr)
        }
        _ => None,
    }
}

pub(super) fn parse_u32_lit(raw: &str) -> Option<u32> {
    let raw = raw.replace('_', "");
    let (radix, digits) = raw
        .strip_prefix("0x")
        .or_else(|| raw.strip_prefix("0X"))
        .map_or_else(
            || {
                raw.strip_prefix("0o")
                    .or_else(|| raw.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            raw.strip_prefix("0b")
                                .or_else(|| raw.strip_prefix("0B"))
                                .map_or((10, raw.as_str()), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    u32::from_str_radix(digits, radix).ok()
}

