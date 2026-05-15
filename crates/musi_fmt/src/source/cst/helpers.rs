use music_syntax::{LexedSource, TokenKind};

pub(super) fn starts_with_item_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'-', ..])
}

pub(super) fn starts_with_module_doc_block_comment(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'!', ..])
}

pub(super) fn next_non_comma_token_kind(
    lexed: &LexedSource,
    start_index: usize,
) -> Option<TokenKind> {
    lexed
        .tokens()
        .iter()
        .skip(start_index)
        .find(|token| token.kind != TokenKind::Comma)
        .map(|token| token.kind)
}

pub(super) fn is_let_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("let ")
        || trimmed.starts_with("let(")
        || trimmed.starts_with("export let ")
        || trimmed.starts_with("export let(")
        || trimmed.starts_with("native let ")
        || trimmed.starts_with("native let(")
        || trimmed.starts_with("export native ")
}

pub(super) fn newline_count(text: &str) -> usize {
    text.bytes().filter(|byte| *byte == b'\n').count()
}

pub(super) const fn is_closing(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket
    )
}
