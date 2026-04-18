use music_syntax::{LexedSource, TokenKind};

use crate::token_class::{is_operator, is_word_like};

pub fn regular_group_next_segment_len(lexed: &LexedSource, start_index: usize) -> usize {
    let mut depth = 0usize;
    let mut len = 0usize;
    let mut previous = TokenKind::Comma;
    for index in start_index..lexed.tokens().len() {
        let token = lexed.tokens()[index];
        if token.kind == TokenKind::Eof {
            break;
        }
        if depth == 0 && matches!(token.kind, TokenKind::Comma) {
            break;
        }
        if depth == 0 && matches!(token.kind, TokenKind::RParen | TokenKind::RBracket) {
            len = len.saturating_add(1);
            break;
        }
        if token_tail_needs_space(previous, token.kind) {
            len = len.saturating_add(1);
        }
        len = len.saturating_add(lexed.token_text(index).map_or(0, str::len));
        match token.kind {
            TokenKind::LParen | TokenKind::LBracket => depth = depth.saturating_add(1),
            TokenKind::RParen | TokenKind::RBracket => depth = depth.saturating_sub(1),
            _ => {}
        }
        previous = token.kind;
    }
    len
}

fn token_tail_needs_space(previous: TokenKind, current: TokenKind) -> bool {
    if matches!(
        previous,
        TokenKind::LParen | TokenKind::LBracket | TokenKind::Dot
    ) || matches!(
        current,
        TokenKind::RParen | TokenKind::RBracket | TokenKind::Dot
    ) {
        return false;
    }
    matches!(previous, TokenKind::Comma)
        || is_operator(previous)
        || is_operator(current)
        || (is_word_like(previous) && is_word_like(current))
}
