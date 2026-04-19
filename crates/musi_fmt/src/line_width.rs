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

pub fn group_flat_len(lexed: &LexedSource, open_index: usize) -> usize {
    let Some(open) = lexed.tokens().get(open_index).copied() else {
        return 0;
    };
    let close = match open.kind {
        TokenKind::LParen => TokenKind::RParen,
        TokenKind::LBracket => TokenKind::RBracket,
        _ => return 0,
    };
    let mut depth = 0usize;
    let mut len = 0usize;
    let mut previous = None;
    for index in open_index..lexed.tokens().len() {
        let token = lexed.tokens()[index];
        if token.kind == TokenKind::Eof {
            break;
        }
        if let Some(previous) = previous
            && token_tail_needs_space(previous, token.kind)
        {
            len = len.saturating_add(1);
        }
        len = len.saturating_add(lexed.token_text(index).map_or(0, str::len));
        match token.kind {
            kind if kind == open.kind => depth = depth.saturating_add(1),
            kind if kind == close => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    break;
                }
            }
            _ => {}
        }
        previous = Some(token.kind);
    }
    len
}

pub fn rhs_flat_len(lexed: &LexedSource, start_index: usize) -> usize {
    rhs_flat_len_until(lexed, start_index, false)
}

pub fn rhs_field_flat_len(lexed: &LexedSource, start_index: usize) -> usize {
    rhs_flat_len_until(lexed, start_index, true)
}

fn rhs_flat_len_until(lexed: &LexedSource, start_index: usize, stop_at_comma: bool) -> usize {
    let mut paren_depth = 0usize;
    let mut brace_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut len = 0usize;
    let mut previous = None;
    for index in start_index..lexed.tokens().len() {
        let token = lexed.tokens()[index];
        if token.kind == TokenKind::Eof {
            break;
        }
        if paren_depth == 0
            && brace_depth == 0
            && bracket_depth == 0
            && token.kind == TokenKind::Semicolon
        {
            break;
        }
        if stop_at_comma
            && paren_depth == 0
            && brace_depth == 0
            && bracket_depth == 0
            && token.kind == TokenKind::Comma
        {
            break;
        }
        if let Some(previous) = previous
            && token_tail_needs_space(previous, token.kind)
        {
            len = len.saturating_add(1);
        }
        len = len.saturating_add(lexed.token_text(index).map_or(0, str::len));
        match token.kind {
            TokenKind::LParen => paren_depth = paren_depth.saturating_add(1),
            TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LBrace => brace_depth = brace_depth.saturating_add(1),
            TokenKind::RBrace => brace_depth = brace_depth.saturating_sub(1),
            TokenKind::LBracket => bracket_depth = bracket_depth.saturating_add(1),
            TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
            _ => {}
        }
        previous = Some(token.kind);
    }
    len
}

pub fn rhs_block_header_len(lexed: &LexedSource, start_index: usize) -> Option<usize> {
    let first = lexed.tokens().get(start_index)?;
    if !matches!(
        first.kind,
        TokenKind::KwClass
            | TokenKind::KwData
            | TokenKind::KwEffect
            | TokenKind::KwInstance
            | TokenKind::KwUnsafe
    ) {
        return None;
    }

    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut len = 0usize;
    let mut previous = None;
    for index in start_index..lexed.tokens().len() {
        let token = lexed.tokens()[index];
        if token.kind == TokenKind::Eof {
            break;
        }
        if paren_depth == 0 && bracket_depth == 0 && token.kind == TokenKind::Semicolon {
            return None;
        }
        if let Some(previous) = previous
            && token_tail_needs_space(previous, token.kind)
        {
            len = len.saturating_add(1);
        }
        len = len.saturating_add(lexed.token_text(index).map_or(0, str::len));
        match token.kind {
            TokenKind::LParen => paren_depth = paren_depth.saturating_add(1),
            TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LBracket => bracket_depth = bracket_depth.saturating_add(1),
            TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
            TokenKind::LBrace if paren_depth == 0 && bracket_depth == 0 => return Some(len),
            _ => {}
        }
        previous = Some(token.kind);
    }
    None
}

pub fn declaration_tail_flat_len(lexed: &LexedSource, start_index: usize) -> usize {
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut len = 0usize;
    let mut previous = None;
    for index in start_index..lexed.tokens().len() {
        let token = lexed.tokens()[index];
        if token.kind == TokenKind::Eof {
            break;
        }
        if paren_depth == 0
            && bracket_depth == 0
            && matches!(token.kind, TokenKind::ColonEq | TokenKind::Semicolon)
        {
            break;
        }
        if let Some(previous) = previous
            && token_tail_needs_space(previous, token.kind)
        {
            len = len.saturating_add(1);
        }
        len = len.saturating_add(lexed.token_text(index).map_or(0, str::len));
        match token.kind {
            TokenKind::LParen => paren_depth = paren_depth.saturating_add(1),
            TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LBracket => bracket_depth = bracket_depth.saturating_add(1),
            TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
            _ => {}
        }
        previous = Some(token.kind);
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
    if current == TokenKind::LBrace && matches!(previous, TokenKind::RBracket | TokenKind::RParen) {
        return true;
    }
    matches!(previous, TokenKind::Comma)
        || is_operator(previous)
        || is_operator(current)
        || (is_word_like(previous) && is_word_like(current))
}
