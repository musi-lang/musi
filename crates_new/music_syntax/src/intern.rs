use music_names::{Ident, Interner};

use crate::{LexedSource, TokenKind};

#[must_use]
pub fn canonical_name_text<'src>(token_kind: TokenKind, raw: &'src str) -> &'src str {
    match token_kind {
        TokenKind::OpIdent => raw
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(raw),
        _ => raw,
    }
}

#[must_use]
pub fn intern_name_token<'src>(
    lexed: &LexedSource<'src>,
    token_index: usize,
    interner: &mut Interner,
) -> Option<Ident> {
    let token = lexed.tokens().get(token_index)?;
    if !matches!(token.kind, TokenKind::Ident | TokenKind::OpIdent) {
        return None;
    }
    let raw = lexed.token_text(token_index)?;
    let canonical = canonical_name_text(token.kind, raw);
    let sym = interner.intern(canonical);
    Some(Ident::new(sym, token.span))
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
