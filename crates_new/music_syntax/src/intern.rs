use crate::TokenKind;

#[must_use]
pub fn canonical_name_text(token_kind: TokenKind, raw: &str) -> &str {
    match token_kind {
        TokenKind::OpIdent => raw
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .unwrap_or(raw),
        _ => raw,
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
