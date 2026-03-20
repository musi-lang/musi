//! Pattern parsing.

#[cfg(test)]
mod tests;

use msc_ast::PatIdx;
use msc_ast::expr::BindKind;
use msc_ast::lit::Lit;
use msc_ast::pat::{Pat, PatRecField};
use msc_lex::token::TokenKind;
use msc_shared::Symbol;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses a pattern: `primary { 'or' primary }`.
    pub(crate) fn parse_pat(&mut self) -> Pat {
        let start = self.start_span();
        let mut lhs = self.parse_pat_primary();

        while self.eat(TokenKind::KwOr) {
            let rhs = self.parse_pat_primary();
            let l_idx = self.alloc_pat(lhs);
            let r_idx = self.alloc_pat(rhs);
            lhs = Pat::Or {
                left: l_idx,
                right: r_idx,
                span: self.finish_span(start),
            };
        }
        lhs
    }

    fn parse_pat_primary(&mut self) -> Pat {
        match self.peek_kind() {
            TokenKind::Underscore => {
                let start = self.start_span();
                let _us = self.bump();
                Pat::Wild {
                    span: self.finish_span(start),
                }
            }

            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::RuneLit => {
                let start = self.start_span();
                let lit = self.parse_lit_value();
                Pat::Lit {
                    lit,
                    span: self.finish_span(start),
                }
            }

            TokenKind::KwMut => {
                let _mut = self.bump();
                self.parse_pat_ident(BindKind::Mut)
            }
            TokenKind::Ident => self.parse_pat_ident(BindKind::Immut),

            // Variant destructor: .Name or .Name(pats)
            TokenKind::Dot => {
                let start = self.start_span();
                let _dot = self.bump();
                let name = self.expect_symbol();
                let args = if self.at(TokenKind::LParen) {
                    self.delimited(
                        TokenKind::LParen,
                        TokenKind::RParen,
                        Self::parse_alloc_pat_typed,
                    )
                } else {
                    vec![]
                };
                Pat::Variant {
                    name,
                    args,
                    span: self.finish_span(start),
                }
            }

            // Anonymous record pattern: .{ fields } or Record pattern: { fields }
            TokenKind::DotLBrace | TokenKind::LBrace => {
                let start = self.start_span();
                let _ = self.bump();
                let fields = self.comma_sep(TokenKind::RBrace, Self::parse_pat_rec_field);
                let _rb = self.expect(TokenKind::RBrace);
                Pat::Record {
                    ty_name: None,
                    fields,
                    span: self.finish_span(start),
                }
            }

            // Tuple pattern: (pats)
            TokenKind::LParen => {
                let start = self.start_span();
                let pats =
                    self.delimited(TokenKind::LParen, TokenKind::RParen, Self::parse_alloc_pat);
                Pat::Tuple {
                    elems: pats,
                    span: self.finish_span(start),
                }
            }

            // Array pattern: [pats]
            TokenKind::LBracket => {
                let start = self.start_span();
                let pats = self.delimited(
                    TokenKind::LBracket,
                    TokenKind::RBracket,
                    Self::parse_alloc_pat,
                );
                Pat::Array {
                    elems: pats,
                    span: self.finish_span(start),
                }
            }

            _ => self.error_pat(&ParseError::ExpectedPattern),
        }
    }

    fn parse_pat_ident(&mut self, kind: BindKind) -> Pat {
        let start = self.start_span();
        let name = self.expect_symbol();

        // Check for destructor suffix: Name(pats) or Name{ fields }
        match self.peek_kind() {
            TokenKind::LParen => {
                let args = self.delimited(
                    TokenKind::LParen,
                    TokenKind::RParen,
                    Self::parse_alloc_pat_typed,
                );
                Pat::Variant {
                    name,
                    args,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBrace => {
                let _lb = self.bump();
                let fields = self.comma_sep(TokenKind::RBrace, Self::parse_pat_rec_field);
                let _rb = self.expect(TokenKind::RBrace);
                Pat::Record {
                    ty_name: Some(name),
                    fields,
                    span: self.finish_span(start),
                }
            }
            _ => {
                let inner = if self.eat(TokenKind::KwAs) {
                    Some(self.parse_alloc_pat())
                } else {
                    None
                };
                Pat::Bind {
                    kind,
                    name,
                    ty: None,
                    inner,
                    span: self.finish_span(start),
                }
            }
        }
    }

    fn parse_pat_rec_field(&mut self) -> PatRecField {
        let start = self.start_span();
        let kind = if self.eat(TokenKind::KwMut) {
            BindKind::Mut
        } else {
            BindKind::Immut
        };
        let name = self.expect_symbol();
        let pat: Option<PatIdx> = if self.eat(TokenKind::Colon) {
            Some(self.parse_alloc_pat())
        } else {
            None
        };
        PatRecField {
            kind,
            name,
            pat,
            span: self.finish_span(start),
        }
    }

    /// Parses a literal value from the current token.
    pub(crate) fn parse_lit_value(&mut self) -> Lit {
        let tok = self.bump();
        let span = tok.span;
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
        match tok.kind {
            TokenKind::IntLit => {
                let text = self.resolve(sym);
                let value = parse_int_lit(text).unwrap_or_else(|| {
                    let _diag = self.diags.error(
                        ParseError::InvalidIntLiteral.to_string(),
                        span,
                        self.file_id,
                    );
                    0
                });
                Lit::Int { value, span }
            }
            TokenKind::FloatLit => {
                let text = self.resolve(sym);
                let value = text.replace('_', "").parse::<f64>().unwrap_or_else(|_| {
                    let _diag = self.diags.error(
                        ParseError::InvalidFloatLiteral.to_string(),
                        span,
                        self.file_id,
                    );
                    0.0
                });
                Lit::Float { value, span }
            }
            TokenKind::StringLit => {
                let raw = self.resolve(sym);
                let value = if raw.contains('\\') {
                    let unescaped = unescape_str(raw);
                    self.interner.intern(&unescaped)
                } else {
                    sym
                };
                Lit::Str { value, span }
            }
            TokenKind::RuneLit => {
                let text = self.resolve(sym);
                let codepoint = parse_rune_lit(text).unwrap_or_else(|| {
                    let _diag = self.diags.error(
                        ParseError::InvalidRuneEscape.to_string(),
                        span,
                        self.file_id,
                    );
                    0
                });
                Lit::Rune { codepoint, span }
            }
            _ => {
                let _diag =
                    self.diags
                        .error(ParseError::ExpectedLiteral.to_string(), span, self.file_id);
                Lit::Int { value: 0, span }
            }
        }
    }
}

/// Parses integer literal text (decimal, hex, octal, binary) into `i64`.
fn parse_int_lit(text: &str) -> Option<i64> {
    let clean = text.replace('_', "");
    let (digits, radix) = clean
        .strip_prefix("0x")
        .or_else(|| clean.strip_prefix("0X"))
        .map_or_else(
            || {
                clean
                    .strip_prefix("0o")
                    .or_else(|| clean.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            clean
                                .strip_prefix("0b")
                                .or_else(|| clean.strip_prefix("0B"))
                                .map_or_else(|| (clean.as_str(), 10), |b| (b, 2))
                        },
                        |o| (o, 8),
                    )
            },
            |h| (h, 16),
        );
    i64::from_str_radix(digits, radix).ok()
}

/// Parses a rune literal (`'x'` or `'\n'`) into a `u32` codepoint.
fn parse_rune_lit(text: &str) -> Option<u32> {
    let inner = text.strip_prefix('\'').and_then(|s| s.strip_suffix('\''))?;
    let ch = inner.strip_prefix('\\').map_or_else(
        || inner.chars().next(),
        |esc| match esc.as_bytes().first().copied() {
            Some(b'n') => Some('\n'),
            Some(b't') => Some('\t'),
            Some(b'r') => Some('\r'),
            Some(b'0') => Some('\0'),
            Some(b'\\') => Some('\\'),
            Some(b'\'') => Some('\''),
            _ => None,
        },
    )?;
    Some(u32::from(ch))
}

/// Process backslash escape sequences in a string literal.
fn unescape_str(raw: &str) -> String {
    let mut out = String::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => out.push('\n'),
                Some('t') => out.push('\t'),
                Some('r') => out.push('\r'),
                Some('0') => out.push('\0'),
                Some(c @ ('\\' | '"')) => out.push(c),
                Some(other) => {
                    out.push('\\');
                    out.push(other);
                }
                None => out.push('\\'),
            }
        } else {
            out.push(c);
        }
    }
    out
}
