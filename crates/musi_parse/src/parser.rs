//! LL(1) + Pratt parser for the Musi language.
//!
//! Produces a typed AST from a flat [`Token`] slice.  All recursive children are
//! arena-allocated via [`AstArenas`]; clients hold [`Idx`] handles.

mod ctrl;
mod defs;
mod exprs;
mod types;
mod util;

use core::mem;

use musi_lex::token::{Token, TokenKind};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Slice, Span};

use crate::ast::{AstArenas, BinOp, Expr, ParsedModule};

/// Parses a token stream into a [`ParsedModule`].
///
/// `tokens` **must** end with [`TokenKind::Eof`].
pub fn parse(
    tokens: &[Token],
    file_id: FileId,
    diags: &mut DiagnosticBag,
    interner: &Interner,
) -> ParsedModule {
    let mut parser = Parser::new(tokens, file_id, diags, interner);
    parser.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
    interner: &'a Interner,
    ctx: AstArenas,
}

impl<'a> Parser<'a> {
    #[allow(clippy::missing_const_for_fn)] // &mut refs in const fns is unstable
    fn new(
        tokens: &'a [Token],
        file_id: FileId,
        diags: &'a mut DiagnosticBag,
        interner: &'a Interner,
    ) -> Self {
        Self {
            tokens,
            pos: 0,
            file_id,
            diags,
            interner,
            ctx: AstArenas::new(),
        }
    }

    #[must_use]
    fn peek(&self) -> &Token {
        // self.pos is always in bounds: the token list always ends with Eof
        // and we never advance past it.
        &self.tokens[self.pos]
    }

    #[must_use]
    fn peek_kind(&self) -> TokenKind {
        self.peek().kind
    }

    #[must_use]
    fn peek2(&self) -> TokenKind {
        if self.pos + 1 < self.tokens.len() {
            self.tokens[self.pos + 1].kind
        } else {
            TokenKind::Eof
        }
    }

    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if tok.kind != TokenKind::Eof {
            self.pos += 1;
        }
        tok
    }

    fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            let _tok = self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Span {
        if self.at(kind) {
            self.advance().span
        } else {
            let span = self.peek().span;
            let _diag = self.diags.error(
                format!(
                    "expected `{}`, found `{}`",
                    kind_text(kind),
                    kind_text(self.peek_kind())
                ),
                span,
                self.file_id,
            );
            self.recover();
            span
        }
    }

    fn start_span(&self) -> u32 {
        self.peek().span.start
    }

    #[must_use]
    fn finish_span(&self, start: u32) -> Span {
        let prev = if self.pos > 0 {
            &self.tokens[self.pos - 1]
        } else {
            self.peek()
        };
        let end = prev.span.end();
        if end >= start {
            Span::new(start, end - start)
        } else {
            Span::new(start, 0)
        }
    }

    fn alloc_expr(&mut self, e: Expr) -> Idx<Expr> {
        self.ctx.exprs.alloc(e)
    }

    fn recover(&mut self) {
        loop {
            match self.peek_kind() {
                TokenKind::Semi
                | TokenKind::RParen
                | TokenKind::RBrace
                | TokenKind::RBracket
                | TokenKind::Eof => break,
                _ => {
                    let _tok = self.advance();
                }
            }
        }
    }

    fn error_expr(&mut self, msg: &str) -> Expr {
        let span = self.peek().span;
        let _diag = self.diags.error(msg, span, self.file_id);
        self.recover();
        Expr::Error {
            span: self.finish_span(span.start),
        }
    }

    fn parse_program(&mut self) -> ParsedModule {
        let start = self.start_span();
        let mut raw = Vec::new();
        while !self.at(TokenKind::Eof) {
            let pos_before = self.pos;
            raw.push(self.parse_and_alloc_expr());
            // Every top-level statement requires a mandatory `;`.
            let _semi = self.expect(TokenKind::Semi);
            // Guarantee progress: `recover()` stops at `)` / `}` / `]` without
            // consuming them, so a stray closing bracket at top level would
            // otherwise loop forever.  If no tokens were consumed in this
            // iteration, force-advance one token.
            if self.pos == pos_before {
                let span = self.peek().span;
                let _d = self.diags.error(
                    format!("unexpected `{}`", kind_text(self.peek_kind())),
                    span,
                    self.file_id,
                );
                let _tok = self.advance();
            }
        }
        let span = self.finish_span(start);
        let items: Slice<_> = self.ctx.expr_lists.alloc_slice(raw);
        let ctx = mem::take(&mut self.ctx);
        ParsedModule { items, ctx, span }
    }
}

// -- Helpers -----------------------------------------------------------------

enum InfixKind {
    Binary(BinOp),
    Assign,
}

/// Returns a human-readable name for a token kind (for error messages).
#[must_use]
fn kind_text(kind: TokenKind) -> &'static str {
    kind.fixed_text().unwrap_or(match kind {
        TokenKind::Ident => "identifier",
        TokenKind::TyIdent => "type variable",
        TokenKind::IntLit => "integer literal",
        TokenKind::FloatLit => "float literal",
        TokenKind::StringLit => "string literal",
        TokenKind::CharLit => "character literal",
        TokenKind::Eof => "end of file",
        TokenKind::Error => "error token",
        _ => "token",
    })
}

/// Returns `true` if `kind` can start an expression (used for optional expr
/// parsing in `return`, `break`, etc.).
#[must_use]
const fn can_start_expr(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident
            | TokenKind::IntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit
            | TokenKind::CharLit
            | TokenKind::LParen
            | TokenKind::LBracket
            | TokenKind::LBrace
            | TokenKind::Minus
            | TokenKind::Not
            | TokenKind::Bang
            | TokenKind::At
            | TokenKind::Tilde
            | TokenKind::Hash
            | TokenKind::Fn
            | TokenKind::Record
            | TokenKind::Choice
            | TokenKind::Const
            | TokenKind::Var
            | TokenKind::If
            | TokenKind::Match
            | TokenKind::While
            | TokenKind::Loop
            | TokenKind::For
            | TokenKind::Label
            | TokenKind::Return
            | TokenKind::Break
            | TokenKind::Cycle
            | TokenKind::Defer
            | TokenKind::Import
            | TokenKind::Export
            | TokenKind::Opaque
            | TokenKind::Extrin
            | TokenKind::Class
            | TokenKind::Given
    )
}

/// Parses the text of an integer literal (decimal, hex, octal, binary)
/// into an `i64`.  Underscores are stripped.
fn parse_int_lit(text: &str) -> i64 {
    let clean = text.replace('_', "");
    let (digits, radix) = strip_radix_prefix(&clean);
    i64::from_str_radix(digits, radix).unwrap_or(0)
}

/// Strips `0x`/`0o`/`0b` prefixes and returns `(digits, radix)`.
fn strip_radix_prefix(s: &str) -> (&str, u32) {
    if let Some(h) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X")) {
        return (h, 16);
    }
    if let Some(o) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
        return (o, 8);
    }
    if let Some(b) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        return (b, 2);
    }
    (s, 10)
}

/// Parses the text of a character literal (`'x'` or `'\n'`) into a `char`.
fn parse_char_lit(text: &str) -> char {
    let inner = text
        .strip_prefix('\'')
        .and_then(|s| s.strip_suffix('\''))
        .unwrap_or(text);
    inner.strip_prefix('\\').map_or_else(
        || inner.chars().next().unwrap_or('\0'),
        |esc| match esc.as_bytes().first() {
            Some(b'n') => '\n',
            Some(b't') => '\t',
            Some(b'r') => '\r',
            Some(b'0') => '\0',
            Some(b'\\') => '\\',
            Some(b'\'') => '\'',
            _ => inner.chars().next().unwrap_or('\0'),
        },
    )
}

#[cfg(test)]
mod tests;
