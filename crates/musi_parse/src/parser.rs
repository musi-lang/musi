use musi_ast::AstArena;
use musi_core::{Diagnostic, DiagnosticBag, Interner, MusiError, MusiResult, Span, Symbol};
use musi_lex::token::{NumericBase, Token, TokenKind};

use crate::errors;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Prec {
    None = 0,
    Assign = 1,
    Pipe = 2,
    Coalesce = 3,
    Disjunction = 4,
    Conjunction = 5,
    BitOr = 6,
    BitXor = 7,
    BitAnd = 8,
    Equality = 9,
    Comparison = 10,
    Range = 11,
    Cons = 12,
    Term = 13,
    Factor = 14,
    Power = 15,
    Unary = 16,
    Postfix = 17,
}

impl Prec {
    #[must_use]
    pub const fn infix(op: TokenKind) -> Option<(u8, u8)> {
        Some(match op {
            TokenKind::LtMinus => (1, 1),
            TokenKind::BarGt => (2, 3),
            TokenKind::QuestionQuestion => (3, 4),
            TokenKind::KwOr => (4, 5),
            TokenKind::KwAnd => (5, 6),
            TokenKind::Bar => (6, 7),
            TokenKind::Caret => (7, 8),
            TokenKind::Amp => (8, 9),
            TokenKind::Eq | TokenKind::SlashEq => (9, 10),
            TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LtEq
            | TokenKind::GtEq
            | TokenKind::KwIs
            | TokenKind::KwAs
            | TokenKind::KwIn => (10, 11),
            TokenKind::DotDot | TokenKind::DotDotLt => (11, 12),
            TokenKind::ColonColon => (12, 12),
            TokenKind::Plus | TokenKind::Minus => (13, 14),
            TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::LtLt
            | TokenKind::GtGt => (14, 15),
            TokenKind::StarStar => (15, 15),
            _ => return None,
        })
    }

    #[must_use]
    pub const fn prefix(op: TokenKind) -> Option<u8> {
        match op {
            TokenKind::Minus | TokenKind::KwNot | TokenKind::Tilde | TokenKind::At => Some(16),
            _ => None,
        }
    }

    #[must_use]
    pub const fn postfix(op: TokenKind) -> Option<u8> {
        match op {
            TokenKind::LParen
            | TokenKind::Dot
            | TokenKind::DotCaret
            | TokenKind::Question
            | TokenKind::Bang => Some(17),
            _ => None,
        }
    }
}

pub struct Parser<'a> {
    pub tokens: &'a [Token],
    pub index: usize,
    pub arena: &'a mut AstArena,
    pub interner: &'a Interner,
    pub diagnostics: DiagnosticBag,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(tokens: &'a [Token], arena: &'a mut AstArena, interner: &'a Interner) -> Self {
        Self {
            tokens,
            index: 0,
            arena,
            interner,
            diagnostics: DiagnosticBag::default(),
        }
    }

    #[must_use]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    #[must_use]
    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    #[must_use]
    pub fn peek_nth(&self, n: usize) -> Option<TokenKind> {
        self.tokens.get(self.index + n).map(|t| t.kind)
    }

    pub fn advance(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.index);
        if tok.is_some() {
            self.index += 1;
        }
        tok
    }

    pub fn advance_by(&mut self, n: usize) {
        self.index = (self.index + n).min(self.tokens.len());
    }

    #[must_use]
    pub fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind() == Some(kind)
    }

    #[must_use]
    pub fn at_any(&self, kinds: &[TokenKind]) -> bool {
        self.peek_kind().is_some_and(|k| kinds.contains(&k))
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        matches!(self.peek_kind(), Some(k) if k == kind)
    }

    #[must_use]
    pub const fn is_eof(&self) -> bool {
        self.index >= self.tokens.len()
    }

    #[must_use]
    pub const fn prev_span(&self) -> Span {
        if self.index > 0 {
            self.tokens[self.index - 1].span
        } else {
            Span::new(0, 0)
        }
    }

    #[must_use]
    pub fn curr_span(&self) -> Span {
        self.peek().map_or_else(|| self.prev_span(), |t| t.span)
    }

    /// # Errors
    /// Returns error if current token does not match expected kind.
    ///
    /// # Panics
    /// Panics if `at(kind)` but `advance()` returns `None` (impossible).
    pub fn expect(&mut self, kind: TokenKind) -> Result<&Token, MusiError> {
        if self.at(kind) {
            Ok(self.advance().expect("checked by `at()`"))
        } else {
            Err(errors::expected_token(kind, self.curr_span()))
        }
    }

    pub fn bump_if(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    #[must_use]
    pub fn at_closing(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(TokenKind::RParen | TokenKind::RBrack | TokenKind::RBrace) | None
        )
    }

    /// # Errors
    /// Returns error from `parse` if any item fails.
    pub fn separated<T>(
        &mut self,
        sep: TokenKind,
        mut parse: impl FnMut(&mut Self) -> Result<T, MusiError>,
    ) -> Result<Vec<T>, MusiError> {
        let mut items = vec![];
        while !self.at_closing() && !self.is_eof() {
            items.push(parse(self)?);
            if !self.bump_if(sep) {
                break;
            }
        }
        Ok(items)
    }

    /// # Errors
    /// Returns error if delimiters are missing or content fails.
    pub fn delimited<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        parse: impl FnOnce(&mut Self) -> Result<T, MusiError>,
    ) -> Result<T, MusiError> {
        let _ = self.expect(open)?;
        let result = parse(self)?;
        let _ = self.expect(close)?;
        Ok(result)
    }

    /// # Errors
    /// Returns error if open delim present but content or close fails.
    pub fn opt_delimited<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        parse: impl FnOnce(&mut Self) -> Result<Vec<T>, MusiError>,
    ) -> Result<Vec<T>, MusiError> {
        if self.at(open) {
            self.delimited(open, close, parse)
        } else {
            Ok(vec![])
        }
    }

    /// # Errors
    /// Returns error if parser fails after trigger is consumed.
    pub fn maybe<T>(
        &mut self,
        trigger: TokenKind,
        parse: impl FnOnce(&mut Self) -> Result<T, MusiError>,
    ) -> Result<Option<T>, MusiError> {
        if self.bump_if(trigger) {
            Ok(Some(parse(self)?))
        } else {
            Ok(None)
        }
    }

    pub fn report(&mut self, diag: Diagnostic) {
        self.diagnostics.add(diag);
    }

    pub fn report_err(&mut self, err: MusiError) {
        let mut diag = Diagnostic::new(err.level, err.message, err.span);
        if let Some(code) = err.code {
            diag = diag.with_code(code);
        }
        if let Some(hint) = err.hint {
            diag = diag.with_note(hint, err.span);
        }
        self.diagnostics.add(diag);
    }

    pub fn sync(&mut self) {
        loop {
            match self.peek_kind() {
                None | Some(TokenKind::RBrace) => break,
                Some(TokenKind::Semicolon) => {
                    _ = self.advance();
                    break;
                }
                _ => {
                    _ = self.advance();
                }
            }
        }
    }

    pub fn sync_to_stmt(&mut self) {
        _ = self.advance();
        loop {
            match self.peek_kind() {
                Some(TokenKind::Semicolon) => {
                    _ = self.advance();
                    break;
                }
                Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => break,
                Some(k) if is_stmt_starter(k) => break,
                _ => {
                    _ = self.advance();
                }
            }
        }
    }

    pub fn sync_to_block_end(&mut self) {
        let mut depth = 1;
        loop {
            match self.peek_kind() {
                None => break,
                Some(TokenKind::LBrace) => {
                    depth += 1;
                    _ = self.advance();
                }
                Some(TokenKind::RBrace) => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    _ = self.advance();
                }
                _ => {
                    _ = self.advance();
                }
            }
        }
    }

    pub(crate) fn parse_int(&self, raw: Symbol, base: NumericBase) -> MusiResult<i64> {
        let s = self.interner.lookup(raw.id).unwrap_or("0");
        i64::from_str_radix(s, base.radix()).map_err(|_| errors::invalid_literal(raw.span))
    }

    pub(crate) fn parse_float(&self, raw: Symbol) -> MusiResult<f64> {
        let s = self.interner.lookup(raw.id).unwrap_or("0.0");
        s.parse::<f64>()
            .map_err(|_| errors::invalid_literal(raw.span))
    }
}

const fn is_stmt_starter(k: TokenKind) -> bool {
    matches!(
        k,
        TokenKind::LitInt { .. }
            | TokenKind::LitFloat { .. }
            | TokenKind::LitString(_)
            | TokenKind::LitRune(_)
            | TokenKind::LitTemplateNoSubst(_)
            | TokenKind::TemplateHead(_)
            | TokenKind::KwTrue
            | TokenKind::KwFalse
            | TokenKind::Ident(_)
            | TokenKind::LParen
            | TokenKind::LBrack
            | TokenKind::LBrace
            | TokenKind::Minus
            | TokenKind::KwNot
            | TokenKind::Tilde
            | TokenKind::At
            | TokenKind::KwIf
            | TokenKind::KwWhile
            | TokenKind::KwFor
            | TokenKind::KwMatch
            | TokenKind::KwReturn
            | TokenKind::KwDefer
            | TokenKind::KwBreak
            | TokenKind::KwCycle
            | TokenKind::KwUnsafe
            | TokenKind::KwImport
            | TokenKind::KwRecord
            | TokenKind::KwChoice
            | TokenKind::KwFn
            | TokenKind::KwVal
            | TokenKind::KwVar
            | TokenKind::KwExport
            | TokenKind::KwExtern
            | TokenKind::AtLBrack
    )
}
