use crate::{
    cursor::Cursor,
    token::{KEYWORDS, NumericBase, NumericSuffix, SYMBOLS, Token, TokenKind},
};
use musi_basic::{interner::Interner, source::SourceFile, span::Span, types::Ident};
use musi_errors::{DiagnosticBag, helpers};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LexerState {
    Normal,
    InTemplate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Normal,
    Template,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    interner: &'a mut Interner,
    errors: DiagnosticBag,
    source: &'a SourceFile,
    cursor: Cursor<'a>,
    state: LexerState,
    braces: Vec<BraceKind>,
}

impl<'a> Lexer<'a> {
    /// Creates new lexer for source file.
    pub fn new(source: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            interner,
            errors: DiagnosticBag::default(),
            source,
            cursor: Cursor::new(&source.input),
            state: LexerState::Normal,
            braces: vec![],
        }
    }

    #[must_use]
    /// Returns reference to accumulated errors.
    pub const fn errors_ref(&self) -> &DiagnosticBag {
        &self.errors
    }

    #[must_use]
    /// Consumes lexer and returns accumulated errors.
    pub fn errors(self) -> DiagnosticBag {
        self.errors
    }

    /// Scans next token from input.
    pub fn next_token(&mut self) -> Token {
        let start = self.cursor.pos();

        let kind = match self.state {
            LexerState::Normal => self.scan_root(start),
            LexerState::InTemplate => self.scan_template_content(start, false),
        };

        if kind == TokenKind::EOF {
            return Token::new(
                kind,
                self.make_span(self.source.input.len(), self.source.input.len()),
            );
        }

        let end = self.cursor.pos();
        Token::new(kind, self.make_span(start, end))
    }

    fn scan_root(&mut self, start: usize) -> TokenKind {
        match self.cursor.peek() {
            Some(c) => match c {
                ' ' | '\t' | '\r' => {
                    self.cursor
                        .eat_while(|c| c == ' ' || c == '\t' || c == '\r');
                    TokenKind::Whitespace
                }
                '\n' => {
                    let _ = self.cursor.bump();
                    TokenKind::Newline
                }
                '/' => self.scan_slash_or_comment(start),
                '"' => self.scan_lit_string(start),
                '\'' => self.scan_lit_rune(start),
                '`' => self.scan_escaped_ident(start),
                '0'..='9' => self.scan_number(start),
                '$' if self.cursor.peek_nth(1) == Some('"') => {
                    self.cursor.bump_n(2);
                    self.scan_template_content(start, true)
                }
                c if is_ident_start(c) => self.scan_ident(start),
                _ => self.scan_symbol(start),
            },
            None => TokenKind::EOF,
        }
    }

    fn scan_slash_or_comment(&mut self, start: usize) -> TokenKind {
        let _ = self.cursor.bump();
        match self.cursor.peek() {
            Some('/') => self.scan_line_comment(),
            Some('*') => self.scan_block_comment(start),
            Some('=') => {
                let _ = self.cursor.bump();
                TokenKind::SlashEq
            }
            _ => TokenKind::Slash,
        }
    }

    fn scan_line_comment(&mut self) -> TokenKind {
        let _ = self.cursor.bump();
        let docstyle = self.cursor.is_next('/');
        if docstyle {
            let _ = self.cursor.bump();
        }
        self.cursor.eat_while(|c| c != '\n');
        TokenKind::LineComment { docstyle }
    }

    fn scan_block_comment(&mut self, start: usize) -> TokenKind {
        let _ = self.cursor.bump();
        let docstyle = self.cursor.is_next('*') && self.cursor.peek_nth(1) != Some('/');
        if docstyle {
            let _ = self.cursor.bump();
        }

        let mut depth = 1;
        while depth > 0 {
            match (self.cursor.peek(), self.cursor.peek_nth(1)) {
                (Some('/'), Some('*')) => {
                    depth += 1;
                    self.cursor.bump_n(2);
                }
                (Some('*'), Some('/')) => {
                    depth -= 1;
                    self.cursor.bump_n(2);
                }
                (None, _) => {
                    self.errors.add(helpers::unclosed_block_comment(
                        self.make_span(start, self.cursor.pos()),
                    ));
                    break;
                }
                _ => {
                    let _ = self.cursor.bump();
                }
            }
        }
        TokenKind::BlockComment { docstyle }
    }

    fn scan_lit_string(&mut self, start: usize) -> TokenKind {
        let _ = self.cursor.bump();
        let mut out = String::new();
        loop {
            match self.cursor.peek() {
                Some('"') => {
                    let _ = self.cursor.bump();
                    let id = self.interner.intern(&out);
                    return TokenKind::LitString(Ident::new(
                        id,
                        self.make_span(start, self.cursor.pos()),
                    ));
                }
                Some('\\') => {
                    let _ = self.cursor.bump();
                    let (c, _) = self.scan_escape(start);
                    out.push(c);
                }
                Some(c) => {
                    let _ = self.cursor.bump();
                    out.push(c);
                }
                None => {
                    self.errors.add(helpers::unclosed_string(
                        self.make_span(start, self.cursor.pos()),
                    ));
                    let id = self.interner.intern(&out);
                    return TokenKind::LitString(Ident::new(
                        id,
                        self.make_span(start, self.cursor.pos()),
                    ));
                }
            }
        }
    }

    fn scan_template_content(&mut self, start: usize, is_head: bool) -> TokenKind {
        let mut out = String::new();
        loop {
            match self.cursor.peek() {
                Some('"') => {
                    let _ = self.cursor.bump();
                    self.state = LexerState::Normal;

                    let id = self.interner.intern(&out);
                    let span = self.make_span(start, self.cursor.pos());
                    if is_head {
                        return TokenKind::LitTemplateNoSubst(Ident::new(id, span));
                    }
                    return TokenKind::TemplateTail(Ident::new(id, span));
                }
                Some('{') => {
                    let _ = self.cursor.bump();
                    self.braces.push(BraceKind::Template);
                    self.state = LexerState::Normal;

                    let id = self.interner.intern(&out);
                    let span = self.make_span(start, self.cursor.pos());
                    if is_head {
                        return TokenKind::TemplateHead(Ident::new(id, span));
                    }
                    return TokenKind::TemplateMiddle(Ident::new(id, span));
                }
                Some('\\') => {
                    let _ = self.cursor.bump();
                    let (c, _) = self.scan_escape(start);
                    out.push(c);
                }
                Some(c) => {
                    let _ = self.cursor.bump();
                    out.push(c);
                }
                None => {
                    self.errors.add(helpers::unclosed_template(
                        self.make_span(start, self.cursor.pos()),
                    ));
                    let id = self.interner.intern(&out);
                    return TokenKind::LitString(Ident::new(
                        id,
                        self.make_span(start, self.cursor.pos()),
                    ));
                }
            }
        }
    }

    fn scan_lit_rune(&mut self, start: usize) -> TokenKind {
        let _ = self.cursor.bump();
        let c = match self.cursor.peek() {
            Some('\\') => {
                let _ = self.cursor.bump();
                self.scan_escape(start).0
            }
            Some('\'') => {
                self.errors.add(helpers::invalid_rune(
                    self.make_span(start, self.cursor.pos()),
                ));
                '\0'
            }
            Some(c) => {
                let _ = self.cursor.bump();
                c
            }
            None => {
                self.errors.add(helpers::unclosed_rune(
                    self.make_span(start, self.cursor.pos()),
                ));
                '\0'
            }
        };

        if self.cursor.is_next('\'') {
            let _ = self.cursor.bump();
        } else {
            self.errors.add(helpers::unclosed_rune(
                self.make_span(start, self.cursor.pos()),
            ));
        }
        TokenKind::LitRune(c)
    }

    fn scan_escaped_ident(&mut self, start: usize) -> TokenKind {
        let _ = self.cursor.bump();
        let mut out = String::new();
        loop {
            match self.cursor.peek() {
                Some('`') => {
                    let _ = self.cursor.bump();
                    break;
                }
                Some(c) => {
                    let _ = self.cursor.bump();
                    out.push(c);
                }
                None => {
                    self.errors.add(helpers::unclosed_escaped_ident(
                        self.make_span(start, self.cursor.pos()),
                    ));
                    break;
                }
            }
        }
        let id = self.interner.intern(&out);
        TokenKind::Ident(Ident::new(id, self.make_span(start, self.cursor.pos())))
    }

    fn scan_number(&mut self, start: usize) -> TokenKind {
        let base = self.scan_number_base();
        let radix = base.radix();

        self.cursor.eat_while(|c| c.is_digit(radix) || c == '_');

        let has_frac = self.has_fraction_part(base);
        let has_exp = self.has_exponent_part(base);
        let is_float = has_frac || has_exp;
        let suffix_data = self.scan_number_suffix();
        self.make_number(start, base, is_float, suffix_data)
    }

    fn scan_number_base(&mut self) -> NumericBase {
        if self.cursor.peek() == Some('0') {
            match self.cursor.peek_nth(1) {
                Some('x') => {
                    self.cursor.bump_n(2);
                    NumericBase::Hex
                }
                Some('o') => {
                    self.cursor.bump_n(2);
                    NumericBase::Octal
                }
                Some('b') => {
                    self.cursor.bump_n(2);
                    NumericBase::Binary
                }
                _ => NumericBase::Decimal,
            }
        } else {
            NumericBase::Decimal
        }
    }

    fn has_fraction_part(&mut self, base: NumericBase) -> bool {
        if base == NumericBase::Decimal
            && self.cursor.peek() == Some('.')
            && self.cursor.peek_nth(1) != Some('.')
        {
            let _ = self.cursor.bump();
            self.cursor.eat_while(is_decimal_digit_part);
            true
        } else {
            false
        }
    }

    fn has_exponent_part(&mut self, base: NumericBase) -> bool {
        if base == NumericBase::Decimal
            && (self.cursor.peek() == Some('e') || self.cursor.peek() == Some('E'))
        {
            let _ = self.cursor.bump();
            if self.cursor.peek() == Some('+') || self.cursor.peek() == Some('-') {
                let _ = self.cursor.bump();
            }
            self.cursor.eat_while(is_decimal_digit_part);
            true
        } else {
            false
        }
    }

    fn scan_number_suffix(&mut self) -> (Option<NumericSuffix>, usize) {
        let suffix_start = self.cursor.pos();
        if is_ident_start(self.cursor.peek().unwrap_or('\0')) {
            self.cursor.eat_while(is_ident_continue);
            let suf_str = self
                .source
                .input
                .get(suffix_start..self.cursor.pos())
                .unwrap_or("");
            (suf_str.parse().ok(), suffix_start)
        } else {
            (None, suffix_start)
        }
    }

    fn make_number(
        &mut self,
        start: usize,
        base: NumericBase,
        is_float: bool,
        suffix_data: (Option<NumericSuffix>, usize),
    ) -> TokenKind {
        let (suffix, suffix_start) = suffix_data;
        let raw_str = self.source.input.get(start..suffix_start).unwrap_or("");
        let id = self.interner.intern(raw_str);

        if is_float {
            TokenKind::LitFloat {
                raw: Ident::new(id, self.make_span(start, suffix_start)),
                suffix,
            }
        } else {
            TokenKind::LitInt {
                raw: Ident::new(id, self.make_span(start, suffix_start)),
                base,
                suffix,
            }
        }
    }

    fn scan_symbol(&mut self, _start: usize) -> TokenKind {
        let rest = self.cursor.rest();
        let mut matched_len = 0;
        let mut matched_kind = None;

        for (kind, sym) in SYMBOLS {
            if rest.starts_with(sym) && sym.len() > matched_len {
                matched_len = sym.len();
                matched_kind = Some(*kind);
            }
        }

        if let Some(kind) = matched_kind {
            self.cursor.bump_n(matched_len);
            match kind {
                TokenKind::LBrace => {
                    self.braces.push(BraceKind::Normal);
                    TokenKind::LBrace
                }
                TokenKind::RBrace => {
                    if self.braces.last() == Some(&BraceKind::Template) {
                        let _ = self.braces.pop();
                        self.state = LexerState::InTemplate;
                    } else if self.braces.last() == Some(&BraceKind::Normal) {
                        let _ = self.braces.pop();
                    }
                    TokenKind::RBrace
                }
                _ => kind,
            }
        } else if let Some(c) = self.cursor.bump() {
            let start = self.cursor.pos() - c.len_utf8();
            self.errors.add(helpers::unknown_char(
                c,
                self.make_span(start, self.cursor.pos()),
            ));
            let id = self.interner.intern(&c.to_string());
            TokenKind::Error(Ident::new(id, self.make_span(start, self.cursor.pos())))
        } else {
            TokenKind::EOF
        }
    }

    fn scan_ident(&mut self, start: usize) -> TokenKind {
        self.cursor.eat_while(is_ident_continue);
        let text = self
            .source
            .input
            .get(start..self.cursor.pos())
            .unwrap_or("");

        if let Ok(idx) = KEYWORDS.binary_search_by_key(&text, |k| k.0) {
            KEYWORDS[idx].1
        } else {
            let id = self.interner.intern(text);
            TokenKind::Ident(Ident::new(id, self.make_span(start, self.cursor.pos())))
        }
    }

    fn scan_escape(&mut self, start_pos: usize) -> (char, usize) {
        match self.cursor.bump() {
            Some('n') => ('\n', 1),
            Some('r') => ('\r', 1),
            Some('t') => ('\t', 1),
            Some('\\') => ('\\', 1),
            Some('\'') => ('\'', 1),
            Some('"') => ('"', 1),
            Some('0') => ('\0', 1),
            Some(c) => {
                self.errors.add(helpers::unknown_escape(
                    c,
                    self.make_span(start_pos, self.cursor.pos()),
                ));
                (c, 1)
            }
            None => ('\0', 0),
        }
    }

    fn make_span(&self, lo: usize, hi: usize) -> Span {
        Span::new(
            self.source.start + u32::try_from(lo).unwrap_or(0),
            self.source.start + u32::try_from(hi).unwrap_or(0),
        )
    }
}

const fn is_ident_start(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

const fn is_ident_continue(c: char) -> bool {
    c.is_ascii_alphanumeric() || c == '_'
}

const fn is_decimal_digit_part(c: char) -> bool {
    c.is_ascii_digit() || c == '_'
}

#[cfg(test)]
mod tests;
