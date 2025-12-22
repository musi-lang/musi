use crate::basic::{
    diagnostic::{DiagnosticBag, report},
    errors::{ErrorKind, LexErrorKind},
    interner::Interner,
    source::SourceFile,
    span::Span,
};
use crate::lex::token::Token;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BraceKind {
    Normal,
    Template,
}

const WHITESPACE: &[char] = &[' ', '\t', '\r', '\n'];

const KEYWORDS: &[(&str, Token)] = &[
    ("alias", Token::KwAlias),
    ("and", Token::KwAnd),
    ("as", Token::KwAs),
    ("break", Token::KwBreak),
    ("case", Token::KwCase),
    ("cycle", Token::KwCycle),
    ("defer", Token::KwDefer),
    ("else", Token::KwElse),
    ("extern", Token::KwExtern),
    ("false", Token::KwFalse),
    ("fn", Token::KwFn),
    ("for", Token::KwFor),
    ("if", Token::KwIf),
    ("import", Token::KwImport),
    ("in", Token::KwIn),
    ("is", Token::KwIs),
    ("match", Token::KwMatch),
    ("mod", Token::KwMod),
    ("not", Token::KwNot),
    ("or", Token::KwOr),
    ("record", Token::KwRecord),
    ("return", Token::KwReturn),
    ("sum", Token::KwSum),
    ("true", Token::KwTrue),
    ("try", Token::KwTry),
    ("unsafe", Token::KwUnsafe),
    ("val", Token::KwVal),
    ("var", Token::KwVar),
    ("while", Token::KwWhile),
    ("with", Token::KwWith),
];

const SYMBOLS: &[(&str, Token)] = &[
    ("..<", Token::DotDotLt),
    ("<<", Token::LtLt),
    ("<=", Token::LtEq),
    ("<-", Token::LtMinus),
    ("::", Token::ColonColon),
    (":=", Token::ColonEq),
    ("=>", Token::EqGt),
    (">=", Token::GtEq),
    (">>", Token::GtGt),
    (">]", Token::GtRBrack),
    ("??", Token::QuestionQuestion),
    (".^", Token::DotCaret),
    ("..", Token::DotDot),
    ("/=", Token::SlashEq),
    ("->", Token::MinusGt),
    ("**", Token::StarStar),
    ("[<", Token::LBrackLt),
    ("|>", Token::BarGt),
    ("%", Token::Percent),
    ("&", Token::Amp),
    ("(", Token::LParen),
    (")", Token::RParen),
    ("*", Token::Star),
    ("+", Token::Plus),
    (",", Token::Comma),
    ("-", Token::Minus),
    (".", Token::Dot),
    ("/", Token::Slash),
    (":", Token::Colon),
    (";", Token::Semicolon),
    ("<", Token::Lt),
    ("=", Token::Eq),
    (">", Token::Gt),
    ("?", Token::Question),
    ("@", Token::At),
    ("[", Token::LBrack),
    ("]", Token::RBrack),
    ("^", Token::Caret),
    ("_", Token::Underscore),
    ("{", Token::LBrace),
    ("|", Token::Bar),
    ("}", Token::RBrace),
    ("~", Token::Tilde),
    ("$", Token::Dollar),
];

pub struct Lexer<'a> {
    interner: &'a mut Interner,
    errors: DiagnosticBag,
    source: &'a SourceFile,
    cursor: usize,
    braces: Vec<BraceKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            interner,
            errors: DiagnosticBag::default(),
            source,
            cursor: 0,
            braces: vec![],
        }
    }

    pub fn next_token(&mut self) -> (Token, Span) {
        self.skip_whitespace();
        if self.cursor >= self.source.input.len() {
            return (Token::EOF, self.span(self.cursor, self.cursor));
        }
        let start = self.cursor;
        let char = self.peek(0).unwrap();

        match char {
            '#' if self.peek(1) == Some('!') => self.scan_line_comment(),
            '/' => self.scan_slash_sequence(start),
            '}' if matches!(self.braces.last(), Some(BraceKind::Template)) => {
                let _ = self.braces.pop();
                self.advance(1);
                (self.scan_template(0), self.span(start, self.cursor))
            }
            '"' => (self.scan_text_literal(), self.span(start, self.cursor)),
            '\'' => (self.scan_rune_literal(), self.span(start, self.cursor)),
            '`' => (self.scan_escaped_ident(), self.span(start, self.cursor)),
            '$' if self.peek(1) == Some('"') => {
                (self.scan_template(2), self.span(start, self.cursor))
            }
            c if Self::is_ident_start(c) => (self.scan_ident(), self.span(start, self.cursor)),
            c if c.is_ascii_digit() => (self.scan_number(), self.span(start, self.cursor)),
            _ => match self.scan_symbol() {
                Some(t) => (t, self.span(start, self.cursor)),
                None => self.report_invalid_char(start, char),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        self.cursor = self.consume_while(self.cursor, |c| WHITESPACE.contains(&c));
    }

    fn scan_line_comment(&mut self) -> (Token, Span) {
        self.cursor = self.consume_while(self.cursor, |c| c != '\n');
        self.next_token()
    }

    fn scan_block_comment(&mut self, start: usize) -> (Token, Span) {
        let mut depth = 1;
        self.advance(2);
        while depth > 0 && self.cursor < self.source.input.len() {
            if self.source.input[self.cursor..].starts_with("/*") {
                depth += 1;
                self.advance(2);
            } else if self.source.input[self.cursor..].starts_with("*/") {
                depth -= 1;
                self.advance(2);
            } else {
                self.advance(self.peek(0).map_or(1, |c| c.len_utf8()));
            }
        }
        if depth > 0 {
            self.report(LexErrorKind::UnclosedComment.into(), start, self.cursor);
            (
                self.invalid_token(&self.source.input[start..self.cursor]),
                self.span(start, self.cursor),
            )
        } else {
            self.next_token()
        }
    }

    fn scan_slash_sequence(&mut self, start: usize) -> (Token, Span) {
        match self.peek(1) {
            Some('/') => self.scan_line_comment(),
            Some('*') => self.scan_block_comment(start),
            _ => match self.scan_symbol() {
                Some(t) => (t, self.span(start, self.cursor)),
                None => {
                    self.advance(1);
                    (
                        self.invalid_token(&self.source.input[start..self.cursor]),
                        self.span(start, self.cursor),
                    )
                }
            },
        }
    }

    fn scan_ident(&mut self) -> Token {
        let start = self.cursor;
        let end = self.consume_while(start, Self::is_ident_char);
        self.cursor = end;
        let content = &self.source.input[start..end];
        KEYWORDS
            .binary_search_by_key(&content, |&(k, _)| k)
            .map_or_else(
                |_| Token::Ident(self.interner.intern(content)),
                |idx| KEYWORDS[idx].1.clone(),
            )
    }

    fn scan_number(&mut self) -> Token {
        let start = self.cursor;
        let (base, prefix_len, name) = match (self.peek(0), self.peek(1)) {
            (Some('0'), Some('x')) => (16, 2, "hexadecimal"),
            (Some('0'), Some('o')) => (8, 2, "octal"),
            (Some('0'), Some('b')) => (2, 2, "binary"),
            _ => (10, 0, "decimal"),
        };

        let mut end = self.consume_while(start + prefix_len, |c| c.is_digit(base) || c == '_');
        if end == start + prefix_len {
            self.report(
                LexErrorKind::InvalidLiteral(name.into()).into(),
                start,
                start + 1,
            );
            return Token::LitInt(0);
        }

        let mut is_real = false;
        if base == 10 {
            if self.source.input[end..].starts_with('.')
                && !self.source.input[end..].starts_with("..")
            {
                is_real = true;
                end = self.consume_while(end + 1, Self::is_num_char);
            }
            if self.source.input[end..].starts_with(['e', 'E']) {
                is_real = true;
                let mut exp = end + 1;
                if self.source.input[exp..].starts_with(['+', '-']) {
                    exp += 1;
                }
                end = self.consume_while(exp, Self::is_num_char);
            }
        }

        self.cursor = end;
        let (normalized, valid) = self.normalize_numeric(start, end);
        if !valid {
            self.report(
                LexErrorKind::MalformedUnderscore((if is_real { "real" } else { name }).into())
                    .into(),
                start,
                end,
            );
        }
        if is_real {
            Token::LitReal(self.interner.intern(&normalized))
        } else {
            Token::LitInt(self.interner.intern(&normalized))
        }
    }

    fn scan_text_literal(&mut self) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, 1, &['"'], LexErrorKind::UnclosedString) {
            Some(end) => Token::LitString(self.interner.intern(&self.source.input[start + 1..end])),
            None => self.invalid_token(&self.source.input[start..]),
        }
    }

    fn scan_template(&mut self, offset: usize) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, offset, &['"', '{'], LexErrorKind::UnclosedTemplate) {
            Some(end) => {
                let content = &self.source.input[start + offset..end];
                let is_expr = self.source.input[end..].starts_with('{');
                let interned = self.interner.intern(content);
                if is_expr {
                    self.braces.push(BraceKind::Template);
                    if offset == 2 {
                        Token::TemplateHead(interned)
                    } else {
                        Token::TemplateMiddle(interned)
                    }
                } else if offset == 2 {
                    Token::LitTemplateNoSubst(interned)
                } else {
                    Token::TemplateTail(interned)
                }
            }
            None => self.invalid_token(&self.source.input[start..]),
        }
    }

    fn scan_rune_literal(&mut self) -> Token {
        let start = self.cursor;
        let mut chars = self.source.input[start + 1..].chars();
        let (c, len) = match chars.next() {
            Some('\'') => {
                return {
                    self.report(LexErrorKind::EmptyRune.into(), start, start + 2);
                    self.cursor += 2;
                    Token::LitRune('\0')
                };
            }
            Some('\\') => chars.next().map_or(('\0', 1), |c| (c, 1 + c.len_utf8())),
            Some(c) => (c, c.len_utf8()),
            None => {
                return {
                    self.report(LexErrorKind::UnclosedRune.into(), start, start + 1);
                    self.cursor = self.source.input.len();
                    Token::LitRune('\0')
                };
            }
        };
        if self.source.input[start + 1 + len..].starts_with('\'') {
            self.cursor = start + 1 + len + 1;
            Token::LitRune(c)
        } else {
            let end = self.consume_while(start + 1 + len, |c| c != '\'');
            let is_unclosed = !self.source.input[end..].starts_with('\'');
            self.cursor = if is_unclosed { end } else { end + 1 };
            self.report(
                if is_unclosed {
                    LexErrorKind::UnclosedRune
                } else {
                    LexErrorKind::MultiCharRune
                }
                .into(),
                start,
                self.cursor,
            );
            Token::LitRune('\0')
        }
    }

    fn scan_escaped_ident(&mut self) -> Token {
        let start = self.cursor;
        match self.consume_quoted(start, 1, &['`'], LexErrorKind::UnclosedEscapedIdent) {
            Some(end) => {
                let content = &self.source.input[start + 1..end];
                if content.is_empty() {
                    self.report(LexErrorKind::InvalidIdent.into(), start, self.cursor);
                    self.invalid_token("`")
                } else {
                    Token::Ident(self.interner.intern(content))
                }
            }
            None => self.invalid_token(&self.source.input[start..]),
        }
    }

    fn scan_symbol(&mut self) -> Option<Token> {
        let input = &self.source.input[self.cursor..];
        for &(sym, ref tok) in SYMBOLS {
            if input.starts_with(sym) {
                self.cursor += sym.len();
                if let Token::LBrace = tok {
                    self.braces.push(BraceKind::Normal);
                } else if let Token::RBrace = tok {
                    if let Some(BraceKind::Normal) = self.braces.last() {
                        let _ = self.braces.pop();
                    }
                }
                return Some(tok.clone());
            }
        }
        None
    }

    fn consume_quoted(
        &mut self,
        literal_start: usize,
        prefix_len: usize,
        terminators: &[char],
        error_if_unclosed: LexErrorKind,
    ) -> Option<usize> {
        let mut pos = literal_start + prefix_len;
        let mut chars = self.source.input[pos..].chars();
        while let Some(c) = chars.next() {
            if terminators.contains(&c) {
                self.cursor = pos + 1;
                return Some(pos);
            }
            pos += c.len_utf8();
            if c == '\\' {
                if let Some(n) = chars.next() {
                    pos += n.len_utf8();
                }
            }
        }
        self.cursor = self.source.input.len();
        self.report(error_if_unclosed.into(), literal_start, literal_start + 1);
        None
    }

    fn consume_while<F>(&self, start: usize, predicate: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        self.source.input[start..]
            .char_indices()
            .find(|&(_, c)| !predicate(c))
            .map_or(self.source.input.len(), |(i, _)| start + i)
    }

    fn normalize_numeric(&self, start: usize, end: usize) -> (String, bool) {
        let s = &self.source.input[start..end];
        let mut out = String::with_capacity(s.len());
        let (mut prev, mut valid) = (false, true);
        for (i, c) in s.char_indices() {
            if c == '_' {
                if i == 0 || i == s.len() - 1 || prev {
                    valid = false;
                }
                prev = true;
            } else {
                prev = false;
                out.push(c);
            }
        }
        (out, valid)
    }

    fn invalid_token(&mut self, content: &str) -> Token {
        Token::Invalid(self.interner.intern(content))
    }

    fn report_invalid_char(&mut self, start: usize, char: char) -> (Token, Span) {
        self.advance(char.len_utf8());
        self.report(LexErrorKind::InvalidChar(char).into(), start, self.cursor);
        (
            self.invalid_token(&char.to_string()),
            self.span(start, self.cursor),
        )
    }

    fn report(&mut self, err: ErrorKind, start: usize, end: usize) {
        self.errors.add(report(err, self.span(start, end)));
    }

    const fn span(&self, start: usize, end: usize) -> Span {
        Span::new(
            self.source.start + start as u32,
            self.source.start + end as u32,
        )
    }

    fn peek(&self, index: usize) -> Option<char> {
        self.source.input[self.cursor..].chars().nth(index)
    }

    fn advance(&mut self, count: usize) {
        self.cursor += count;
    }

    const fn is_ident_start(c: char) -> bool {
        c == '_' || c.is_ascii_alphabetic()
    }

    const fn is_ident_char(c: char) -> bool {
        Self::is_ident_start(c) || c.is_ascii_digit()
    }

    const fn is_num_char(c: char) -> bool {
        c.is_ascii_digit() || c == '_'
    }
}
