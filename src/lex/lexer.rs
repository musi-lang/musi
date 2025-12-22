use crate::basic::{
    diagnostic::{DiagnosticBag, report},
    errors::{Error, LexicalError},
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
    offset: usize,
    brace_stack: Vec<BraceKind>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a SourceFile, interner: &'a mut Interner) -> Self {
        Self {
            interner,
            errors: DiagnosticBag::default(),
            source,
            offset: 0,
            brace_stack: vec![],
        }
    }

    pub fn next_token(&mut self) -> (Token, Span) {
        self.skip_whitespace();
        if self.offset >= self.source.input.len() {
            return (Token::EOF, self.span(self.offset, self.offset));
        }
        let start = self.offset;
        let c = self.peek(0).unwrap();

        match c {
            '#' if self.peek(1) == Some('!') => self.scan_line_comment(),
            '/' => self.scan_slash_or_comment(start),
            '}' if matches!(self.brace_stack.last(), Some(BraceKind::Template)) => {
                let _ = self.brace_stack.pop();
                self.advance(1);
                (self.scan_template(0), self.span(start, self.offset))
            }
            '"' => (self.scan_string(), self.span(start, self.offset)),
            '\'' => (self.scan_rune(), self.span(start, self.offset)),
            '`' => (self.scan_ident_escape(), self.span(start, self.offset)),
            '$' if self.peek(1) == Some('"') => {
                (self.scan_template(2), self.span(start, self.offset))
            }
            c if Self::is_ident_start(c) => (self.scan_ident(), self.span(start, self.offset)),
            c if c.is_ascii_digit() => (self.scan_number(), self.span(start, self.offset)),
            _ => match self.scan_symbol() {
                Some(t) => (t, self.span(start, self.offset)),
                None => self.report_unknown_char(start, c),
            },
        }
    }

    fn skip_whitespace(&mut self) {
        self.offset = self.scan_while(self.offset, |c| WHITESPACE.contains(&c));
    }

    fn scan_line_comment(&mut self) -> (Token, Span) {
        self.offset = self.scan_while(self.offset, |c| c != '\n');
        self.next_token()
    }

    fn scan_block_comment(&mut self, start: usize) -> (Token, Span) {
        let mut depth = 1;
        self.advance(2);
        while depth > 0 && self.offset < self.source.input.len() {
            if self.source.input[self.offset..].starts_with("/*") {
                depth += 1;
                self.advance(2);
            } else if self.source.input[self.offset..].starts_with("*/") {
                depth -= 1;
                self.advance(2);
            } else {
                self.advance(self.peek(0).map_or(1, |c| c.len_utf8()));
            }
        }
        if depth > 0 {
            self.report(LexicalError::UnclosedComment.into(), start, self.offset);
            (
                self.unknown_token(&self.source.input[start..self.offset]),
                self.span(start, self.offset),
            )
        } else {
            self.next_token()
        }
    }

    fn scan_slash_or_comment(&mut self, start: usize) -> (Token, Span) {
        match self.peek(1) {
            Some('/') => self.scan_line_comment(),
            Some('*') => self.scan_block_comment(start),
            _ => match self.scan_symbol() {
                Some(t) => (t, self.span(start, self.offset)),
                None => {
                    self.advance(1);
                    (
                        self.unknown_token(&self.source.input[start..self.offset]),
                        self.span(start, self.offset),
                    )
                }
            },
        }
    }

    fn scan_ident(&mut self) -> Token {
        let start = self.offset;
        let end = self.scan_while(start, Self::is_ident_char);
        self.offset = end;
        let content = &self.source.input[start..end];
        KEYWORDS
            .binary_search_by_key(&content, |&(k, _)| k)
            .map_or_else(
                |_| Token::Ident(self.interner.intern(content)),
                |idx| KEYWORDS[idx].1.clone(),
            )
    }

    fn scan_number(&mut self) -> Token {
        let start = self.offset;
        let (radix, off, name) = match (self.peek(0), self.peek(1)) {
            (Some('0'), Some('x')) => (16, 2, "hexadecimal"),
            (Some('0'), Some('o')) => (8, 2, "octal"),
            (Some('0'), Some('b')) => (2, 2, "binary"),
            _ => (10, 0, "decimal"),
        };

        let mut end = self.scan_while(start + off, |c| c.is_digit(radix) || c == '_');
        if end == start + off {
            self.report(
                LexicalError::InvalidLiteral(name.into()).into(),
                start,
                start + 1,
            );
            return Token::LitInt(0);
        }

        let mut is_real = false;
        if radix == 10 {
            if self.source.input[end..].starts_with('.')
                && !self.source.input[end..].starts_with("..")
            {
                is_real = true;
                end = self.scan_while(end + 1, Self::is_num_char);
            }
            if self.source.input[end..].starts_with(['e', 'E']) {
                is_real = true;
                let mut exp = end + 1;
                if self.source.input[exp..].starts_with(['+', '-']) {
                    exp += 1;
                }
                end = self.scan_while(exp, Self::is_num_char);
            }
        }

        self.offset = end;
        let (clean, valid) = self.handle_underscores(start, end);
        if !valid {
            self.report(
                LexicalError::MalformedUnderscore((if is_real { "real" } else { name }).into())
                    .into(),
                start,
                end,
            );
        }
        if is_real {
            Token::LitReal(self.interner.intern(&clean))
        } else {
            Token::LitInt(self.interner.intern(&clean))
        }
    }

    fn scan_string(&mut self) -> Token {
        let start = self.offset;
        match self.scan_quoted(start, 1, &['"'], LexicalError::UnclosedString) {
            Some(end) => Token::LitString(self.interner.intern(&self.source.input[start + 1..end])),
            None => self.unknown_token(&self.source.input[start..]),
        }
    }

    fn scan_template(&mut self, offset: usize) -> Token {
        let start = self.offset;
        match self.scan_quoted(start, offset, &['"', '{'], LexicalError::UnclosedTemplate) {
            Some(end) => {
                let content = &self.source.input[start + offset..end];
                let is_expr = self.source.input[end..].starts_with('{');
                let interned = self.interner.intern(content);
                if is_expr {
                    self.brace_stack.push(BraceKind::Template);
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
            None => self.unknown_token(&self.source.input[start..]),
        }
    }

    fn scan_rune(&mut self) -> Token {
        let start = self.offset;
        let mut chars = self.source.input[start + 1..].chars();
        let (c, len) = match chars.next() {
            Some('\'') => {
                return {
                    self.report(LexicalError::EmptyRune.into(), start, start + 2);
                    self.offset += 2;
                    Token::LitRune('\0')
                };
            }
            Some('\\') => chars.next().map_or(('\0', 1), |c| (c, 1 + c.len_utf8())),
            Some(c) => (c, c.len_utf8()),
            None => {
                return {
                    self.report(LexicalError::UnclosedRune.into(), start, start + 1);
                    self.offset = self.source.input.len();
                    Token::LitRune('\0')
                };
            }
        };
        if self.source.input[start + 1 + len..].starts_with('\'') {
            self.offset = start + 1 + len + 1;
            Token::LitRune(c)
        } else {
            let end = self.scan_while(start + 1 + len, |c| c != '\'');
            let is_unclosed = !self.source.input[end..].starts_with('\'');
            self.offset = if is_unclosed { end } else { end + 1 };
            self.report(
                if is_unclosed {
                    LexicalError::UnclosedRune
                } else {
                    LexicalError::MultiCharRune
                }
                .into(),
                start,
                self.offset,
            );
            Token::LitRune('\0')
        }
    }

    fn scan_ident_escape(&mut self) -> Token {
        let start = self.offset;
        match self.scan_quoted(start, 1, &['`'], LexicalError::UnclosedEscapedIdent) {
            Some(end) => {
                let content = &self.source.input[start + 1..end];
                if content.is_empty() {
                    self.report(LexicalError::InvalidIdent.into(), start, self.offset);
                    self.unknown_token("`")
                } else {
                    Token::Ident(self.interner.intern(content))
                }
            }
            None => self.unknown_token(&self.source.input[start..]),
        }
    }

    fn scan_symbol(&mut self) -> Option<Token> {
        let input = &self.source.input[self.offset..];
        for &(sym, ref tok) in SYMBOLS {
            if input.starts_with(sym) {
                self.offset += sym.len();
                if let Token::LBrace = tok {
                    self.brace_stack.push(BraceKind::Normal);
                } else if let Token::RBrace = tok {
                    if let Some(BraceKind::Normal) = self.brace_stack.last() {
                        self.brace_stack.pop();
                    }
                }
                return Some(tok.clone());
            }
        }
        None
    }

    fn scan_quoted(
        &mut self,
        start: usize,
        off: usize,
        stop: &[char],
        err: LexicalError,
    ) -> Option<usize> {
        let mut pos = start + off;
        let mut chars = self.source.input[pos..].chars();
        while let Some(c) = chars.next() {
            if stop.contains(&c) {
                self.offset = pos + 1;
                return Some(pos);
            }
            pos += c.len_utf8();
            if c == '\\' {
                if let Some(n) = chars.next() {
                    pos += n.len_utf8();
                }
            }
        }
        self.offset = self.source.input.len();
        self.report(err.into(), start, start + 1);
        None
    }

    fn scan_while<F>(&self, start: usize, pred: F) -> usize
    where
        F: Fn(char) -> bool,
    {
        self.source.input[start..]
            .char_indices()
            .find(|&(_, c)| !pred(c))
            .map_or(self.source.input.len(), |(i, _)| start + i)
    }

    fn handle_underscores(&self, start: usize, end: usize) -> (String, bool) {
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

    fn unknown_token(&mut self, content: &str) -> Token {
        Token::Unknown(self.interner.intern(content))
    }

    fn report_unknown_char(&mut self, start: usize, c: char) -> (Token, Span) {
        self.advance(c.len_utf8());
        self.report(LexicalError::UnknownChar(c).into(), start, self.offset);
        (
            self.unknown_token(&c.to_string()),
            self.span(start, self.offset),
        )
    }

    fn report(&mut self, err: Error, start: usize, end: usize) {
        self.errors.add(report(err, self.span(start, end)));
    }

    const fn span(&self, start: usize, end: usize) -> Span {
        Span::new(
            self.source.start + start as u32,
            self.source.start + end as u32,
        )
    }

    fn peek(&self, n: usize) -> Option<char> {
        self.source.input[self.offset..].chars().nth(n)
    }

    fn advance(&mut self, n: usize) {
        self.offset += n;
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
