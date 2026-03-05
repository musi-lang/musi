//! LL(1) + Pratt parser for the Musi language.
//!
//! Produces a typed AST from a flat [`Token`] slice.  All recursive children are
//! arena-allocated via [`ParseCtx`]; clients hold [`Idx`] handles.

use core::mem;

use musi_lex::token::{Token, TokenKind};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Span, Symbol};

use crate::ast::{
    ArrayItem, Attr, AttrArg, BinOp, BindKind, ChoiceVariant, Cond, ElifChain, Expr, ImportClause,
    ImportItem, LitValue, MatchArm, Modifier, Param, ParseCtx, ParsedModule, Pat, PatField,
    PatSuffix, PostfixOp, PrefixOp, RecField, RecLitField, Ty, TyParam, VariantPayload,
};

// ── Public entry point ──────────────────────────────────────────────────────

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

// ── Parser ──────────────────────────────────────────────────────────────────

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    file_id: FileId,
    diags: &'a mut DiagnosticBag,
    interner: &'a Interner,
    ctx: ParseCtx,
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
            ctx: ParseCtx::new(),
        }
    }

    // ── Cursor helpers ──────────────────────────────────────────────────

    /// Returns the current token without advancing.
    /// Safe: the token list always ends with `Eof`.
    #[must_use]
    fn peek(&self) -> &Token {
        // The token list always ends with Eof, and we never advance past Eof,
        // so `self.pos` is always in bounds.
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

    /// Consumes the current token and advances.
    fn advance(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if tok.kind != TokenKind::Eof {
            self.pos += 1;
        }
        tok
    }

    #[must_use]
    fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    /// Consumes the current token if it matches `kind`.  Returns `true` if consumed.
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            let _tok = self.advance();
            true
        } else {
            false
        }
    }

    /// Expects `kind` — consumes if matching, otherwise emits an error and returns
    /// a dummy span at the current position.
    fn expect(&mut self, kind: TokenKind) -> Span {
        if self.at(kind) {
            self.advance().span
        } else {
            let got = self.peek();
            let got_text = kind_text(got.kind);
            let exp_text = kind_text(kind);
            let _diag = self.diags.error(format!("expected {exp_text}, found {got_text}"), got.span, self.file_id);
            Span::DUMMY
        }
    }

    #[must_use]
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

    // ── Arena allocation helpers ────────────────────────────────────────

    fn alloc_expr(&mut self, e: Expr) -> Idx<Expr> {
        self.ctx.exprs.alloc(e)
    }

    // ── Error recovery ──────────────────────────────────────────────────

    /// Skips tokens until a likely synchronization point.
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

    // ── Top-level ───────────────────────────────────────────────────────

    fn parse_program(&mut self) -> ParsedModule {
        let start = self.start_span();
        let mut items = Vec::new();
        while !self.at(TokenKind::Eof) {
            let pos_before = self.pos;
            items.push(self.parse_and_alloc_expr());
            // Each top-level item is followed by `;` (optional at EOF).
            let _semi = self.eat(TokenKind::Semi);
            // Guarantee progress: `recover()` stops at `)` / `}` / `]` without
            // consuming them, so a stray closing bracket at top level would
            // otherwise loop forever.  If no tokens were consumed in this
            // iteration, force-advance one token.
            if self.pos == pos_before {
                let span = self.peek().span;
                let _d = self.diags.error(format!("unexpected `{}`", kind_text(self.peek_kind())), span, self.file_id);
                let _tok = self.advance();
            }
        }
        let span = self.finish_span(start);
        let ctx = mem::take(&mut self.ctx);
        ParsedModule { items, ctx, span }
    }

    // ── Expression parsing (Pratt) ──────────────────────────────────────

    fn parse_expr(&mut self) -> Expr {
        self.parse_pratt(0)
    }

    fn parse_pratt(&mut self, min_bp: u8) -> Expr {
        let start = self.start_span();

        // 1. Prefix / atom
        let mut lhs = self.parse_prefix_or_atom();

        // 2. Postfix chain
        lhs = self.parse_postfix_chain(lhs, start);

        // 3. Infix chain
        loop {
            let Some((l_bp, r_bp, op_kind)) = self.infix_info() else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let _op_tok = self.advance();
            let rhs_expr = self.parse_pratt(r_bp);
            let lhs_idx = self.alloc_expr(lhs);
            let rhs_idx = self.alloc_expr(rhs_expr);
            let span = self.finish_span(start);

            lhs = match op_kind {
                InfixKind::Binary(op) => Expr::Binary {
                    op,
                    lhs: lhs_idx,
                    rhs: rhs_idx,
                    span,
                },
                InfixKind::Assign => Expr::Assign {
                    target: lhs_idx,
                    value: rhs_idx,
                    span,
                },
            };
        }

        lhs
    }

    // ── Infix precedence table ──────────────────────────────────────────

    /// Returns `(left_bp, right_bp, kind)` for the current token if it is an
    /// infix operator, or `None` otherwise.
    #[must_use]
    fn infix_info(&self) -> Option<(u8, u8, InfixKind)> {
        use BinOp as B;
        use InfixKind::{Assign, Binary};
        use TokenKind as T;

        let kind = self.peek_kind();
        let (l, r, op) = match kind {
            // BP 10 — assign (right-assoc)
            T::LtMinus => (10, 9, Assign),
            // BP 20 — or / xor (left-assoc)
            T::Or => (20, 21, Binary(B::Or)),
            T::Xor => (20, 21, Binary(B::Xor)),
            // BP 30 — and (left-assoc)
            T::And => (30, 31, Binary(B::And)),
            // BP 40 — equality (non-assoc)
            T::Eq => (40, 41, Binary(B::Eq)),
            T::SlashEq => (40, 41, Binary(B::NotEq)),
            // BP 50 — comparison (non-assoc)
            T::Lt => (50, 51, Binary(B::Lt)),
            T::Gt => (50, 51, Binary(B::Gt)),
            T::LtEq => (50, 51, Binary(B::LtEq)),
            T::GtEq => (50, 51, Binary(B::GtEq)),
            T::In => (50, 51, Binary(B::In)),
            // BP 60 — range (non-assoc)
            T::DotDot => (60, 61, Binary(B::Range)),
            T::DotDotLt => (60, 61, Binary(B::RangeExcl)),
            // BP 70 — cons (left-assoc)
            T::ColonColon => (70, 71, Binary(B::Cons)),
            // BP 80 — bitwise or / xor (left-assoc)
            T::Pipe => (80, 81, Binary(B::BitOr)),
            T::Caret => (80, 81, Binary(B::BitXor)),
            // BP 90 — bitwise and (left-assoc)
            T::Amp => (90, 91, Binary(B::BitAnd)),
            // BP 100 — shift (left-assoc)
            T::Shl => (100, 101, Binary(B::Shl)),
            T::Shr => (100, 101, Binary(B::Shr)),
            // BP 110 — additive (left-assoc)
            T::Plus => (110, 111, Binary(B::Add)),
            T::Minus => (110, 111, Binary(B::Sub)),
            // BP 120 — multiplicative (left-assoc)
            T::Star => (120, 121, Binary(B::Mul)),
            T::Slash => (120, 121, Binary(B::Div)),
            T::Percent => (120, 121, Binary(B::Rem)),
            _ => return None,
        };
        Some((l, r, op))
    }

    // ── Postfix parsing ─────────────────────────────────────────────────

    fn parse_postfix_chain(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            match self.peek_kind() {
                // Call: f(args)
                TokenKind::LParen => {
                    let _lp = self.advance();
                    let args = self.parse_expr_list(TokenKind::RParen);
                    let _rp = self.expect(TokenKind::RParen);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(base, PostfixOp::Call { args, span: self.finish_span(start) }, start);
                }
                // Index: e.[args]
                TokenKind::DotLBracket => {
                    let _dlb = self.advance();
                    let args = self.parse_expr_list(TokenKind::RBracket);
                    let _rb = self.expect(TokenKind::RBracket);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(base, PostfixOp::Index { args, span: self.finish_span(start) }, start);
                }
                // RecDot: e.{ fields }
                TokenKind::DotLBrace => {
                    let _dlb = self.advance();
                    let fields = self.parse_separated_list(TokenKind::RBrace, |p| p.parse_rec_lit_field());
                    let _rb = self.expect(TokenKind::RBrace);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(base, PostfixOp::RecDot { fields, span: self.finish_span(start) }, start);
                }
                // Field: e.name or e.0
                TokenKind::Dot => {
                    let _dot = self.advance();
                    let field_start = self.start_span();
                    match self.peek_kind() {
                        TokenKind::Ident | TokenKind::IntLit => {
                            let name = self.expect_symbol();
                            let f_span = self.finish_span(field_start);
                            let base = self.alloc_expr(lhs);
                            lhs = self.wrap_postfix(base, PostfixOp::Field { name, span: f_span }, start);
                        }
                        _ => {
                            let _err = self.diags.error("expected field name", self.peek().span, self.file_id);
                            break;
                        }
                    }
                }
                // As cast: e as T
                TokenKind::As => {
                    let _as_kw = self.advance();
                    let ty = self.parse_ty();
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(base, PostfixOp::As { ty, span: self.finish_span(start) }, start);
                }
                _ => break,
            }
        }
        lhs
    }

    // ── Prefix / atom dispatch ──────────────────────────────────────────

    fn parse_prefix(&mut self, op: PrefixOp) -> Expr {
        let start = self.start_span();
        let _tok = self.advance();
        let inner = self.parse_pratt(130);
        let operand = self.alloc_expr(inner);
        Expr::Prefix {
            op,
            operand,
            span: self.finish_span(start),
        }
    }

    fn parse_prefix_or_atom(&mut self) -> Expr {
        // Doc comments (`///`) are emitted as tokens so their text can be
        // used for documentation.  The parser doesn't attach them to nodes
        // yet, so consume and discard any leading doc-comment tokens here.
        while self.eat(TokenKind::DocComment) {}
        match self.peek_kind() {
            // Prefix operators (BP 130)
            TokenKind::Minus => self.parse_prefix(PrefixOp::Neg),
            TokenKind::Not => self.parse_prefix(PrefixOp::Not),
            TokenKind::Bang => self.parse_prefix(PrefixOp::Deref),
            TokenKind::At => self.parse_prefix(PrefixOp::AddrOf),
            TokenKind::Tilde => self.parse_prefix(PrefixOp::BitNot),

            // Atoms
            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::CharLit => {
                self.parse_lit()
            }

            TokenKind::Ident => {
                // Check for `true` and `false` (parsed by lexer as Ident)
                let start = self.start_span();
                let tok = self.advance().clone();
                let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
                let text = self.resolve(sym);
                if text == "true" || text == "false" {
                    Expr::Lit {
                        value: LitValue::Bool(text == "true"),
                        span: self.finish_span(start),
                    }
                } else {
                    Expr::Ident {
                        name: sym,
                        span: self.finish_span(start),
                    }
                }
            }

            TokenKind::LParen => self.parse_expr_paren(),

            TokenKind::LBracket => self.parse_array_lit(),

            TokenKind::LBrace => self.parse_anon_rec_lit(),

            // Keywords dispatching to dedicated parse methods
            TokenKind::If => self.parse_if(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Cycle => self.parse_cycle(),
            TokenKind::Defer => self.parse_defer(),
            TokenKind::Import => self.parse_import(),

            // Constructs that may carry attrs/modifiers
            TokenKind::Hash | TokenKind::Export | TokenKind::Opaque | TokenKind::Native => {
                self.parse_with_prefix()
            }
            TokenKind::Fn => self.parse_fn_expr(Vec::new(), Vec::new()),
            TokenKind::Record => self.parse_record(Vec::new(), Vec::new()),
            TokenKind::Choice => self.parse_choice(Vec::new(), Vec::new()),
            TokenKind::Const | TokenKind::Var => self.parse_bind(Vec::new(), Vec::new()),

            // Loop constructs
            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Label => self.parse_label(),

            _ => self.error_expr("unexpected token"),
        }
    }

    // ── Literal ─────────────────────────────────────────────────────────

    fn parse_lit(&mut self) -> Expr {
        let start = self.start_span();
        let value = self.parse_lit_value();
        Expr::Lit {
            value,
            span: self.finish_span(start),
        }
    }

    // ── Parenthesised expression (LL(1)) ────────────────────────────────

    fn parse_expr_paren(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.expect(TokenKind::LParen);

        // Unit: ()
        if self.eat(TokenKind::RParen) {
            return Expr::Unit {
                span: self.finish_span(start),
            };
        }

        // Parse first expression
        let first = self.parse_expr();

        match self.peek_kind() {
            // Tuple: (e, ...)
            TokenKind::Comma => {
                let _comma = self.advance();
                let mut elements = vec![self.alloc_expr(first)];
                elements.extend(self.parse_separated_list(TokenKind::RParen, |p| p.parse_and_alloc_expr()));
                let _rp = self.expect(TokenKind::RParen);
                Expr::Tuple {
                    elements,
                    span: self.finish_span(start),
                }
            }
            // Block: (e; ... [tail])
            TokenKind::Semi => {
                let _semi = self.advance();
                let first_idx = self.alloc_expr(first);
                // Parse remaining statements and tail until `)`
                self.parse_block_tail(vec![first_idx], start)
            }
            _ => {
                // Single: (e), or unexpected token with error recovery; RParen expected
                let _rp = self.expect(TokenKind::RParen);
                let inner = self.alloc_expr(first);
                Expr::Paren {
                    inner,
                    span: self.finish_span(start),
                }
            }
        }
    }

    // ── Array literal ───────────────────────────────────────────────────

    fn parse_array_lit(&mut self) -> Expr {
        let start = self.start_span();
        let _lb = self.expect(TokenKind::LBracket);
        let mut items = Vec::new();

        if !self.at(TokenKind::RBracket) {
            loop {
                if self.eat(TokenKind::LtDotDot) {
                    items.push(ArrayItem::Spread(self.parse_and_alloc_expr()));
                } else {
                    items.push(ArrayItem::Single(self.parse_and_alloc_expr()));
                }
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RBracket) {
                    break;
                }
            }
        }
        let _rb = self.expect(TokenKind::RBracket);
        Expr::Array {
            items,
            span: self.finish_span(start),
        }
    }

    // ── Anonymous record literal ────────────────────────────────────────

    fn parse_anon_rec_lit(&mut self) -> Expr {
        let start = self.start_span();
        let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| p.parse_rec_lit_field());
        Expr::AnonRec {
            fields,
            span: self.finish_span(start),
        }
    }

    // ── Record literal fields ───────────────────────────────────────────

    fn parse_rec_lit_field(&mut self) -> RecLitField {
        let start = self.start_span();

        // Spread: <.. expr
        if self.eat(TokenKind::LtDotDot) {
            let idx = self.parse_and_alloc_expr();
            return RecLitField::Spread {
                expr: idx,
                span: self.finish_span(start),
            };
        }

        // field_base: [attrs] [var] name := expr
        let attrs = self.maybe_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        let _bind = self.expect(TokenKind::ColonEq);
        let value = self.parse_and_alloc_expr();
        RecLitField::Named {
            attrs,
            mutable,
            name,
            value,
            span: self.finish_span(start),
        }
    }

    // ── If ──────────────────────────────────────────────────────────────

    fn parse_if(&mut self) -> Expr {
        let start = self.start_span();
        let _if = self.expect(TokenKind::If);

        let cond = Box::new(self.parse_cond());
        let _then = self.expect(TokenKind::Then);
        let then_body = self.parse_and_alloc_expr();

        let mut elif_chains = Vec::new();
        while self.at(TokenKind::Elif) {
            let elif_start = self.start_span();
            let _elif = self.advance();
            let elif_cond = Box::new(self.parse_cond());
            let elif_guard = self.parse_optional_guard();
            let _then = self.expect(TokenKind::Then);
            let elif_body = self.parse_and_alloc_expr();
            elif_chains.push(ElifChain {
                cond: elif_cond,
                guard: elif_guard,
                body: elif_body,
                span: self.finish_span(elif_start),
            });
        }

        let else_body = self.parse_option(TokenKind::Else, |p| p.parse_and_alloc_expr());

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            span: self.finish_span(start),
        }
    }

    // ── Cond ────────────────────────────────────────────────────────────

    fn parse_cond(&mut self) -> Cond {
        if self.at(TokenKind::Case) {
            let start = self.start_span();
            let _case = self.advance();
            let kind = self.parse_bind_kind();
            let pat = self.parse_pat();
            let _bind = self.expect(TokenKind::ColonEq);
            let init = self.parse_and_alloc_expr();
            Cond::Case {
                kind,
                pat,
                init,
                span: self.finish_span(start),
            }
        } else {
            Cond::Expr(self.parse_and_alloc_expr())
        }
    }

    // ── Match ───────────────────────────────────────────────────────────

    fn parse_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.expect(TokenKind::Match);
        let scrutinee = self.parse_and_alloc_expr();
        let _with = self.expect(TokenKind::With);

        let mut arms = Vec::new();
        loop {
            // Collect optional attrs before checking for `case`
            let attrs = self.maybe_attrs();
            if !self.at(TokenKind::Case) {
                break;
            }
            let arm_start = self.start_span();
            let _case = self.advance();
            let pat = self.parse_pat();
            let guard = self.parse_optional_guard();
            let _arrow = self.expect(TokenKind::EqGt);
            let body = self.parse_and_alloc_expr();
            arms.push(MatchArm {
                attrs,
                pat,
                guard,
                body,
                span: self.finish_span(arm_start),
            });
        }

        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    // ── Return / Break / Cycle / Defer ──────────────────────────────────

    fn parse_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.expect(TokenKind::Return);
        let value = self.parse_optional_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_break(&mut self) -> Expr {
        let start = self.start_span();
        let _brk = self.expect(TokenKind::Break);
        // Optional value expression
        let value = self.parse_optional_expr();
        Expr::Break {
            label: None,
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_cycle(&mut self) -> Expr {
        let start = self.start_span();
        let _cyc = self.expect(TokenKind::Cycle);
        let label = self.optional_ident();
        let guard = self.parse_optional_guard();
        Expr::Cycle {
            label,
            guard,
            span: self.finish_span(start),
        }
    }

    fn parse_defer(&mut self) -> Expr {
        let start = self.start_span();
        let _def = self.expect(TokenKind::Defer);
        let body = self.parse_and_alloc_expr();
        Expr::Defer {
            body,
            span: self.finish_span(start),
        }
    }

    // ── Import ──────────────────────────────────────────────────────────

    fn parse_import(&mut self) -> Expr {
        let start = self.start_span();
        let _imp = self.expect(TokenKind::Import);

        let items = if self.eat(TokenKind::Star) {
            ImportClause::Glob
        } else {
            let list = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
                let item_start = p.start_span();
                let name = p.expect_symbol();
                let alias = p.parse_option(TokenKind::As, |p2| p2.expect_symbol());
                ImportItem {
                    name,
                    alias,
                    span: p.finish_span(item_start),
                }
            });
            ImportClause::Items(list)
        };

        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Import {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    // ── Constructs with attrs/modifiers prefix ──────────────────────────

    fn parse_with_prefix(&mut self) -> Expr {
        let attrs = self.maybe_attrs();
        let modifiers = self.parse_modifiers();

        match self.peek_kind() {
            TokenKind::Fn => self.parse_fn_expr(attrs, modifiers),
            TokenKind::Record => self.parse_record(attrs, modifiers),
            TokenKind::Choice => self.parse_choice(attrs, modifiers),
            TokenKind::Const | TokenKind::Var => self.parse_bind(attrs, modifiers),
            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Label => self.parse_label(),
            _ => self.error_expr("expected declaration or loop after attributes/modifiers"),
        }
    }

    fn maybe_attrs(&mut self) -> Vec<Attr> {
        if self.at(TokenKind::Hash) && self.peek2() == TokenKind::LBracket {
            let _hash = self.expect(TokenKind::Hash);
            self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, |p| p.parse_attr())
        } else {
            Vec::new()
        }
    }

    fn parse_attr(&mut self) -> Attr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LParen) {
            let list = self.parse_separated_list(TokenKind::RParen, |p| p.parse_attr_arg());
            let _rp = self.expect(TokenKind::RParen);
            list
        } else {
            Vec::new()
        };
        Attr {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_attr_arg(&mut self) -> AttrArg {
        let start = self.start_span();
        // Named: ident [":=" lit]
        if self.at(TokenKind::Ident) {
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_lit_value())
            } else {
                None
            };
            return AttrArg::Named {
                name,
                value,
                span: self.finish_span(start),
            };
        }
        // Literal argument
        let lit = self.parse_lit_value();
        AttrArg::Lit(lit, self.finish_span(start))
    }

    fn parse_modifiers(&mut self) -> Vec<Modifier> {
        let mut mods = Vec::new();
        loop {
            if self.eat(TokenKind::Export) {
                mods.push(Modifier::Export);
            } else if self.eat(TokenKind::Opaque) {
                mods.push(Modifier::Opaque);
            } else if self.eat(TokenKind::Native) {
                let abi = if self.at(TokenKind::StringLit) {
                    Some(self.expect_symbol())
                } else {
                    None
                };
                mods.push(Modifier::Native(abi));
            } else {
                break;
            }
        }
        mods
    }

    // ── fn (named def or lambda) ────────────────────────────────────────

    fn parse_fn_expr(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _fn = self.expect(TokenKind::Fn);

        // LL(1): if next is Ident or Underscore → named FnDef, else Lambda
        if self.at(TokenKind::Ident) || self.at(TokenKind::Underscore) {
            // Named function definition
            let name = self.expect_symbol();
            let ty_params = self.maybe_ty_params();
            let params = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_param());
            let ret_ty = self.parse_option(TokenKind::Colon, |p| p.parse_ty());
            let body = if self.at(TokenKind::LParen) {
                Some(self.parse_alloc_block())
            } else {
                None
            };
            Expr::FnDef {
                attrs,
                modifiers,
                name,
                ty_params,
                params,
                ret_ty,
                body,
                span: self.finish_span(start),
            }
        } else {
            // Lambda: fn [ty_params] (params) [: ret_ty] => expr
            let ty_params = self.maybe_ty_params();
            let params = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_param());
            let ret_ty = self.parse_option(TokenKind::Colon, |p| p.parse_ty());
            let _arrow = self.expect(TokenKind::EqGt);
            let body = self.parse_and_alloc_expr();
            Expr::Lambda {
                attrs,
                ty_params,
                params,
                ret_ty,
                body,
                span: self.finish_span(start),
            }
        }
    }

    fn parse_param(&mut self) -> Param {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, |p| p.parse_ty());
        Param {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    // ── Record ──────────────────────────────────────────────────────────

    fn parse_record(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _rec = self.expect(TokenKind::Record);
        let name = self.optional_ident();
        let ty_params = self.maybe_ty_params();
        let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| p.parse_rec_field());
        Expr::Record {
            attrs,
            modifiers,
            name,
            ty_params,
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_rec_field(&mut self) -> RecField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, |p| p.parse_ty());
        RecField {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    // ── Choice ──────────────────────────────────────────────────────────

    fn parse_choice(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _choice = self.expect(TokenKind::Choice);
        let name = self.optional_ident();
        let ty_params = self.maybe_ty_params();
        let _lb = self.expect(TokenKind::LBrace);
        let variants = self.parse_pipe_separated(TokenKind::RBrace, |p| p.parse_choice_variant());
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Choice {
            attrs,
            modifiers,
            name,
            ty_params,
            variants,
            span: self.finish_span(start),
        }
    }

    fn parse_choice_variant(&mut self) -> ChoiceVariant {
        let start = self.start_span();
        let attrs = self.maybe_attrs();
        let name = self.expect_symbol();
        let payload = match self.peek_kind() {
            TokenKind::LParen => {
                let tys = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_ty());
                Some(VariantPayload::Positional(tys))
            }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| p.parse_rec_field());
                Some(VariantPayload::Named(fields))
            }
            TokenKind::ColonEq => {
                let _ceq = self.advance();
                let lit = self.parse_lit_value();
                Some(VariantPayload::Discriminant(lit))
            }
            _ => None,
        };
        ChoiceVariant {
            attrs,
            name,
            payload,
            span: self.finish_span(start),
        }
    }

    // ── Bind (const/var) ────────────────────────────────────────────────

    fn parse_bind(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let kind = self.parse_bind_kind();
        let pat = self.parse_pat();
        let ty = self.parse_option(TokenKind::Colon, |p| p.parse_ty());
        let init = self.parse_option(TokenKind::ColonEq, |p| p.parse_and_alloc_expr());
        Expr::Bind {
            attrs,
            modifiers,
            kind,
            pat,
            ty,
            init,
            span: self.finish_span(start),
        }
    }

    // ── While / Loop / For / Label ──────────────────────────────────────

    fn parse_while(&mut self) -> Expr {
        let start = self.start_span();
        let _while = self.expect(TokenKind::While);
        let cond = Box::new(self.parse_cond());
        let guard = self.parse_optional_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::While {
            cond,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_loop(&mut self) -> Expr {
        let start = self.start_span();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        let post_cond = self.parse_option(TokenKind::While, |p| Box::new(p.parse_cond()));
        Expr::Loop {
            body,
            post_cond,
            span: self.finish_span(start),
        }
    }

    fn parse_for(&mut self) -> Expr {
        let start = self.start_span();
        let _for = self.expect(TokenKind::For);
        let pat = self.parse_pat();
        let _in = self.expect(TokenKind::In);
        let iter = self.parse_and_alloc_expr();
        let guard = self.parse_optional_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::For {
            pat,
            iter,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_label(&mut self) -> Expr {
        let start = self.start_span();
        let _label = self.expect(TokenKind::Label);
        let name = self.expect_symbol();
        let body = self.parse_alloc_block();
        Expr::Label {
            name,
            body,
            span: self.finish_span(start),
        }
    }

    // ── Block: ( stmts; ... [tail] ) ────────────────────────────────────

    fn parse_block(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.expect(TokenKind::LParen);
        self.parse_block_tail(Vec::new(), start)
    }

    // ── Type parsing ────────────────────────────────────────────────────

    fn parse_ty(&mut self) -> Ty {
        let start = self.start_span();
        let first = self.parse_ty_noarrow();

        // Check for function type: T1 -> T2
        if self.at(TokenKind::MinusGt) {
            let mut params = vec![first];
            while self.eat(TokenKind::MinusGt) {
                params.push(self.parse_ty_noarrow());
            }
            // The last element is the return type
            let ret = Box::new(params.pop().expect("at least one parsed"));
            return Ty::Arrow {
                params,
                ret,
                span: self.finish_span(start),
            };
        }
        first
    }

    fn parse_ty_noarrow(&mut self) -> Ty {
        match self.peek_kind() {
            TokenKind::TyIdent => {
                let start = self.start_span();
                let name = self.expect_symbol();
                Ty::Var {
                    name,
                    span: self.finish_span(start),
                }
            }
            TokenKind::Ident => {
                let start = self.start_span();
                let name = self.expect_symbol();
                let args = if self.eat(TokenKind::LBracket) {
                    let tys = self.parse_separated_list(TokenKind::RBracket, |p| p.parse_ty());
                    let _rb = self.expect(TokenKind::RBracket);
                    tys
                } else {
                    Vec::new()
                };
                Ty::Named {
                    name,
                    args,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LParen => {
                let start = self.start_span();
                let elements = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_ty());
                Ty::Prod {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBracket => {
                let start = self.start_span();
                let _lb = self.advance();
                let size = if self.at(TokenKind::IntLit) {
                    let lit_expr = self.parse_lit();
                    Some(self.alloc_expr(lit_expr))
                } else {
                    None
                };
                let _rb = self.expect(TokenKind::RBracket);
                let element = Box::new(self.parse_ty());
                Ty::Arr {
                    element,
                    size,
                    span: self.finish_span(start),
                }
            }
            _ => {
                let span = self.peek().span;
                let _diag = self.diags.error("expected type expression", span, self.file_id);
                Ty::Error {
                    span: self.finish_span(span.start),
                }
            }
        }
    }

    fn maybe_ty_params(&mut self) -> Vec<TyParam> {
        // Type params use `[` with `'T` inside — truly LL(1): `[` alone is enough.
        //   fn name['T, 'U](params) ...
        //   choice Foo['T] { ... }
        // The grammar says `ast_ty_params = "[", [ty_param_list], "]"`.
        if !self.at(TokenKind::LBracket) {
            return Vec::new();
        }
        self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, |p| {
            let start = p.start_span();
            let name = p.expect_symbol();
            TyParam {
                name,
                bounds: Vec::new(),
                span: p.finish_span(start),
            }
        })
    }

    // ── Pattern parsing ─────────────────────────────────────────────────

    fn parse_pat(&mut self) -> Pat {
        let start = self.start_span();
        let mut primary = self.parse_pat_primary();

        // Check for `or` alternatives
        if self.at(TokenKind::Or) {
            let mut alternatives = vec![primary];
            while self.eat(TokenKind::Or) {
                alternatives.push(self.parse_pat_primary());
            }
            primary = Pat::Or {
                alternatives,
                span: self.finish_span(start),
            };
        }
        primary
    }

    fn parse_pat_primary(&mut self) -> Pat {
        match self.peek_kind() {
            TokenKind::Ident => self.parse_pat_ident(),
            TokenKind::Underscore => {
                let start = self.start_span();
                let _us = self.advance();
                Pat::Wild {
                    span: self.finish_span(start),
                }
            }
            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::CharLit => {
                let start = self.start_span();
                let value = self.parse_lit_value();
                Pat::Lit {
                    value,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LParen => {
                let start = self.start_span();
                let elements = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_pat());
                Pat::Prod {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBracket => {
                let start = self.start_span();
                let elements = self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, |p| p.parse_pat());
                Pat::Arr {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBrace => {
                let start = self.start_span();
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| p.parse_pat_field());
                Pat::AnonRec {
                    fields,
                    span: self.finish_span(start),
                }
            }
            _ => {
                let span = self.peek().span;
                let _diag = self.diags.error("expected pattern", span, self.file_id);
                self.recover();
                Pat::Error {
                    span: self.finish_span(span.start),
                }
            }
        }
    }

    fn parse_pat_ident(&mut self) -> Pat {
        let start = self.start_span();
        let name = self.expect_symbol();
        match self.peek_kind() {
            // Positional sum pattern: Name(p1, p2)
            TokenKind::LParen => {
                let args = self.parse_delimited(TokenKind::LParen, TokenKind::RParen, |p| p.parse_pat());
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Positional { args, span }),
                    span,
                }
            }
            // Named-field pattern: Name{ f: p }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| p.parse_pat_field());
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Named { fields, span }),
                    span,
                }
            }
            // Bare ident — variable binding
            _ => Pat::Ident {
                name,
                suffix: None,
                span: self.finish_span(start),
            },
        }
    }

    fn parse_pat_field(&mut self) -> PatField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let pat = self.parse_option(TokenKind::Colon, |p| p.parse_pat());
        PatField {
            attrs,
            mutable,
            name,
            pat,
            span: self.finish_span(start),
        }
    }

    // ── Shared helpers ──────────────────────────────────────────────────

    /// Parses a comma-separated list of items until `closing` is the next token.
    /// Handles a trailing comma before `closing` gracefully.
    fn parse_separated_list<T, F>(&mut self, closing: TokenKind, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut items = Vec::new();
        if self.at(closing) {
            return items;
        }
        loop {
            items.push(f(self));
            if !self.eat(TokenKind::Comma) {
                break;
            }
            if self.at(closing) {
                break; // trailing comma
            }
        }
        items
    }

    /// Consumes `open`, parses a comma-separated list until `close`, then
    /// consumes `close`.  Equivalent to `expect(open) + parse_separated_list
    /// + expect(close)` but expressed in one call.
    fn parse_delimited<T, F>(&mut self, open: TokenKind, close: TokenKind, f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let _open = self.expect(open);
        let items = self.parse_separated_list(close, f);
        let _close = self.expect(close);
        items
    }

    /// Parses an expression and immediately arena-allocates it.
    fn parse_and_alloc_expr(&mut self) -> Idx<Expr> {
        let e = self.parse_expr();
        self.alloc_expr(e)
    }

    /// Parses a block (`(stmts; ... [tail])`) and arena-allocates it.
    fn parse_alloc_block(&mut self) -> Idx<Expr> {
        let b = self.parse_block();
        self.alloc_expr(b)
    }

    /// Parses block statements + optional tail until `)`, then the closing `)`.
    /// `stmts` holds any statements already parsed before entering this helper.
    fn parse_block_tail(&mut self, mut stmts: Vec<Idx<Expr>>, start: u32) -> Expr {
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let e = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                let idx = self.alloc_expr(e);
                stmts.push(idx);
            } else {
                // tail expression
                let tail = self.alloc_expr(e);
                let _rp = self.expect(TokenKind::RParen);
                return Expr::Block {
                    stmts,
                    tail: Some(tail),
                    span: self.finish_span(start),
                };
            }
        }
        let _rp = self.expect(TokenKind::RParen);
        Expr::Block {
            stmts,
            tail: None,
            span: self.finish_span(start),
        }
    }

    /// Parses an optional expression (if the next token can start one) and
    /// arena-allocates it.  Returns `None` when no expression follows.
    fn parse_optional_expr(&mut self) -> Option<Idx<Expr>> {
        if can_start_expr(self.peek_kind()) {
            Some(self.parse_and_alloc_expr())
        } else {
            None
        }
    }

    /// Parses an optional clause: if `trigger` is present, consumes it and
    /// calls `f`; otherwise returns `None`.
    fn parse_option<T, F>(&mut self, trigger: TokenKind, mut f: F) -> Option<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        if self.eat(trigger) {
            Some(f(self))
        } else {
            None
        }
    }

    /// Parses an optional `if guard` expression, allocating the result.
    fn parse_optional_guard(&mut self) -> Option<Idx<Expr>> {
        self.parse_option(TokenKind::If, |p| p.parse_and_alloc_expr())
    }

    /// Parses a pipe-separated list of items until `closing` is the next
    /// token.  Accepts an optional leading `|`.
    fn parse_pipe_separated<T, F>(&mut self, closing: TokenKind, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut items = Vec::new();
        if self.at(closing) {
            return items;
        }
        // consume optional leading pipe
        let _ = self.eat(TokenKind::Pipe);
        loop {
            items.push(f(self));
            if !self.eat(TokenKind::Pipe) {
                break;
            }
            if self.at(closing) {
                break;
            }
        }
        items
    }

    /// Wraps `base` in a [`Expr::Postfix`] node, computing the outer span from
    /// `start`.  The caller is responsible for computing any per-op inner span
    /// before passing `op`.
    fn wrap_postfix(&mut self, base: Idx<Expr>, op: PostfixOp, start: u32) -> Expr {
        Expr::Postfix {
            base,
            op,
            span: self.finish_span(start),
        }
    }

    /// Parses `const` or `var`, returning the corresponding [`BindKind`].
    fn parse_bind_kind(&mut self) -> BindKind {
        if self.eat(TokenKind::Const) {
            BindKind::Const
        } else {
            let _var = self.expect(TokenKind::Var);
            BindKind::Var
        }
    }

    /// Returns the next identifier's symbol if the current token is an `Ident`,
    /// consuming it; otherwise returns `None` without advancing.
    fn optional_ident(&mut self) -> Option<Symbol> {
        if self.at(TokenKind::Ident) {
            Some(self.expect_symbol())
        } else {
            None
        }
    }

    /// Parses the common header shared by parameters, record fields, and
    /// pattern fields: optional attrs, optional `var`, then an identifier.
    /// Returns `(start, attrs, mutable, name)`.
    fn parse_field_header(&mut self) -> (u32, Vec<Attr>, bool, Symbol) {
        let start = self.start_span();
        let attrs = self.maybe_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        (start, attrs, mutable, name)
    }

    /// Expects an `Ident`, `TyIdent`, `IntLit`, or `StringLit` token and
    /// returns its [`Symbol`].  For `Underscore` (no interned symbol), returns
    /// a sentinel `Symbol(u32::MAX)`.
    fn expect_symbol(&mut self) -> Symbol {
        let tok = self.advance().clone();
        if let Some(sym) = tok.symbol {
            return sym;
        }
        // Underscore has no interned symbol — return sentinel; other tokens are errors
        if tok.kind != TokenKind::Underscore {
            let _diag = self.diags.error("expected identifier", tok.span, self.file_id);
        }
        Symbol(u32::MAX)
    }

    fn parse_lit_value(&mut self) -> LitValue {
        let tok = self.advance().clone();
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
        match tok.kind {
            TokenKind::IntLit => LitValue::Int(parse_int_lit(self.resolve(sym))),
            TokenKind::FloatLit => {
                LitValue::Float(self.resolve(sym).replace('_', "").parse::<f64>().unwrap_or(0.0))
            }
            TokenKind::StringLit => LitValue::Str(sym),
            TokenKind::CharLit => LitValue::Char(parse_char_lit(self.resolve(sym))),
            _ => {
                let _diag = self.diags.error("expected literal", tok.span, self.file_id);
                LitValue::Bool(false)
            }
        }
    }

    /// Resolves a [`Symbol`] to its interned text.
    #[must_use]
    fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    /// Parses a comma-separated expression list until `closing`.
    fn parse_expr_list(&mut self, closing: TokenKind) -> Vec<Idx<Expr>> {
        self.parse_separated_list(closing, |p| p.parse_and_alloc_expr())
    }
}

// ── Free helpers ────────────────────────────────────────────────────────────

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
        TokenKind::DocComment => "doc comment",
        TokenKind::Eof => "end of file",
        TokenKind::Error => "error token",
        _ => "token",
    })
}

/// Returns `true` if `kind` can start an expression (used for optional expr
/// parsing in `return`, `break`, etc.).
#[must_use]
#[allow(clippy::missing_const_for_fn)] // matches! is not const-compatible
fn can_start_expr(kind: TokenKind) -> bool {
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
            | TokenKind::Native
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
        (h, 16)
    } else if let Some(o) = s.strip_prefix("0o").or_else(|| s.strip_prefix("0O")) {
        (o, 8)
    } else if let Some(b) = s.strip_prefix("0b").or_else(|| s.strip_prefix("0B")) {
        (b, 2)
    } else {
        (s, 10)
    }
}

/// Parses the text of a character literal (`'x'` or `'\n'`) into a `char`.
fn parse_char_lit(text: &str) -> char {
    let inner = text.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')).unwrap_or(text);
    if let Some(esc) = inner.strip_prefix('\\') {
        match esc.as_bytes().first() {
            Some(b'n') => '\n',
            Some(b't') => '\t',
            Some(b'r') => '\r',
            Some(b'0') => '\0',
            Some(b'\\') => '\\',
            Some(b'\'') => '\'',
            _ => inner.chars().next().unwrap_or('\0'),
        }
    } else {
        inner.chars().next().unwrap_or('\0')
    }
}

// ── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests;
