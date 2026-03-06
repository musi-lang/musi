//! LL(1) + Pratt parser for the Musi language.
//!
//! Produces a typed AST from a flat [`Token`] slice.  All recursive children are
//! arena-allocated via [`AstArenas`]; clients hold [`Idx`] handles.

use core::mem;

use musi_lex::token::{Token, TokenKind};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Span, Symbol};

use crate::ast::{
    ArrayItem, AstArenas, Attr, AttrArg, BinOp, BindKind, ChoiceVariant, ClassMember, Cond,
    Constraint, ElifBranch, ExportItem, Expr, FieldInit, ImportClause, ImportItem, LitValue,
    MatchArm, Modifier, Param, ParsedModule, Pat, PatField, PatSuffix, PostfixOp, PrefixOp,
    RecField, Ty, TyParam, VariantPayload,
};

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

    #[must_use]
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
            let got = self.peek();
            let got_text = kind_text(got.kind);
            let exp_text = kind_text(kind);
            let _diag = self.diags.error(
                format!("expected {exp_text}, found {got_text}"),
                got.span,
                self.file_id,
            );
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
        let mut items = Vec::new();
        while !self.at(TokenKind::Eof) {
            let pos_before = self.pos;
            items.push(self.parse_and_alloc_expr());
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
        let ctx = mem::take(&mut self.ctx);
        ParsedModule { items, ctx, span }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_pratt(0)
    }

    fn parse_pratt(&mut self, min_bp: u8) -> Expr {
        let start = self.start_span();

        let mut lhs = self.parse_unary_expr();
        lhs = self.parse_postfix_chain(lhs, start);

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

    /// Returns `(left_bp, right_bp, kind)` for the current token if it is an
    /// infix operator, or `None` otherwise.
    #[must_use]
    fn infix_info(&self) -> Option<(u8, u8, InfixKind)> {
        use BinOp as B;
        use InfixKind::{Assign, Binary};
        use TokenKind as T;

        let kind = self.peek_kind();
        let (l, r, op) = match kind {
            // BP 10 -- assign (right-assoc)
            T::LtMinus => (10, 9, Assign),
            // BP 20 -- or / xor (left-assoc)
            T::Or => (20, 21, Binary(B::Or)),
            T::Xor => (20, 21, Binary(B::Xor)),
            // BP 30 -- and (left-assoc)
            T::And => (30, 31, Binary(B::And)),
            // BP 40 -- equality (non-assoc)
            T::Eq => (40, 41, Binary(B::Eq)),
            T::SlashEq => (40, 41, Binary(B::NotEq)),
            // BP 50 -- comparison (non-assoc)
            T::Lt => (50, 51, Binary(B::Lt)),
            T::Gt => (50, 51, Binary(B::Gt)),
            T::LtEq => (50, 51, Binary(B::LtEq)),
            T::GtEq => (50, 51, Binary(B::GtEq)),
            T::In => (50, 51, Binary(B::In)),
            // BP 60 -- range (non-assoc)
            T::DotDot => (60, 61, Binary(B::Range)),
            T::DotDotLt => (60, 61, Binary(B::RangeExcl)),
            // BP 70 -- cons (left-assoc)
            T::ColonColon => (70, 71, Binary(B::Cons)),
            // BP 80 -- bitwise or / xor (left-assoc)
            T::Pipe => (80, 81, Binary(B::BitOr)),
            T::Caret => (80, 81, Binary(B::BitXor)),
            // BP 90 -- bitwise and (left-assoc)
            T::Amp => (90, 91, Binary(B::BitAnd)),
            // BP 100 -- shift (left-assoc)
            T::Shl => (100, 101, Binary(B::Shl)),
            T::Shr => (100, 101, Binary(B::Shr)),
            // BP 110 -- additive (left-assoc)
            T::Plus => (110, 111, Binary(B::Add)),
            T::Minus => (110, 111, Binary(B::Sub)),
            // BP 120 -- multiplicative (left-assoc)
            T::Star => (120, 121, Binary(B::Mul)),
            T::Slash => (120, 121, Binary(B::Div)),
            T::Percent => (120, 121, Binary(B::Rem)),
            _ => return None,
        };
        Some((l, r, op))
    }

    fn parse_postfix_chain(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            match self.peek_kind() {
                // Call: f(args)
                TokenKind::LParen => {
                    let _lp = self.advance();
                    let args = self.parse_expr_list(TokenKind::RParen);
                    let _rp = self.expect(TokenKind::RParen);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::Call {
                            args,
                            span: self.finish_span(start),
                        },
                        start,
                    );
                }
                // Index: e.[args]
                TokenKind::DotLBracket => {
                    let _dlb = self.advance();
                    let args = self.parse_expr_list(TokenKind::RBracket);
                    let _rb = self.expect(TokenKind::RBracket);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::Index {
                            args,
                            span: self.finish_span(start),
                        },
                        start,
                    );
                }
                // RecDot: e.{ fields }
                TokenKind::DotLBrace => {
                    let _dlb = self.advance();
                    let fields =
                        self.parse_separated_list(TokenKind::RBrace, Parser::parse_rec_lit_field);
                    let _rb = self.expect(TokenKind::RBrace);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::RecDot {
                            fields,
                            span: self.finish_span(start),
                        },
                        start,
                    );
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
                            lhs = self.wrap_postfix(
                                base,
                                PostfixOp::Field { name, span: f_span },
                                start,
                            );
                        }
                        _ => {
                            let _err = self.diags.error(
                                "expected field name",
                                self.peek().span,
                                self.file_id,
                            );
                            break;
                        }
                    }
                }
                // As cast: e as T
                TokenKind::As => {
                    let _as_kw = self.advance();
                    let ty = self.parse_ty();
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::As {
                            ty,
                            span: self.finish_span(start),
                        },
                        start,
                    );
                }
                _ => break,
            }
        }
        lhs
    }

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

    fn parse_unary_expr(&mut self) -> Expr {
        match self.peek_kind() {
            TokenKind::Minus => self.parse_prefix(PrefixOp::Neg),
            TokenKind::Not => self.parse_prefix(PrefixOp::Not),
            TokenKind::Bang => self.parse_prefix(PrefixOp::Deref),
            TokenKind::At => self.parse_prefix(PrefixOp::AddrOf),
            TokenKind::Tilde => self.parse_prefix(PrefixOp::BitNot),

            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::CharLit => {
                self.parse_lit()
            }

            TokenKind::Ident => {
                let start = self.start_span();
                let tok = self.advance().clone();
                let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
                Expr::Ident {
                    name: sym,
                    span: self.finish_span(start),
                }
            }

            TokenKind::LParen => self.parse_expr_paren(),

            TokenKind::LBracket => self.parse_array_lit(),

            TokenKind::DotLBrace => self.parse_anon_rec_lit(),

            TokenKind::If => self.parse_if(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Cycle => self.parse_cycle(),
            TokenKind::Defer => self.parse_defer(),
            TokenKind::Import => self.parse_import(),

            // export { ... } from "path" -- re-export; export fn/record/... -- modifier
            TokenKind::Export => {
                if self.peek2() == TokenKind::LBrace {
                    self.parse_export()
                } else {
                    self.parse_expr_after_attrs()
                }
            }

            TokenKind::Using => self.parse_using(),

            TokenKind::Hash | TokenKind::Opaque | TokenKind::Native => {
                self.parse_expr_after_attrs()
            }
            TokenKind::Fn => self.parse_fn_expr(Vec::new(), Vec::new()),
            TokenKind::Record => self.parse_record(Vec::new(), Vec::new()),
            TokenKind::Choice => self.parse_choice(Vec::new(), Vec::new()),
            TokenKind::Const | TokenKind::Var => self.parse_bind(Vec::new(), Vec::new()),

            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Label => self.parse_label(),

            TokenKind::Class => self.parse_class_def(),
            TokenKind::Given => self.parse_given_def(),

            _ => self.error_expr("unexpected token"),
        }
    }

    fn parse_lit(&mut self) -> Expr {
        let start = self.start_span();
        let value = self.parse_lit_value();
        Expr::Lit {
            value,
            span: self.finish_span(start),
        }
    }

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
                elements.extend(
                    self.parse_separated_list(TokenKind::RParen, Parser::parse_and_alloc_expr),
                );
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

    fn parse_anon_rec_lit(&mut self) -> Expr {
        let start = self.start_span();
        let _dlb = self.expect(TokenKind::DotLBrace);
        let fields = self.parse_separated_list(TokenKind::RBrace, Parser::parse_rec_lit_field);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::AnonRec {
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_rec_lit_field(&mut self) -> FieldInit {
        let start = self.start_span();

        // Spread: <.. expr
        if self.eat(TokenKind::LtDotDot) {
            let idx = self.parse_and_alloc_expr();
            return FieldInit::Spread {
                expr: idx,
                span: self.finish_span(start),
            };
        }

        // field_base: [attrs] [var] name [:= expr]
        // Shorthand (no :=): { x } desugars to { x := x }
        let attrs = self.parse_opt_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        let value = if self.eat(TokenKind::ColonEq) {
            self.parse_and_alloc_expr()
        } else {
            // Shorthand: synthesize Expr::Ident with same name
            let ident_span = self.finish_span(start);
            self.alloc_expr(Expr::Ident {
                name,
                span: ident_span,
            })
        };
        FieldInit::Named {
            attrs,
            mutable,
            name,
            value,
            span: self.finish_span(start),
        }
    }

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
            let elif_guard = self.parse_opt_guard();
            let _then = self.expect(TokenKind::Then);
            let elif_body = self.parse_and_alloc_expr();
            elif_chains.push(ElifBranch {
                cond: elif_cond,
                guard: elif_guard,
                body: elif_body,
                span: self.finish_span(elif_start),
            });
        }

        let else_body = self.parse_option(TokenKind::Else, Parser::parse_and_alloc_expr);

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            span: self.finish_span(start),
        }
    }

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

    fn parse_match_arm(&mut self) -> MatchArm {
        let arm_start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let pat = self.parse_pat();
        let guard = self.parse_opt_guard();
        let _arrow = self.expect(TokenKind::EqGt);
        let body = self.parse_alloc_arm_body();
        MatchArm {
            attrs,
            pat,
            guard,
            body,
            span: self.finish_span(arm_start),
        }
    }

    fn parse_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.expect(TokenKind::Match);
        let scrutinee = self.parse_and_alloc_expr();
        let _with = self.expect(TokenKind::With);
        let _lp = self.expect(TokenKind::LParen);

        let mut arms = vec![self.parse_match_arm()];
        while self.eat(TokenKind::Pipe) {
            arms.push(self.parse_match_arm());
        }

        let _rp = self.expect(TokenKind::RParen);
        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    fn parse_alloc_arm_body(&mut self) -> Idx<Expr> {
        let expr = self.parse_arm_body();
        self.alloc_expr(expr)
    }

    /// Like `parse_pratt(0)` but stops before a top-level `|` (Pipe),
    /// which serves as the arm separator in match expressions.
    fn parse_arm_body(&mut self) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_unary_expr();
        lhs = self.parse_postfix_chain(lhs, start);
        loop {
            if self.at(TokenKind::Pipe) {
                break;
            }
            let Some((_l_bp, r_bp, op_kind)) = self.infix_info() else {
                break;
            };
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

    fn parse_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.expect(TokenKind::Return);
        let value = self.parse_opt_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_break(&mut self) -> Expr {
        let start = self.start_span();
        let _brk = self.expect(TokenKind::Break);
        // Optional value expression
        let value = self.parse_opt_expr();
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
        let guard = self.parse_opt_guard();
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

    fn parse_import(&mut self) -> Expr {
        let start = self.start_span();
        let _imp = self.expect(TokenKind::Import);

        let items = if self.eat(TokenKind::Star) {
            if self.eat(TokenKind::As) {
                let name = self.expect_symbol();
                ImportClause::GlobAs(name)
            } else {
                ImportClause::Glob
            }
        } else {
            let list = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
                let item_start = p.start_span();
                let name = p.expect_symbol();
                let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
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

    fn parse_export(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.expect(TokenKind::Export);
        let items = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
            let item_start = p.start_span();
            let name = p.expect_symbol();
            let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
            ExportItem {
                name,
                alias,
                span: p.finish_span(item_start),
            }
        });
        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Export {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    fn parse_using(&mut self) -> Expr {
        let start = self.start_span();
        let _using = self.expect(TokenKind::Using);
        let name = self.expect_symbol();
        let _assign = self.expect(TokenKind::ColonEq);
        let init = self.parse_and_alloc_expr();
        let body = self.parse_alloc_block();
        Expr::Using {
            name,
            init,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_after_attrs(&mut self) -> Expr {
        let attrs = self.parse_opt_attrs();
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
            TokenKind::Class => self.parse_class_def(),
            TokenKind::Given => self.parse_given_def(),
            _ => self.error_expr("expected declaration or loop after attributes/modifiers"),
        }
    }

    fn parse_opt_attrs(&mut self) -> Vec<Attr> {
        if self.at(TokenKind::Hash) && self.peek2() == TokenKind::LBracket {
            let _hash = self.expect(TokenKind::Hash);
            self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, Parser::parse_attr)
        } else {
            Vec::new()
        }
    }

    fn parse_attr(&mut self) -> Attr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LParen) {
            let list = self.parse_separated_list(TokenKind::RParen, Parser::parse_attr_arg);
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
        AttrArg::Value {
            value: lit,
            span: self.finish_span(start),
        }
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

    fn parse_fn_expr(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _fn = self.expect(TokenKind::Fn);

        // LL(1): if next is Ident or Underscore → named FnDef, else Lambda
        if self.at(TokenKind::Ident) || self.at(TokenKind::Underscore) {
            // Named function definition
            let name = self.expect_symbol();
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let body = if self.eat(TokenKind::EqGt) {
                Some(self.parse_and_alloc_expr())
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
                where_clause,
                body,
                span: self.finish_span(start),
            }
        } else {
            // Lambda: fn [ty_params] (params) [: ret_ty] [where ...] => expr
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let _arrow = self.expect(TokenKind::EqGt);
            let body = self.parse_and_alloc_expr();
            Expr::Lambda {
                attrs,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                span: self.finish_span(start),
            }
        }
    }

    fn parse_param(&mut self) -> Param {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        Param {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    fn parse_record(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _rec = self.expect(TokenKind::Record);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_rec_field);
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
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        RecField {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    fn parse_choice(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _choice = self.expect(TokenKind::Choice);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let _lb = self.expect(TokenKind::LBrace);
        let variants = self.parse_pipe_separated(TokenKind::RBrace, Parser::parse_choice_variant);
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
        let attrs = self.parse_opt_attrs();
        let name = self.expect_symbol();
        let payload = match self.peek_kind() {
            TokenKind::LParen => {
                let tys =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_ty);
                Some(VariantPayload::Positional(tys))
            }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_rec_field);
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

    fn parse_bind(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let kind = self.parse_bind_kind();
        let pat = self.parse_pat();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        let init = self.parse_option(TokenKind::ColonEq, Parser::parse_and_alloc_expr);
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

    fn parse_while(&mut self) -> Expr {
        let start = self.start_span();
        let _while = self.expect(TokenKind::While);
        let cond = Box::new(self.parse_cond());
        let guard = self.parse_opt_guard();
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
        let guard = self.parse_opt_guard();
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

    fn parse_class_def(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::Class);
        let name = self.expect_symbol();
        let ty_params = self.parse_opt_ty_params();
        let supers = if self.eat(TokenKind::Satisfies) {
            let mut list = vec![self.parse_ty_named()];
            while self.eat(TokenKind::Comma) {
                list.push(self.parse_ty_named());
            }
            list
        } else {
            Vec::new()
        };
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::ClassDef {
            name,
            ty_params,
            supers,
            members,
            span: self.finish_span(start),
        }
    }

    fn parse_given_def(&mut self) -> Expr {
        let start = self.start_span();
        let _given = self.expect(TokenKind::Given);
        let class_app = self.parse_ty_named();
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::GivenDef {
            class_app,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_named(&mut self) -> Ty {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LBracket) {
            let tys = self.parse_separated_list(TokenKind::RBracket, Parser::parse_ty);
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

    fn parse_opt_where_clause(&mut self) -> Vec<Constraint> {
        if !self.eat(TokenKind::Where) {
            return Vec::new();
        }
        let mut constraints = Vec::new();
        loop {
            let c_start = self.start_span();
            let ty_var = self.expect_symbol();
            let _sat = self.expect(TokenKind::Satisfies);
            let bound = self.parse_ty_named();
            constraints.push(Constraint {
                ty_var,
                bound,
                span: self.finish_span(c_start),
            });
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        constraints
    }

    fn parse_class_body(&mut self) -> Vec<ClassMember> {
        let mut members = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let member = if self.at(TokenKind::Law) {
                let m_start = self.start_span();
                let _law = self.expect(TokenKind::Law);
                let name = self.expect_symbol();
                let params = self.parse_delimited(
                    TokenKind::LParen,
                    TokenKind::RParen,
                    Parser::parse_param,
                );
                let _arrow = self.expect(TokenKind::EqGt);
                let body = self.parse_and_alloc_expr();
                ClassMember::Law {
                    name,
                    params,
                    body,
                    span: self.finish_span(m_start),
                }
            } else {
                let fn_expr = self.parse_fn_expr(Vec::new(), Vec::new());
                let idx = self.alloc_expr(fn_expr);
                ClassMember::Method(idx)
            };
            members.push(member);
            let _semi = self.expect(TokenKind::Semi);
        }
        members
    }

    fn parse_block(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.expect(TokenKind::LParen);
        self.parse_block_tail(Vec::new(), start)
    }

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
                    let tys = self.parse_separated_list(TokenKind::RBracket, Parser::parse_ty);
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
                let elements =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_ty);
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
                let _diag = self
                    .diags
                    .error("expected type expression", span, self.file_id);
                Ty::Error {
                    span: self.finish_span(span.start),
                }
            }
        }
    }

    fn parse_opt_ty_params(&mut self) -> Vec<TyParam> {
        // Type params use `[` with `'T` inside -- truly LL(1): `[` alone is enough.
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
                let elements =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_pat);
                Pat::Prod {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBracket => {
                let start = self.start_span();
                let elements =
                    self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, Parser::parse_pat);
                Pat::Arr {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBrace => {
                let start = self.start_span();
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_pat_field);
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
                let args =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_pat);
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Positional { args, span }),
                    span,
                }
            }
            // Named-field pattern: Name{ f: p }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_pat_field);
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Named { fields, span }),
                    span,
                }
            }
            // Bare ident -- variable binding
            _ => Pat::Ident {
                name,
                suffix: None,
                span: self.finish_span(start),
            },
        }
    }

    fn parse_pat_field(&mut self) -> PatField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let pat = self.parse_option(TokenKind::Colon, Parser::parse_pat);
        PatField {
            attrs,
            mutable,
            name,
            pat,
            span: self.finish_span(start),
        }
    }

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
                break;
            }
        }
        items
    }

    fn parse_delimited<T, F>(&mut self, open: TokenKind, close: TokenKind, f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let _open = self.expect(open);
        let items = self.parse_separated_list(close, f);
        let _close = self.expect(close);
        items
    }

    fn parse_and_alloc_expr(&mut self) -> Idx<Expr> {
        let e = self.parse_expr();
        self.alloc_expr(e)
    }

    fn parse_alloc_block(&mut self) -> Idx<Expr> {
        let b = self.parse_block();
        self.alloc_expr(b)
    }

    fn parse_block_tail(&mut self, mut stmts: Vec<Idx<Expr>>, start: u32) -> Expr {
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let e = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                let idx = self.alloc_expr(e);
                stmts.push(idx);
            } else {
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

    fn parse_opt_expr(&mut self) -> Option<Idx<Expr>> {
        if can_start_expr(self.peek_kind()) {
            Some(self.parse_and_alloc_expr())
        } else {
            None
        }
    }

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

    fn parse_opt_guard(&mut self) -> Option<Idx<Expr>> {
        self.parse_option(TokenKind::If, Parser::parse_and_alloc_expr)
    }

    fn parse_pipe_separated<T, F>(&mut self, closing: TokenKind, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut items = Vec::new();
        if self.at(closing) {
            return items;
        }
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
    fn wrap_postfix(&self, base: Idx<Expr>, op: PostfixOp, start: u32) -> Expr {
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

    fn optional_ident(&mut self) -> Option<Symbol> {
        if self.at(TokenKind::Ident) {
            Some(self.expect_symbol())
        } else {
            None
        }
    }

    fn parse_field_header(&mut self) -> (u32, Vec<Attr>, bool, Symbol) {
        let start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        (start, attrs, mutable, name)
    }

    fn expect_symbol(&mut self) -> Symbol {
        let tok = self.advance().clone();
        if let Some(sym) = tok.symbol {
            return sym;
        }
        if tok.kind != TokenKind::Underscore {
            let _diag = self
                .diags
                .error("expected identifier", tok.span, self.file_id);
        }
        Symbol(u32::MAX)
    }

    fn parse_lit_value(&mut self) -> LitValue {
        let tok = self.advance().clone();
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
        match tok.kind {
            TokenKind::IntLit => LitValue::Int(parse_int_lit(self.resolve(sym))),
            TokenKind::FloatLit => LitValue::Float(
                self.resolve(sym)
                    .replace('_', "")
                    .parse::<f64>()
                    .unwrap_or(0.0),
            ),
            TokenKind::StringLit => LitValue::Str(sym),
            TokenKind::CharLit => LitValue::Char(parse_char_lit(self.resolve(sym))),
            _ => {
                let _diag = self.diags.error("expected literal", tok.span, self.file_id);
                LitValue::Int(0)
            }
        }
    }

    #[must_use]
    fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    fn parse_expr_list(&mut self, closing: TokenKind) -> Vec<Idx<Expr>> {
        self.parse_separated_list(closing, Parser::parse_and_alloc_expr)
    }
}

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
