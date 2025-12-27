use musi_ast::{
    Attr, AttrArg, AttrArgs, Attrs, ChoiceCase, ChoiceCaseItem, ChoiceCaseItems, CondId, CondKind,
    ExprId, ExprIds, ExprKind, Field, FnSig, Ident, LitKind, MatchCase, Modifiers, StmtIds,
    StmtKind, TemplatePart,
};
use musi_basic::{
    error::{IntoMusiError, MusiResult},
    span::Span,
};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind, parser::Prec};

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_expr(&mut self) -> MusiResult<ExprId> {
        self.parse_expr_bp(0)
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_expr_block(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let (stmts, expr) =
            self.delimited(TokenKind::LBrace, TokenKind::RBrace, Self::parse_block_body)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Block { stmts, expr }, span))
    }

    /// # Errors
    /// Returns `ParseErrorKind::ExpectedIdent` if current token is not identifier.
    pub fn expect_ident(&mut self) -> MusiResult<Ident> {
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                Ok(id)
            }
            _ => Err(ParseErrorKind::ExpectedIdent.into_musi_error(self.curr_span())),
        }
    }

    pub fn try_ident(&mut self) -> Option<Ident> {
        if let Some(TokenKind::Ident(id)) = self.peek_kind() {
            let _ = self.advance();
            Some(id)
        } else {
            None
        }
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    ///
    /// # Panics
    /// Never panics - `.expect()` is guarded by prior `Prec::prefix`/`Prec::infix` check.
    pub fn parse_expr_bp(&mut self, min_bp: u8) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let mut lhs = self.parse_expr_prefix(start)?;
        loop {
            if let Some(new_lhs) = self.try_parse_postfix(lhs, min_bp)? {
                lhs = new_lhs;
                continue;
            }
            if let Some(new_lhs) = self.try_parse_not_in(lhs, start, min_bp)? {
                lhs = new_lhs;
                continue;
            }
            if let Some(new_lhs) = self.try_parse_infix(lhs, start, min_bp)? {
                lhs = new_lhs;
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn parse_expr_prefix(&mut self, start: Span) -> MusiResult<ExprId> {
        match self.peek_kind().and_then(Prec::prefix) {
            Some(bp) => {
                let op = self.advance().expect("Prec::prefix").kind;
                let operand = self.parse_expr_bp(bp)?;
                let span = start.merge(self.prev_span());
                Ok(self.arena.alloc_expr(ExprKind::Unary { op, operand }, span))
            }
            None => self.parse_expr_primary(),
        }
    }

    fn try_parse_postfix(&mut self, lhs: ExprId, min_bp: u8) -> MusiResult<Option<ExprId>> {
        if let Some(l_bp) = self.peek_kind().and_then(Prec::postfix)
            && l_bp >= min_bp
        {
            return Ok(Some(self.parse_expr_postfix(lhs)?));
        }
        Ok(None)
    }

    fn try_parse_not_in(
        &mut self,
        lhs: ExprId,
        start: Span,
        min_bp: u8,
    ) -> MusiResult<Option<ExprId>> {
        if self.at(TokenKind::KwNot) && self.peek_nth(1) == Some(TokenKind::KwIn) {
            let (l_bp, r_bp) = Prec::infix(TokenKind::KwIn).expect("`in` is infix");
            if l_bp >= min_bp {
                self.advance_by(2);
                let rhs = self.parse_expr_bp(r_bp)?;
                let span = start.merge(self.prev_span());
                let inner = self.arena.alloc_expr(
                    ExprKind::Binary {
                        op: TokenKind::KwIn,
                        lhs,
                        rhs,
                    },
                    span,
                );
                return Ok(Some(self.arena.alloc_expr(
                    ExprKind::Unary {
                        op: TokenKind::KwNot,
                        operand: inner,
                    },
                    span,
                )));
            }
        }
        Ok(None)
    }

    fn try_parse_infix(
        &mut self,
        lhs: ExprId,
        start: Span,
        min_bp: u8,
    ) -> MusiResult<Option<ExprId>> {
        let Some((l_bp, r_bp)) = self.peek_kind().and_then(Prec::infix) else {
            return Ok(None);
        };
        if l_bp < min_bp {
            return Ok(None);
        }
        let op = self.advance().expect("Prec::infix").kind;
        self.parse_infix_rhs(op, lhs, r_bp, start).map(Some)
    }

    fn parse_infix_rhs(
        &mut self,
        op: TokenKind,
        lhs: ExprId,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<ExprId> {
        if matches!(op, TokenKind::DotDot | TokenKind::DotDotLt) {
            return self.parse_expr_range(op, lhs, r_bp, start);
        }
        self.parse_expr_binary_or_assign(op, lhs, r_bp, start)
    }

    fn parse_expr_range(
        &mut self,
        op: TokenKind,
        lhs: ExprId,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<ExprId> {
        let end = if self.can_start_expr() {
            Some(self.parse_expr_bp(r_bp)?)
        } else {
            None
        };
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::Range {
                start: lhs,
                end,
                inclusive: op == TokenKind::DotDot,
            },
            span,
        ))
    }

    fn parse_expr_binary_or_assign(
        &mut self,
        op: TokenKind,
        lhs: ExprId,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<ExprId> {
        let rhs = self.parse_expr_bp(r_bp)?;
        let rhs_node = self.arena.exprs.get(rhs);
        let span = start.merge(rhs_node.span);
        if op == TokenKind::LtMinus {
            Ok(self.arena.alloc_expr(
                ExprKind::Assign {
                    target: lhs,
                    value: rhs,
                },
                span,
            ))
        } else {
            Ok(self
                .arena
                .alloc_expr(ExprKind::Binary { op, lhs, rhs }, span))
        }
    }

    fn parse_expr_postfix(&mut self, lhs: ExprId) -> MusiResult<ExprId> {
        let lhs_node = self.arena.exprs.get(lhs);
        let start = lhs_node.span;
        match self.peek_kind() {
            Some(TokenKind::LParen) => self.parse_postfix_call(lhs, start),
            Some(TokenKind::DotCaret) => Ok(self.parse_postfix_deref(lhs, start)),
            Some(TokenKind::Dot) => match self.peek_nth(1) {
                Some(TokenKind::LBrack) => self.parse_postfix_index(lhs, start),
                Some(TokenKind::LBrace) => self.parse_postfix_record(lhs, start),
                _ => self.parse_postfix_field(lhs, start),
            },
            _ => Ok(lhs),
        }
    }

    fn parse_postfix_call(&mut self, lhs: ExprId, start: Span) -> MusiResult<ExprId> {
        let args = self.delimited(TokenKind::LParen, TokenKind::RParen, |p| {
            p.separated(TokenKind::Comma, Self::parse_expr)
        })?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Call { callee: lhs, args }, span))
    }

    fn parse_postfix_index(&mut self, lhs: ExprId, start: Span) -> MusiResult<ExprId> {
        self.advance_by(2); // consume `.` and `[`
        let index = self.parse_expr()?;
        let _ = self.expect(TokenKind::RBrack)?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Index { base: lhs, index }, span))
    }

    fn parse_postfix_field(&mut self, lhs: ExprId, start: Span) -> MusiResult<ExprId> {
        let _ = self.advance(); // consume `.`
        let field = self.expect_ident()?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Field { base: lhs, field }, span))
    }

    fn parse_postfix_deref(&mut self, lhs: ExprId, start: Span) -> ExprId {
        let _ = self.advance(); // consume `.^`
        let span = start.merge(self.prev_span());
        self.arena.alloc_expr(ExprKind::Deref(lhs), span)
    }

    fn parse_expr_primary(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(
                TokenKind::LitInt(_)
                | TokenKind::LitReal(_)
                | TokenKind::LitString(_)
                | TokenKind::LitTemplateNoSubst(_)
                | TokenKind::LitRune(_)
                | TokenKind::KwTrue
                | TokenKind::KwFalse,
            ) => self.parse_expr_lit(),
            Some(TokenKind::TemplateHead(_)) => self.parse_expr_template(),
            Some(TokenKind::Ident(id)) => Ok(self.parse_expr_ident(id)),
            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                self.parse_expr_record_anon()
            }
            Some(TokenKind::LParen) => self.parse_expr_paren(),
            Some(TokenKind::LBrack) => self.parse_expr_array(),
            Some(TokenKind::LBrace) => self.parse_expr_block(),
            Some(TokenKind::KwIf) => self.parse_expr_if(),
            Some(TokenKind::KwWhile) => self.parse_expr_while(),
            Some(TokenKind::KwFor) => self.parse_expr_for(),
            Some(TokenKind::KwMatch) => self.parse_expr_match(),
            Some(TokenKind::KwReturn) => self.parse_expr_return(),
            Some(TokenKind::KwDefer) => self.parse_expr_defer(),
            Some(TokenKind::KwBreak) => self.parse_expr_break(),
            Some(TokenKind::KwCycle) => {
                let _ = self.advance();
                Ok(self.arena.alloc_expr(ExprKind::Cycle, start))
            }
            Some(TokenKind::KwUnsafe) => self.parse_expr_unsafe(),
            Some(TokenKind::KwImport) => self.parse_expr_import(),
            Some(TokenKind::AtLBrack) => self.parse_expr_with_attrs(),
            Some(TokenKind::KwExport | TokenKind::KwExtern) => {
                self.parse_expr_with_modifiers(vec![], start)
            }
            Some(TokenKind::KwRecord) => self.parse_expr_record_def(vec![], Modifiers::default()),
            Some(TokenKind::KwChoice) => self.parse_expr_choice_def(vec![], Modifiers::default()),
            Some(TokenKind::KwAlias) => self.parse_expr_alias_def(vec![], Modifiers::default()),
            Some(TokenKind::KwFn) => self.parse_expr_fn_def(vec![], Modifiers::default()),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(Modifiers::default()),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_expr_lit(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let lit = self.parse_lit()?;
        Ok(self.arena.alloc_expr(ExprKind::Lit(lit), start))
    }

    fn parse_lit(&mut self) -> MusiResult<LitKind> {
        match self.peek_kind() {
            Some(TokenKind::LitInt(v)) => {
                let _ = self.advance();
                Ok(LitKind::Int(v))
            }
            Some(TokenKind::LitReal(v)) => {
                let _ = self.advance();
                Ok(LitKind::Real(v))
            }
            Some(TokenKind::LitString(id) | TokenKind::LitTemplateNoSubst(id)) => {
                let _ = self.advance();
                Ok(LitKind::String(id))
            }
            Some(TokenKind::LitRune(c)) => {
                let _ = self.advance();
                Ok(LitKind::Rune(c))
            }
            Some(TokenKind::KwTrue) => {
                let _ = self.advance();
                Ok(LitKind::Bool(true))
            }
            Some(TokenKind::KwFalse) => {
                let _ = self.advance();
                Ok(LitKind::Bool(false))
            }
            _ => Err(ParseErrorKind::ExpectedLit.into_musi_error(self.curr_span())),
        }
    }

    fn parse_expr_template(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let Some(TokenKind::TemplateHead(id)) = self.peek_kind() else {
            return Err(ParseErrorKind::ExpectedLit.into_musi_error(start));
        };
        let _ = self.advance();
        let mut parts = vec![TemplatePart::Text(id)];
        loop {
            let expr_id = self.parse_expr()?;
            parts.push(TemplatePart::Expr(expr_id));
            match self.peek_kind() {
                Some(TokenKind::TemplateMiddle(id)) => {
                    let _ = self.advance();
                    parts.push(TemplatePart::Text(id));
                }
                Some(TokenKind::TemplateTail(id)) => {
                    let _ = self.advance();
                    parts.push(TemplatePart::Text(id));
                    break;
                }
                _ => {
                    return Err(
                        ParseErrorKind::UnclosedTemplateExpr.into_musi_error(self.curr_span())
                    );
                }
            }
        }
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Lit(LitKind::Template(parts)), span))
    }

    fn parse_expr_tuple(&mut self, elems: ExprIds, start: Span) -> MusiResult<ExprId> {
        let _ = self.expect(TokenKind::RParen)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Tuple(elems), span))
    }

    fn parse_expr_grouped(&mut self, inner_id: ExprId, start: Span) -> MusiResult<ExprId> {
        let _ = self.expect(TokenKind::RParen)?;
        let inner = self.arena.exprs.get(inner_id);
        let kind = inner.kind.clone();
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(kind, span))
    }

    fn parse_expr_paren(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            let span = start.merge(self.prev_span());
            return Ok(self.arena.alloc_expr(ExprKind::Tuple(vec![]), span));
        }
        let first = self.parse_expr()?;
        if self.bump_if(TokenKind::Comma) {
            let mut elems = vec![first];
            if !self.at(TokenKind::RParen) {
                elems.extend(self.separated(TokenKind::Comma, Self::parse_expr)?);
            }
            self.parse_expr_tuple(elems, start)
        } else {
            self.parse_expr_grouped(first, start)
        }
    }

    fn parse_expr_array(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        let elems = self.separated(TokenKind::Comma, Self::parse_expr)?;
        let _ = self.expect(TokenKind::RBrack)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Array(elems), span))
    }

    fn parse_expr_ident(&mut self, ident: musi_ast::Ident) -> ExprId {
        let _ = self.advance();
        self.arena.alloc_expr(ExprKind::Ident(ident), ident.span)
    }

    fn parse_postfix_record(&mut self, lhs: ExprId, start: Span) -> MusiResult<ExprId> {
        self.advance_by(2); // consume `.` and `{`
        let fields = self.separated(TokenKind::Comma, Self::parse_field)?;
        let _ = self.expect(TokenKind::RBrace)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::Record {
                base: Some(lhs),
                fields,
            },
            span,
        ))
    }

    fn parse_expr_record_anon(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        self.advance_by(2); // consume `.` and `{`

        let mut base = None;
        if self.is_record_update() {
            base = Some(self.parse_expr()?);
            let _ = self.expect(TokenKind::KwWith)?;
        }

        let fields = self.separated(TokenKind::Comma, Self::parse_field)?;
        let _ = self.expect(TokenKind::RBrace)?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Record { base, fields }, span))
    }

    fn is_record_update(&self) -> bool {
        let mut depth = 0;
        let mut i = 0;
        while let Some(tok) = self.tokens.get(self.index + i) {
            match tok.kind {
                TokenKind::LBrace => depth += 1,
                TokenKind::RBrace => {
                    if depth == 0 {
                        break;
                    }
                    depth -= 1;
                }
                TokenKind::KwWith if depth == 0 => return true,
                TokenKind::Comma | TokenKind::ColonEq if depth == 0 => return false,
                _ => {}
            }
            i += 1;
        }
        false
    }

    fn parse_block_body(&mut self) -> MusiResult<(StmtIds, Option<ExprId>)> {
        let mut stmts = vec![];
        let mut final_expr = None;
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            let expr_id = self.parse_expr()?;
            if self.bump_if(TokenKind::Semicolon) {
                let span = self.prev_span();
                let stmt_id = self.arena.alloc_stmt(StmtKind::Expr(expr_id), span);
                stmts.push(stmt_id);
            } else {
                final_expr = Some(expr_id);
                break;
            }
        }
        Ok((stmts, final_expr))
    }

    fn parse_cond(&mut self) -> MusiResult<CondId> {
        if self.bump_if(TokenKind::KwCase) {
            let pat = self.parse_pat()?;
            let _ = self.expect(TokenKind::ColonEq)?;
            let init = self.parse_expr()?;
            let mut extra = vec![];
            while self.bump_if(TokenKind::Comma) {
                extra.push(self.parse_expr()?);
            }
            Ok(self.arena.alloc_cond(CondKind::Case { pat, init, extra }))
        } else {
            let expr = self.parse_expr()?;
            Ok(self.arena.alloc_cond(CondKind::Expr(expr)))
        }
    }

    fn parse_expr_if(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwIf)?;
        let cond = self.parse_cond()?;
        let then_br = self.parse_expr_block()?;
        let else_br = if self.bump_if(TokenKind::KwElse) {
            Some(if self.at(TokenKind::KwIf) {
                self.parse_expr_if()?
            } else {
                self.parse_expr_block()?
            })
        } else {
            None
        };
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::If {
                cond,
                then_br,
                else_br,
            },
            span,
        ))
    }

    fn parse_expr_while(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_cond()?;
        let body = self.parse_expr_block()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::While { cond, body }, span))
    }

    fn parse_expr_for(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFor)?;
        let pat = self.parse_pat()?;
        let _ = self.expect(TokenKind::KwIn)?;
        let iter = self.parse_expr()?;
        let body = self.parse_expr_block()?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::For { pat, iter, body }, span))
    }

    fn parse_expr_match(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwMatch)?;
        let scrutinee = self.parse_expr()?;
        let _ = self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        while self.bump_if(TokenKind::KwCase) {
            let pat = self.parse_pat()?;
            let guard = if self.bump_if(TokenKind::KwIf) {
                Some(self.parse_expr()?)
            } else {
                None
            };
            let _ = self.expect(TokenKind::EqGt)?;
            let body = self.parse_expr()?;
            cases.push(MatchCase { pat, guard, body });
            let _ = self.bump_if(TokenKind::Comma);
        }
        let _ = self.expect(TokenKind::RBrace)?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_expr(ExprKind::Match { scrutinee, cases }, span))
    }

    fn parse_expr_return(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwReturn)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Return(expr), span))
    }

    fn parse_expr_defer(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwDefer)?;
        let expr = self.parse_expr()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Defer(expr), span))
    }

    fn parse_expr_break(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwBreak)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Break(expr), span))
    }

    fn parse_expr_unsafe(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwUnsafe)?;
        let block = self.parse_expr_block()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Unsafe(block), span))
    }

    fn parse_expr_import(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwImport)?;
        let Some(TokenKind::LitString(path)) = self.peek_kind() else {
            return Err(ParseErrorKind::ExpectedStringLit.into_musi_error(self.curr_span()));
        };
        let _ = self.advance();
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(ExprKind::Import(path), span))
    }

    fn parse_expr_with_attrs(&mut self) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let attrs = self.parse_attrs()?;
        self.parse_expr_with_modifiers(attrs, start)
    }

    fn parse_expr_with_modifiers(&mut self, attrs: Attrs, start: Span) -> MusiResult<ExprId> {
        let mods = self.parse_modifiers();
        match self.peek_kind() {
            Some(TokenKind::KwRecord) => self.parse_expr_record_def(attrs, mods),
            Some(TokenKind::KwChoice) => self.parse_expr_choice_def(attrs, mods),
            Some(TokenKind::KwAlias) => self.parse_expr_alias_def(attrs, mods),
            Some(TokenKind::KwFn) => self.parse_expr_fn_def(attrs, mods),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(mods),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_expr_record_def(&mut self, attrs: Attrs, mods: Modifiers) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwRecord)?;
        let name = self.try_ident();
        let ty_params = self.parse_ty_expr_params()?;
        let fields = self.delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
            p.separated(TokenKind::Semicolon, Self::parse_field)
        })?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::RecordDef {
                attrs,
                mods,
                name,
                ty_params,
                fields,
            },
            span,
        ))
    }

    fn parse_expr_choice_def(&mut self, attrs: Attrs, mods: Modifiers) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwChoice)?;
        let name = self.try_ident();
        let ty_params = self.parse_ty_expr_params()?;
        let _ = self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        while self.bump_if(TokenKind::KwCase) {
            let case_name = self.expect_ident()?;
            let ty_args = self.parse_ty_expr_args()?;
            let fields = self.opt_delimited(TokenKind::LParen, TokenKind::RParen, |p| {
                p.parse_choice_case_items()
            })?;
            cases.push(ChoiceCase {
                name: case_name,
                ty_args,
                fields,
            });
            let _ = self.bump_if(TokenKind::Comma);
        }
        let _ = self.expect(TokenKind::RBrace)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::ChoiceDef {
                attrs,
                mods,
                name,
                ty_params,
                cases,
            },
            span,
        ))
    }

    fn parse_choice_case_items(&mut self) -> MusiResult<ChoiceCaseItems> {
        self.separated(TokenKind::Comma, |p| {
            if p.at(TokenKind::KwVar)
                || (matches!(p.peek_kind(), Some(TokenKind::Ident(_)))
                    && matches!(p.peek_nth(1), Some(TokenKind::Colon | TokenKind::ColonEq)))
            {
                Ok(ChoiceCaseItem::Field(p.parse_field()?))
            } else {
                Ok(ChoiceCaseItem::Type(p.parse_ty_expr()?))
            }
        })
    }

    fn parse_expr_alias_def(&mut self, attrs: Attrs, mods: Modifiers) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwAlias)?;
        let name = self.expect_ident()?;
        let ty_params = self.parse_ty_expr_params()?;
        let _ = self.expect(TokenKind::ColonEq)?;
        let ty = self.parse_ty_expr()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::Alias {
                attrs,
                mods,
                name,
                ty_params,
                ty,
            },
            span,
        ))
    }

    fn parse_expr_fn_def(&mut self, attrs: Attrs, mods: Modifiers) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFn)?;
        let name = self.try_ident();
        let ty_params = self.parse_ty_expr_params()?;
        let params = self.opt_delimited(TokenKind::LParen, TokenKind::RParen, |p| {
            p.separated(TokenKind::Comma, Self::parse_field)
        })?;
        let ret = self.maybe(TokenKind::Colon, Self::parse_ty_expr)?;
        let body = if self.bump_if(TokenKind::EqGt) {
            self.parse_expr()?
        } else {
            self.parse_expr_block()?
        };
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::Fn {
                attrs,
                mods,
                sig: FnSig {
                    name,
                    ty_params,
                    params,
                    ret,
                },
                body,
            },
            span,
        ))
    }

    fn parse_expr_bind(&mut self, mods: Modifiers) -> MusiResult<ExprId> {
        let start = self.curr_span();
        let mutable = self.at(TokenKind::KwVar);
        let _ = self.advance();
        let pat = self.parse_pat()?;
        let ty = self.maybe(TokenKind::Colon, Self::parse_ty_expr)?;
        let _ = self.expect(TokenKind::ColonEq)?;
        let init = self.parse_expr()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_expr(
            ExprKind::Bind {
                mods,
                mutable,
                pat,
                ty,
                init,
            },
            span,
        ))
    }
}

impl Parser<'_> {
    fn parse_attrs(&mut self) -> MusiResult<Attrs> {
        let mut attrs = vec![];
        while self.bump_if(TokenKind::AtLBrack) {
            loop {
                let name = self.expect_ident()?;
                let args = self.opt_delimited(TokenKind::LParen, TokenKind::RParen, |p| {
                    p.parse_attr_args()
                })?;
                attrs.push(Attr { name, args });
                if !self.bump_if(TokenKind::Comma) {
                    break;
                }
            }
            let _ = self.expect(TokenKind::RBrack)?;
        }
        Ok(attrs)
    }

    fn parse_attr_args(&mut self) -> MusiResult<AttrArgs> {
        self.separated(TokenKind::Comma, |p| {
            if let Some(TokenKind::Ident(id)) = p.peek_kind() {
                if p.peek_nth(1) == Some(TokenKind::ColonEq) {
                    p.advance_by(2);
                    let expr = p.parse_expr()?;
                    return Ok(AttrArg {
                        name: Some(id),
                        value: Some(expr),
                        lit: None,
                    });
                }
                let _ = p.advance();
                return Ok(AttrArg {
                    name: Some(id),
                    value: None,
                    lit: None,
                });
            }
            Ok(AttrArg {
                name: None,
                value: None,
                lit: Some(p.parse_lit()?),
            })
        })
    }

    fn parse_modifiers(&mut self) -> Modifiers {
        let mut mods = Modifiers::default();
        loop {
            match self.peek_kind() {
                Some(TokenKind::KwExport) => {
                    let _ = self.advance();
                    mods.exportness = true;
                }
                Some(TokenKind::KwExtern) => {
                    let _ = self.advance();
                    let abi = if let Some(TokenKind::LitString(id)) = self.peek_kind() {
                        let _ = self.advance();
                        Some(id)
                    } else {
                        None
                    };
                    mods.externness = (abi, true);
                }
                Some(TokenKind::KwUnsafe)
                    if !matches!(self.peek_nth(1), Some(TokenKind::LBrace)) =>
                {
                    let _ = self.advance();
                    mods.unsafeness = true;
                }
                _ => break,
            }
        }
        mods
    }

    fn parse_field(&mut self) -> MusiResult<Field> {
        let mutable = self.bump_if(TokenKind::KwVar);
        let name = self.expect_ident()?;
        let ty = self.maybe(TokenKind::Colon, Self::parse_ty_expr)?;
        let init = self.maybe(TokenKind::ColonEq, Self::parse_expr)?;
        Ok(Field {
            mutable,
            name,
            ty,
            init,
        })
    }

    fn can_start_expr(&self) -> bool {
        matches!(
            self.peek_kind(),
            Some(
                TokenKind::LitInt(_)
                    | TokenKind::LitReal(_)
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
                    | TokenKind::Dot
                    | TokenKind::Minus
                    | TokenKind::KwNot
                    | TokenKind::Tilde
                    | TokenKind::At
                    | TokenKind::KwIf
                    | TokenKind::KwWhile
                    | TokenKind::KwFor
                    | TokenKind::KwMatch
                    | TokenKind::KwTry
                    | TokenKind::KwReturn
                    | TokenKind::KwDefer
                    | TokenKind::KwBreak
                    | TokenKind::KwCycle
                    | TokenKind::KwUnsafe
                    | TokenKind::KwImport
                    | TokenKind::AtLBrack
                    | TokenKind::KwExport
                    | TokenKind::KwExtern
                    | TokenKind::KwRecord
                    | TokenKind::KwChoice
                    | TokenKind::KwAlias
                    | TokenKind::KwFn
                    | TokenKind::KwVal
                    | TokenKind::KwVar
            )
        )
    }
}

#[cfg(test)]
mod tests;
