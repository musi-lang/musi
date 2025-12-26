use musi_ast::{
    Attr, AttrArg, AttrArgList, AttrList, Cond, CondPtr, Expr, ExprKind, Field, FnSig, LitKind,
    MatchCase, Modifiers, OptExpr, OptExprPtr, Stmt, StmtKind, StmtList, SumCase, SumCaseItem,
    SumCaseItemList, TemplatePart,
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
    pub fn parse_expr(&mut self) -> MusiResult<Expr> {
        self.parse_expr_bp(0)
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_expr_block(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let (stmts, expr) =
            self.delimited(TokenKind::LBrace, TokenKind::RBrace, Self::parse_block_body)?;
        Ok(Expr::new(
            ExprKind::Block { stmts, expr },
            start.merge(self.prev_span()),
        ))
    }

    /// # Errors
    /// Returns `ParseErrorKind::ExpectedIdent` if current token is not identifier.
    pub fn expect_ident(&mut self) -> MusiResult<u32> {
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                Ok(id)
            }
            _ => Err(ParseErrorKind::ExpectedIdent.into_musi_error(self.curr_span())),
        }
    }

    pub fn try_ident(&mut self) -> Option<u32> {
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
    pub fn parse_expr_bp(&mut self, min_bp: u8) -> MusiResult<Expr> {
        let start = self.curr_span();
        let mut lhs = self.parse_expr_prefix(start)?;
        loop {
            if let Some(new_lhs) = self.try_parse_postfix(lhs.clone(), min_bp)? {
                lhs = new_lhs;
                continue;
            }
            if let Some(new_lhs) = self.try_parse_not_in(lhs.clone(), start, min_bp)? {
                lhs = new_lhs;
                continue;
            }
            if let Some(new_lhs) = self.try_parse_infix(lhs.clone(), start, min_bp)? {
                lhs = new_lhs;
                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn parse_expr_prefix(&mut self, start: Span) -> MusiResult<Expr> {
        match self.peek_kind().and_then(Prec::prefix) {
            Some(bp) => {
                let op = self.advance().expect("Prec::prefix").kind;
                Ok(Expr::unary(
                    op,
                    self.parse_expr_bp(bp)?,
                    start.merge(self.prev_span()),
                ))
            }
            None => self.parse_expr_primary(),
        }
    }

    fn try_parse_postfix(&mut self, lhs: Expr, min_bp: u8) -> MusiResult<OptExpr> {
        if let Some(l_bp) = self.peek_kind().and_then(Prec::postfix)
            && l_bp >= min_bp
        {
            return Ok(Some(self.parse_expr_postfix(lhs)?));
        }
        Ok(None)
    }

    fn try_parse_not_in(&mut self, lhs: Expr, start: Span, min_bp: u8) -> MusiResult<OptExpr> {
        if self.at(TokenKind::KwNot) && self.peek_nth(1) == Some(TokenKind::KwIn) {
            let (l_bp, r_bp) = Prec::infix(TokenKind::KwIn).expect("`in` is infix");
            if l_bp >= min_bp {
                self.advance_by(2);
                let span = start.merge(self.prev_span());
                let inner = Expr::binary(TokenKind::KwIn, lhs, self.parse_expr_bp(r_bp)?, span);
                return Ok(Some(Expr::unary(TokenKind::KwNot, inner, span)));
            }
        }
        Ok(None)
    }

    fn try_parse_infix(&mut self, lhs: Expr, start: Span, min_bp: u8) -> MusiResult<OptExpr> {
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
        lhs: Expr,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<Expr> {
        if matches!(op, TokenKind::DotDot | TokenKind::DotDotLt) {
            return self.parse_expr_range(op, lhs, r_bp, start);
        }
        self.parse_expr_binary_or_assign(op, lhs, r_bp, start)
    }

    fn parse_expr_range(
        &mut self,
        op: TokenKind,
        lhs: Expr,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<Expr> {
        let end = if self.can_start_expr() {
            Some(Box::new(self.parse_expr_bp(r_bp)?))
        } else {
            None
        };
        Ok(Expr::new(
            ExprKind::Range {
                start: Box::new(lhs),
                end,
                inclusive: op == TokenKind::DotDot,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_binary_or_assign(
        &mut self,
        op: TokenKind,
        lhs: Expr,
        r_bp: u8,
        start: Span,
    ) -> MusiResult<Expr> {
        let rhs = self.parse_expr_bp(r_bp)?;
        let span = start.merge(rhs.span);
        Ok(if op == TokenKind::LtMinus {
            Expr::new(
                ExprKind::Assign {
                    target: Box::new(lhs),
                    value: Box::new(rhs),
                },
                span,
            )
        } else {
            Expr::binary(op, lhs, rhs, span)
        })
    }

    fn parse_expr_postfix(&mut self, lhs: Expr) -> MusiResult<Expr> {
        let start = lhs.span;
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

    fn parse_postfix_call(&mut self, lhs: Expr, start: Span) -> MusiResult<Expr> {
        let args = self.delimited(TokenKind::LParen, TokenKind::RParen, |p| {
            p.separated(TokenKind::Comma, Self::parse_expr)
        })?;
        Ok(Expr::new(
            ExprKind::Call {
                callee: Box::new(lhs),
                args,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_postfix_index(&mut self, lhs: Expr, start: Span) -> MusiResult<Expr> {
        self.advance_by(2); // consume `.` and `[`
        let index = self.parse_expr()?;
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(Expr::new(
            ExprKind::Index {
                base: Box::new(lhs),
                index: Box::new(index),
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_postfix_field(&mut self, lhs: Expr, start: Span) -> MusiResult<Expr> {
        let _ = self.advance(); // consume `.`
        Ok(Expr::new(
            ExprKind::Field {
                base: Box::new(lhs),
                field: self.expect_ident()?,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_postfix_deref(&mut self, lhs: Expr, start: Span) -> Expr {
        let _ = self.advance(); // consume `.^`
        Expr::new(
            ExprKind::Deref(Box::new(lhs)),
            start.merge(self.prev_span()),
        )
    }

    fn parse_expr_primary(&mut self) -> MusiResult<Expr> {
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
            Some(TokenKind::KwTry) => self.parse_expr_try(),
            Some(TokenKind::KwReturn) => self.parse_expr_return(),
            Some(TokenKind::KwDefer) => self.parse_expr_defer(),
            Some(TokenKind::KwBreak) => self.parse_expr_break(),
            Some(TokenKind::KwCycle) => {
                let _ = self.advance();
                Ok(Expr::new(ExprKind::Cycle, start))
            }
            Some(TokenKind::KwUnsafe) => self.parse_expr_unsafe(),
            Some(TokenKind::KwImport) => self.parse_expr_import(),
            Some(TokenKind::AtLBrack) => self.parse_expr_with_attrs(),
            Some(TokenKind::KwExport | TokenKind::KwExtern) => {
                self.parse_expr_with_modifiers(vec![], start)
            }
            Some(TokenKind::KwRecord) => self.parse_expr_record_def(vec![], Modifiers::default()),
            Some(TokenKind::KwSum) => self.parse_expr_sum_def(vec![], Modifiers::default()),
            Some(TokenKind::KwAlias) => self.parse_expr_alias_def(vec![], Modifiers::default()),
            Some(TokenKind::KwFn) => self.parse_expr_fn_def(vec![], Modifiers::default()),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(Modifiers::default()),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_expr_lit(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let lit = self.parse_lit()?;
        Ok(Expr::new(ExprKind::Lit(lit), start))
    }

    fn parse_lit(&mut self) -> MusiResult<LitKind> {
        match self.peek_kind() {
            Some(TokenKind::LitInt(id)) => {
                let _ = self.advance();
                Ok(LitKind::Int(i64::from(id)))
            }
            Some(TokenKind::LitReal(id)) => {
                let _ = self.advance();
                Ok(LitKind::Real(f64::from(id)))
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

    fn parse_expr_template(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let Some(TokenKind::TemplateHead(id)) = self.peek_kind() else {
            return Err(ParseErrorKind::ExpectedLit.into_musi_error(start));
        };
        let _ = self.advance();
        let mut parts = vec![TemplatePart::Text(id)];
        loop {
            parts.push(TemplatePart::Expr(Box::new(self.parse_expr()?)));
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
        Ok(Expr::new(
            ExprKind::Lit(LitKind::Template(parts)),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_tuple(&mut self, elems: Vec<Expr>, start: Span) -> MusiResult<Expr> {
        let _ = self.expect(TokenKind::RParen)?;
        Ok(Expr::new(
            ExprKind::Tuple(elems),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_grouped(&mut self, inner: Expr, start: Span) -> MusiResult<Expr> {
        let _ = self.expect(TokenKind::RParen)?;
        Ok(Expr::new(inner.kind, start.merge(self.prev_span())))
    }

    fn parse_expr_paren(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            return Ok(Expr::new(
                ExprKind::Tuple(vec![]),
                start.merge(self.prev_span()),
            ));
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

    fn parse_expr_array(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.advance();
        let elems = self.separated(TokenKind::Comma, Self::parse_expr)?;
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(Expr::new(
            ExprKind::Array(elems),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_ident(&mut self, id: u32) -> Expr {
        let start = self.curr_span();
        let _ = self.advance();
        Expr::new(ExprKind::Ident(id), start)
    }

    fn parse_postfix_record(&mut self, lhs: Expr, start: Span) -> MusiResult<Expr> {
        self.advance_by(2); // consume `.` and `{`
        let fields = self.separated(TokenKind::Comma, Self::parse_field)?;
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr::new(
            ExprKind::Record {
                ty: Some(Box::new(lhs)),
                fields,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_record_anon(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        self.advance_by(2); // consume `.` and `{`
        let fields = self.separated(TokenKind::Comma, Self::parse_field)?;
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr::new(
            ExprKind::Record { ty: None, fields },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_block_body(&mut self) -> MusiResult<(StmtList, OptExprPtr)> {
        let mut stmts = vec![];
        let mut final_expr = None;
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            let expr = self.parse_expr()?;
            if self.bump_if(TokenKind::Semicolon) {
                stmts.push(Stmt {
                    kind: StmtKind::Expr(expr),
                    span: self.prev_span(),
                });
            } else {
                final_expr = Some(Box::new(expr));
                break;
            }
        }
        Ok((stmts, final_expr))
    }

    fn parse_cond(&mut self) -> MusiResult<CondPtr> {
        if self.bump_if(TokenKind::KwCase) {
            let pat = self.parse_pat()?;
            let _ = self.expect(TokenKind::ColonEq)?;
            let init = self.parse_expr()?;
            let mut extra = vec![];
            while self.bump_if(TokenKind::Comma) {
                extra.push(self.parse_expr()?);
            }
            Ok(CondPtr::new(Cond::Case { pat, init, extra }))
        } else {
            Ok(CondPtr::new(Cond::Expr(self.parse_expr()?)))
        }
    }

    fn parse_expr_if(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwIf)?;
        let cond = self.parse_cond()?;
        let then_br = Box::new(self.parse_expr_block()?);
        let else_br = if self.bump_if(TokenKind::KwElse) {
            Some(Box::new(if self.at(TokenKind::KwIf) {
                self.parse_expr_if()?
            } else {
                self.parse_expr_block()?
            }))
        } else {
            None
        };
        Ok(Expr::new(
            ExprKind::If {
                cond,
                then_br,
                else_br,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_while(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwWhile)?;
        let cond = self.parse_cond()?;
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr::new(
            ExprKind::While { cond, body },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_for(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFor)?;
        let pat = self.parse_pat()?;
        let _ = self.expect(TokenKind::KwIn)?;
        let iter = Box::new(self.parse_expr()?);
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr::new(
            ExprKind::For { pat, iter, body },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_match(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwMatch)?;
        let scrutinee = Box::new(self.parse_expr()?);
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
        Ok(Expr::new(
            ExprKind::Match { scrutinee, cases },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_try(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwTry)?;
        let expr = Box::new(self.parse_expr()?);
        let (else_binding, else_body) = if self.bump_if(TokenKind::KwElse) {
            let bind = match self.peek_kind() {
                Some(TokenKind::Ident(id)) if !self.at(TokenKind::LBrace) => {
                    let _ = self.advance();
                    Some(id)
                }
                _ => None,
            };
            (bind, Some(Box::new(self.parse_expr()?)))
        } else {
            (None, None)
        };
        Ok(Expr::new(
            ExprKind::Try {
                expr,
                else_binding,
                else_body,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_return(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwReturn)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        Ok(Expr::new(
            ExprKind::Return(expr),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_defer(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwDefer)?;
        Ok(Expr::new(
            ExprKind::Defer(Box::new(self.parse_expr()?)),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_break(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwBreak)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        Ok(Expr::new(
            ExprKind::Break(expr),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_unsafe(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwUnsafe)?;
        Ok(Expr::new(
            ExprKind::Unsafe(Box::new(self.parse_expr_block()?)),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_import(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwImport)?;
        let Some(TokenKind::LitString(path)) = self.peek_kind() else {
            return Err(ParseErrorKind::ExpectedStringLit.into_musi_error(self.curr_span()));
        };
        let _ = self.advance();
        Ok(Expr::new(
            ExprKind::Import(path),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_with_attrs(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let attrs = self.parse_attrs()?;
        self.parse_expr_with_modifiers(attrs, start)
    }

    fn parse_expr_with_modifiers(&mut self, attrs: AttrList, start: Span) -> MusiResult<Expr> {
        let mods = self.parse_modifiers();
        match self.peek_kind() {
            Some(TokenKind::KwRecord) => self.parse_expr_record_def(attrs, mods),
            Some(TokenKind::KwSum) => self.parse_expr_sum_def(attrs, mods),
            Some(TokenKind::KwAlias) => self.parse_expr_alias_def(attrs, mods),
            Some(TokenKind::KwFn) => self.parse_expr_fn_def(attrs, mods),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(mods),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_expr_record_def(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwRecord)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let fields = self.delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
            p.separated(TokenKind::Semicolon, Self::parse_field)
        })?;
        Ok(Expr::new(
            ExprKind::RecordDef {
                attrs,
                mods,
                name,
                ty_params,
                fields,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_sum_def(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwSum)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let _ = self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        while self.bump_if(TokenKind::KwCase) {
            let case_name = self.expect_ident()?;
            let ty_args = self.parse_typ_args()?;
            let fields = self.opt_delimited(TokenKind::LParen, TokenKind::RParen, |p| {
                p.parse_sum_case_items()
            })?;
            cases.push(SumCase {
                name: case_name,
                ty_args,
                fields,
            });
            let _ = self.bump_if(TokenKind::Comma);
        }
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr::new(
            ExprKind::SumDef {
                attrs,
                mods,
                name,
                ty_params,
                cases,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_sum_case_items(&mut self) -> MusiResult<SumCaseItemList> {
        self.separated(TokenKind::Comma, |p| {
            if p.at(TokenKind::KwVar)
                || (matches!(p.peek_kind(), Some(TokenKind::Ident(_)))
                    && matches!(p.peek_nth(1), Some(TokenKind::Colon | TokenKind::ColonEq)))
            {
                Ok(SumCaseItem::Field(p.parse_field()?))
            } else {
                Ok(SumCaseItem::Type(p.parse_typ()?))
            }
        })
    }

    fn parse_expr_alias_def(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwAlias)?;
        let name = self.expect_ident()?;
        let ty_params = self.parse_typ_params()?;
        let _ = self.expect(TokenKind::ColonEq)?;
        let ty = self.parse_typ()?;
        Ok(Expr::new(
            ExprKind::Alias {
                attrs,
                mods,
                name,
                ty_params,
                ty,
            },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_fn_def(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFn)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let params = self.opt_delimited(TokenKind::LParen, TokenKind::RParen, |p| {
            p.separated(TokenKind::Comma, Self::parse_field)
        })?;
        let ret = self.maybe(TokenKind::Colon, Self::parse_typ)?;
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr::new(
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
            start.merge(self.prev_span()),
        ))
    }

    fn parse_expr_bind(&mut self, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let mutable = self.at(TokenKind::KwVar);
        let _ = self.advance();
        let pat = self.parse_pat()?;
        let ty = self.maybe(TokenKind::Colon, Self::parse_typ)?;
        let _ = self.expect(TokenKind::ColonEq)?;
        let init = Box::new(self.parse_expr()?);
        Ok(Expr::new(
            ExprKind::Bind {
                mods,
                mutable,
                pat,
                ty,
                init,
            },
            start.merge(self.prev_span()),
        ))
    }
}

impl Parser<'_> {
    fn parse_attrs(&mut self) -> MusiResult<AttrList> {
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

    fn parse_attr_args(&mut self) -> MusiResult<AttrArgList> {
        self.separated(TokenKind::Comma, |p| {
            if let Some(TokenKind::Ident(id)) = p.peek_kind() {
                if p.peek_nth(1) == Some(TokenKind::ColonEq) {
                    p.advance_by(2);
                    return Ok(AttrArg {
                        name: Some(id),
                        value: Some(p.parse_expr()?),
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
        let ty = self.maybe(TokenKind::Colon, Self::parse_typ)?;
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
                    | TokenKind::KwSum
                    | TokenKind::KwAlias
                    | TokenKind::KwFn
                    | TokenKind::KwVal
                    | TokenKind::KwVar
            )
        )
    }
}
