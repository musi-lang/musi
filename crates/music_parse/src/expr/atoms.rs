use core::mem;

use music_ast::common::Param;
use music_ast::expr::{
    CompClause, ComprehensionData, ExprKind, FStrPart, PiecewiseArm, PwGuard, RecordField, UnaryOp,
};
use music_ast::pat::PatKind;
use music_ast::{ExprId, ParamList, PatId};
use music_lex::{StringFragment, Token, TokenKind, TriviaList};
use music_shared::{Ident, Literal, Span};

use crate::errors::{ParseError, ParseErrorKind, ParseResult, describe_token};
use crate::parser::Parser;

use super::PREFIX_BP;

impl Parser<'_> {
    pub(super) fn parse_literal(&mut self) -> ParseResult<ExprId> {
        let token = self.advance();
        let span = token.span;
        let lit = match &token.kind {
            TokenKind::Int(n) => Literal::Int(*n),
            TokenKind::Float(f) => Literal::Float(*f),
            TokenKind::Str(s) => Literal::Str(s.clone()),
            TokenKind::Rune(c) => Literal::Rune(*c),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedExpr {
                        found: describe_token(&token.kind),
                    },
                    span,
                    context: None,
                });
            }
        };
        Ok(self.alloc_expr(ExprKind::Lit(lit), span))
    }

    pub(super) fn parse_fstring_lit(&mut self) -> ParseResult<ExprId> {
        let token = self.advance();
        let span = token.span;
        let lex_parts = match &token.kind {
            TokenKind::FStr(parts) => parts.clone(),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedExpr {
                        found: describe_token(&token.kind),
                    },
                    span,
                    context: None,
                });
            }
        };
        let ast_parts = self.convert_fstr_parts(lex_parts)?;
        Ok(self.alloc_expr(ExprKind::FStrLit(ast_parts), span))
    }

    pub(super) fn convert_fstr_parts(
        &mut self,
        lex_parts: Vec<StringFragment>,
    ) -> ParseResult<Vec<FStrPart>> {
        let mut out = Vec::with_capacity(lex_parts.len());
        for part in lex_parts {
            match part {
                StringFragment::Lit(s) => out.push(FStrPart::Lit(s)),
                StringFragment::Expr(tokens) => {
                    let expr_id = self.parse_owned_sub_tokens(tokens)?;
                    out.push(FStrPart::Expr(expr_id));
                }
            }
        }
        Ok(out)
    }

    pub(super) fn parse_owned_sub_tokens(&mut self, tokens: Vec<Token>) -> ParseResult<ExprId> {
        let mut sub_tokens = Vec::with_capacity(tokens.len() + 1);
        sub_tokens.extend(tokens);
        let eof_span = sub_tokens
            .last()
            .map_or(Span::DUMMY, |t| Span::new(t.span.end, t.span.end));
        sub_tokens.push(Token {
            kind: TokenKind::Eof,
            span: eof_span,
            leading_trivia: TriviaList::new(),
            trailing_trivia: TriviaList::new(),
        });
        let mut sub = Parser::new(&sub_tokens, self.source, self.interner);
        sub.ast = mem::take(&mut self.ast);
        let result = sub.parse_expr(0);
        self.ast = sub.ast;
        self.errors.extend(sub.errors);
        result
    }

    pub(super) fn parse_ident_expr(&mut self) -> ParseResult<ExprId> {
        let ident = self.expect_ident()?;
        let span = ident.span;
        Ok(self.alloc_expr(ExprKind::Var(ident), span))
    }

    pub(crate) fn expect_ident(&mut self) -> ParseResult<Ident> {
        match self.peek_kind() {
            TokenKind::Ident => Ok(self.advance_ident()),
            TokenKind::EscapedIdent => Ok(self.advance_ident_trimmed()),
            TokenKind::Eof => Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof {
                    expected: "identifier",
                },
                span: self.span(),
                context: None,
            }),
            _ => Err(self.err_expected_token("identifier")),
        }
    }

    pub(super) fn expect_ident_or_kw_op(&mut self) -> ParseResult<Ident> {
        match self.peek_kind() {
            TokenKind::KwShl | TokenKind::KwShr => {
                let span = self.span();
                let _ = self.advance();
                Ok(self.make_ident_from_span(span))
            }
            _ => self.expect_ident(),
        }
    }

    pub(super) fn parse_paren(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::LParen, "'('")?;
        match self.peek_kind() {
            TokenKind::RParen => self.parse_unit(start),
            TokenKind::Comma | TokenKind::Semi => {
                let _ = self.advance();
                let end = self.expect(&TokenKind::RParen, "')'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::TupleLit(Vec::new()), span))
            }
            TokenKind::Pipe => self.parse_piecewise_from(None, start),
            _ => self.parse_paren_expr(start),
        }
    }

    pub(super) fn parse_unit(&mut self, start: Span) -> ParseResult<ExprId> {
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        self.try_lambda(Vec::new(), span)
    }

    pub(super) fn parse_paren_expr(&mut self, start: Span) -> ParseResult<ExprId> {
        let first = self.parse_expr(0)?;
        match self.peek_kind() {
            TokenKind::RParen => self.finish_grouped_or_lambda(first, start),
            TokenKind::Comma => self.parse_tuple_rest(first, start),
            TokenKind::Semi => self.parse_seq_rest(first, start),
            TokenKind::KwIf => {
                let _ = self.expect(&TokenKind::KwIf, "'if'")?;
                let guard = self.parse_pw_guard()?;
                self.parse_piecewise_from(Some((first, guard)), start)
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::InvalidParenForm,
                span: self.span(),
                context: Some("in parenthesized expression"),
            }),
        }
    }

    pub(super) fn finish_grouped_or_lambda(
        &mut self,
        inner: ExprId,
        start: Span,
    ) -> ParseResult<ExprId> {
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        if self.at(&TokenKind::EqGt) || self.at(&TokenKind::Colon) {
            let params = self.reinterpret_as_params(inner)?;
            return self.try_lambda(params, span);
        }
        Ok(inner)
    }

    pub(super) fn parse_tuple_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
        let mut items = vec![first];
        while self.eat(&TokenKind::Comma) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            items.push(self.parse_expr(0)?);
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        if self.at(&TokenKind::EqGt) || self.at(&TokenKind::Colon) {
            let params = self.reinterpret_exprs_as_params(&items)?;
            return self.try_lambda(params, span);
        }
        Ok(self.alloc_expr(ExprKind::TupleLit(items), span))
    }

    pub(super) fn parse_seq_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
        let mut stmts = vec![first];
        while self.eat(&TokenKind::Semi) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            stmts.push(self.parse_expr(0)?);
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Seq(stmts), span))
    }

    pub(super) fn parse_piecewise_from(
        &mut self,
        first_arm: Option<(ExprId, PwGuard)>,
        start: Span,
    ) -> ParseResult<ExprId> {
        let mut arms = if let Some((value, guard)) = first_arm {
            vec![PiecewiseArm { value, guard }]
        } else {
            let _ = self.advance();
            let val = self.parse_expr(0)?;
            let _ = self.expect(&TokenKind::KwIf, "'if'")?;
            let guard = self.parse_pw_guard()?;
            vec![PiecewiseArm { value: val, guard }]
        };
        while self.eat(&TokenKind::Pipe) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            let val = self.parse_expr(0)?;
            let _ = self.expect(&TokenKind::KwIf, "'if'")?;
            let guard = self.parse_pw_guard()?;
            arms.push(PiecewiseArm { value: val, guard });
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Piecewise(arms), span))
    }

    pub(super) fn parse_pw_guard(&mut self) -> ParseResult<PwGuard> {
        if self.eat_wildcard() {
            return Ok(PwGuard::Wildcard);
        }
        let expr = self.parse_expr(0)?;
        Ok(PwGuard::Expr(expr))
    }

    pub(super) fn try_lambda(&mut self, params: ParamList, start: Span) -> ParseResult<ExprId> {
        let ret_ty = if self.eat(&TokenKind::Colon) {
            Some(self.parse_ty()?)
        } else {
            None
        };
        if !self.at(&TokenKind::EqGt) {
            if params.is_empty() && ret_ty.is_none() {
                return Ok(self.alloc_expr(ExprKind::TupleLit(Vec::new()), start));
            }
            return Err(self.err_expected_token("'=>'"));
        }
        let _ = self.advance();
        let body = self.parse_expr(0)?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::Lambda {
                params,
                ret_ty,
                body,
            },
            span,
        ))
    }

    pub(super) fn reinterpret_as_params(&self, expr: ExprId) -> ParseResult<ParamList> {
        let spanned = self.ast.exprs.get(expr);
        match &spanned.kind {
            ExprKind::Var(ident) => Ok(vec![Param {
                mutable: false,
                name: *ident,
                ty: None,
                default: None,
            }]),
            ExprKind::TupleLit(items) => self.reinterpret_exprs_as_params(items),
            _ => Err(ParseError {
                kind: ParseErrorKind::InvalidParenForm,
                span: spanned.span,
                context: None,
            }),
        }
    }

    pub(super) fn reinterpret_exprs_as_params(&self, exprs: &[ExprId]) -> ParseResult<ParamList> {
        let mut params = Vec::with_capacity(exprs.len());
        for &expr_id in exprs {
            let spanned = self.ast.exprs.get(expr_id);
            match &spanned.kind {
                ExprKind::Var(ident) => params.push(Param {
                    mutable: false,
                    name: *ident,
                    ty: None,
                    default: None,
                }),
                _ => {
                    return Err(ParseError {
                        kind: ParseErrorKind::InvalidParenForm,
                        span: spanned.span,
                        context: None,
                    });
                }
            }
        }
        Ok(params)
    }

    pub(super) fn parse_array(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::LBracket, "'['")?;
        if self.at(&TokenKind::RBracket) {
            let end = self.expect(&TokenKind::RBracket, "']'")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::ArrayLit(Vec::new()), span));
        }
        let first = self.parse_expr(0)?;
        self.parse_array_rest(first, start)
    }

    pub(super) fn parse_array_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
        match self.peek_kind() {
            TokenKind::RBracket => {
                let end = self.expect(&TokenKind::RBracket, "']'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::ArrayLit(vec![first]), span))
            }
            TokenKind::Pipe => self.parse_comprehension(first, start),
            TokenKind::Semi => self.parse_matrix_rest(vec![first], start),
            TokenKind::Comma => {
                let _ = self.advance();
                let mut items = vec![first];
                loop {
                    if self.at(&TokenKind::RBracket) {
                        break;
                    }
                    items.push(self.parse_array_item()?);
                    if self.at(&TokenKind::Semi) {
                        return self.parse_matrix_rest(items, start);
                    }
                    if !self.eat(&TokenKind::Comma) {
                        break;
                    }
                }
                let end = self.expect(&TokenKind::RBracket, "']'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::ArrayLit(items), span))
            }
            _ => {
                Err(self
                    .err_expected_token_in("',' or ';' or '|' or ']'", Some("in array literal")))
            }
        }
    }

    pub(super) fn parse_array_item(&mut self) -> ParseResult<ExprId> {
        if self.at(&TokenKind::DotDotDot) {
            let spread_start = self.span();
            let _ = self.advance();
            let target = self.parse_expr(PREFIX_BP)?;
            let spread_span = spread_start.to(self.prev_span());
            Ok(self.alloc_expr(ExprKind::UnaryOp(UnaryOp::Spread, target), spread_span))
        } else {
            self.parse_expr(0)
        }
    }

    pub(super) fn parse_matrix_rest(
        &mut self,
        first_row: Vec<ExprId>,
        start: Span,
    ) -> ParseResult<ExprId> {
        let mut rows = vec![first_row];
        while self.eat(&TokenKind::Semi) {
            if self.at(&TokenKind::RBracket) {
                break;
            }
            let mut row = vec![self.parse_expr(0)?];
            while self.eat(&TokenKind::Comma) {
                row.push(self.parse_expr(0)?);
            }
            rows.push(row);
        }
        let end = self.expect(&TokenKind::RBracket, "']'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::MatrixLit(rows), span))
    }

    pub(super) fn parse_comprehension(&mut self, expr: ExprId, start: Span) -> ParseResult<ExprId> {
        let _ = self.advance();
        let mut clauses = Vec::new();
        clauses.push(self.parse_comp_clause()?);
        while self.eat(&TokenKind::Comma) {
            if self.at(&TokenKind::RBracket) {
                break;
            }
            clauses.push(self.parse_comp_clause()?);
        }
        let end = self.expect(&TokenKind::RBracket, "']'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(
            ExprKind::Comprehension(Box::new(ComprehensionData { expr, clauses })),
            span,
        ))
    }

    pub(super) fn parse_comp_clause(&mut self) -> ParseResult<CompClause> {
        let expr = self.parse_expr(0)?;
        if self.eat(&TokenKind::KwIn) {
            let pat = self.expr_to_pat(expr)?;
            let iter = self.parse_expr(0)?;
            Ok(CompClause::Generator { pat, iter })
        } else {
            Ok(CompClause::Filter(expr))
        }
    }

    pub(super) fn expr_to_pat(&mut self, expr_id: ExprId) -> ParseResult<PatId> {
        let spanned = self.ast.exprs.get(expr_id);
        let span = spanned.span;
        match &spanned.kind {
            ExprKind::Var(ident) => {
                let ident = *ident;
                Ok(self.alloc_pat(PatKind::Bind(ident), span))
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::ExpectedPat {
                    found: "expression",
                },
                span,
                context: None,
            }),
        }
    }

    pub(super) fn parse_rec_lit(&mut self) -> ParseResult<ExprId> {
        let open_span = self.expect(&TokenKind::DotLBrace, "'.{'")?;
        let fields = self.parse_rec_fields()?;
        let end =
            self.expect_closing(&TokenKind::RBrace, "'.{'", open_span, "in record literal")?;
        let span = open_span.to(end);
        Ok(self.alloc_expr(ExprKind::RecordLit(fields), span))
    }

    pub(super) fn parse_rec_fields(&mut self) -> ParseResult<Vec<RecordField>> {
        let mut fields = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            if self.at(&TokenKind::DotDotDot) {
                let _ = self.advance();
                let expr = self.parse_expr(PREFIX_BP)?;
                fields.push(RecordField::Spread(expr));
            } else {
                let name = self.expect_ident()?;
                let value = self.parse_opt_default()?;
                fields.push(RecordField::Named { name, value });
            }
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(fields)
    }

    pub(super) fn parse_dot_pfx(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::Dot, "'.'")?;
        let name = self.expect_ident()?;
        let args = if self.eat(&TokenKind::LParen) {
            let exprs = self.parse_expr_list(&TokenKind::RParen)?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            exprs
        } else {
            Vec::new()
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::VariantLit(name, args), span))
    }
}
