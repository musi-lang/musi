use core::mem;

use music_ast::common::{
    Attr, AttrArg, Constraint, EffectItem, FnDecl, LawDecl, MemberDecl, MemberName, ModifierSet,
    OpFixity, Param, RecordDefField, Signature, TyRef, VariantDef,
};
use music_ast::expr::{
    AccessMode, BinOp, CaseArm, CaseData, ClassDefData, CompClause, ComprehensionData, DataBody,
    ExprKind, FStrPart, FieldTarget, HandleData, HandlerClause, ImportKind, IndexKind,
    InstanceBody, InstanceDef, LetBinding, PiecewiseArm, PostfixOp, PwGuard, QuoteKind,
    RecordField, SpliceKind, TypeOpKind, UnaryOp,
};
use music_ast::pat::PatKind;
use music_ast::{AttrList, ExprId, ExprList, IdentList, ParamList, PatId};
use music_lex::{TokenKind, TriviaList};
use music_shared::{Ident, Literal, Span};

use crate::errors::{ParseError, ParseErrorKind, ParseResult, describe_token};
use crate::parser::Parser;

// Prefix binding power for unary operators.
const PREFIX_BP: u8 = 22;

// Assignment binding power (lowest, right-associative).
const ASSIGN_BP: u8 = 2;

// Postfix binding power (higher than any infix).
const POSTFIX_BP: u8 = 24;

const fn is_comparison(op: BinOp) -> bool {
    matches!(
        op,
        BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq
    )
}

const fn is_range(op: BinOp) -> bool {
    matches!(op, BinOp::Range | BinOp::RangeExcl)
}

impl Parser<'_> {
    // ── Pratt core ────────────────────────────────────────────────

    pub(crate) fn parse_expr(&mut self, min_bp: u8) -> ParseResult<ExprId> {
        self.parse_expr_inner(min_bp, true)
    }

    /// Parse an expression but do not treat `(` as a postfix call.
    /// Used for match scrutinees where `(` starts the arm list.
    fn parse_expr_no_call(&mut self, min_bp: u8) -> ParseResult<ExprId> {
        self.parse_expr_inner(min_bp, false)
    }

    fn parse_expr_inner(&mut self, min_bp: u8, allow_call: bool) -> ParseResult<ExprId> {
        let mut left = self.parse_prefix()?;
        loop {
            if let Some(new_left) = self.try_postfix(left, allow_call)? {
                left = new_left;
                continue;
            }
            if let Some(new_left) = self.try_assign(left)? {
                left = new_left;
                continue;
            }
            let Some((op, l_bp, r_bp)) = self.peek_infix_bp() else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let _ = self.advance();
            let right = self.parse_expr(r_bp)?;
            let left_span = self.ast.exprs.get(left).span;
            let right_span = self.ast.exprs.get(right).span;
            let span = left_span.to(right_span);
            if let ExprKind::BinOp(left_op, _, _) = self.ast.exprs.get(left).kind {
                if (is_comparison(op) && is_comparison(left_op))
                    || (is_range(op) && is_range(left_op))
                {
                    self.error(ParseError {
                        kind: ParseErrorKind::NonAssociativeChain,
                        span,
                        context: None,
                    });
                }
            }
            left = self.alloc_expr(ExprKind::BinOp(op, left, right), span);
        }
        Ok(left)
    }

    fn parse_prefix(&mut self) -> ParseResult<ExprId> {
        let start = self.span();
        let op = match self.peek_kind() {
            TokenKind::Minus => Some(UnaryOp::Neg),
            TokenKind::KwNot => Some(UnaryOp::Not),
            TokenKind::KwMut => Some(UnaryOp::Mut),
            TokenKind::DotDotDot => Some(UnaryOp::Spread),
            _ => None,
        };
        if let Some(op) = op {
            let _ = self.advance();
            return self.parse_unary(op, start);
        }
        self.parse_atom()
    }

    fn parse_unary(&mut self, op: UnaryOp, start: Span) -> ParseResult<ExprId> {
        let operand = self.parse_expr(PREFIX_BP)?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::UnaryOp(op, operand), span))
    }

    // ── Atom dispatch ─────────────────────────────────────────────

    fn parse_atom(&mut self) -> ParseResult<ExprId> {
        match self.peek_kind() {
            TokenKind::Int(_) | TokenKind::Float(_) | TokenKind::Str(_) | TokenKind::Rune(_) => {
                self.parse_literal()
            }
            TokenKind::FStr(_) => self.parse_fstring_lit(),
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_ident_expr(),
            TokenKind::LParen => self.parse_paren(),
            TokenKind::LBracket => self.parse_array(),
            TokenKind::DotLBrace => self.parse_rec_lit(),
            TokenKind::Dot => self.parse_dot_pfx(),
            TokenKind::KwCase => self.parse_case(),
            TokenKind::KwLet => self.parse_let(ModifierSet::default(), Vec::new()),
            TokenKind::KwReturn => self.parse_return(),
            TokenKind::KwResume => self.parse_resume(),
            TokenKind::KwImport => self.parse_import(),
            TokenKind::KwForeign => self.parse_foreign(ModifierSet::default()),
            TokenKind::KwData => self.parse_data_def(),
            TokenKind::KwEffect => self.parse_effect_def(),
            TokenKind::KwClass => self.parse_class_def(),
            TokenKind::KwInstance => self.parse_instance_def(false, Vec::new()),
            TokenKind::KwPerform => self.parse_perform(),
            TokenKind::KwHandle => self.parse_handle(),
            TokenKind::KwQuote => self.parse_quote(),
            TokenKind::Hash | TokenKind::HashLParen | TokenKind::HashLBracket => {
                self.parse_splice()
            }
            TokenKind::At => self.parse_with_attrs(),
            TokenKind::KwExport => self.parse_export(),
            _ => Err(self.err_expected_expr()),
        }
    }

    // ── Infix binding power ───────────────────────────────────────

    fn peek_infix_bp(&self) -> Option<(BinOp, u8, u8)> {
        match self.peek_kind() {
            TokenKind::QuestionQuestion => Some((BinOp::NilCoalesce, 4, 3)),
            TokenKind::PipeGt => Some((BinOp::PipeRight, 6, 7)),
            TokenKind::KwOr => Some((BinOp::Or, 8, 9)),
            TokenKind::KwXor => Some((BinOp::Xor, 10, 11)),
            TokenKind::KwAnd => Some((BinOp::And, 12, 13)),
            TokenKind::Eq => Some((BinOp::Eq, 14, 15)),
            TokenKind::SlashEq => Some((BinOp::NotEq, 14, 15)),
            TokenKind::Lt => Some((BinOp::Lt, 14, 15)),
            TokenKind::Gt => Some((BinOp::Gt, 14, 15)),
            TokenKind::LtEq => Some((BinOp::LtEq, 14, 15)),
            TokenKind::GtEq => Some((BinOp::GtEq, 14, 15)),
            TokenKind::DotDot => Some((BinOp::Range, 16, 17)),
            TokenKind::DotDotLt => Some((BinOp::RangeExcl, 16, 17)),
            TokenKind::ColonColon => Some((BinOp::Cons, 17, 16)),
            TokenKind::Plus => Some((BinOp::Add, 18, 19)),
            TokenKind::Minus => Some((BinOp::Sub, 18, 19)),
            TokenKind::KwShl => Some((BinOp::Shl, 17, 18)),
            TokenKind::KwShr => Some((BinOp::Shr, 17, 18)),
            TokenKind::Star => Some((BinOp::Mul, 20, 21)),
            TokenKind::Slash => Some((BinOp::Div, 20, 21)),
            TokenKind::Percent => Some((BinOp::Rem, 20, 21)),
            _ => None,
        }
    }

    fn try_assign(&mut self, left: ExprId) -> ParseResult<Option<ExprId>> {
        if !self.at(&TokenKind::LtMinus) {
            return Ok(None);
        }
        let _ = self.advance();
        let right = self.parse_expr(ASSIGN_BP)?;
        let left_span = self.ast.exprs.get(left).span;
        let right_span = self.ast.exprs.get(right).span;
        let span = left_span.to(right_span);
        Ok(Some(self.alloc_expr(ExprKind::Assign(left, right), span)))
    }

    // ── Postfix ───────────────────────────────────────────────────

    fn try_postfix(&mut self, left: ExprId, allow_call: bool) -> ParseResult<Option<ExprId>> {
        match self.peek_kind() {
            TokenKind::LParen if allow_call => Ok(Some(self.parse_call(left)?)),
            TokenKind::DotLBracket => Ok(Some(self.parse_index(left)?)),
            TokenKind::DotLBrace => Ok(Some(self.parse_record_update(left)?)),
            TokenKind::Dot => Ok(Some(self.parse_access(left, AccessMode::Direct)?)),
            TokenKind::QuestionDot => Ok(Some(self.parse_access(left, AccessMode::Optional)?)),
            TokenKind::BangDot => Ok(Some(self.parse_access(left, AccessMode::Forced)?)),
            TokenKind::Question => Ok(Some(self.parse_postfix_op(left, PostfixOp::Propagate))),
            TokenKind::Bang => Ok(Some(self.parse_postfix_op(left, PostfixOp::Force))),
            TokenKind::ColonQuestion => Ok(Some(self.parse_type_op(left, true)?)),
            TokenKind::ColonQuestionGt => Ok(Some(self.parse_type_op(left, false)?)),
            _ => Ok(None),
        }
    }

    fn parse_call(&mut self, callee: ExprId) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let args = self.parse_expr_list(&TokenKind::RParen)?;
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = self.ast.exprs.get(callee).span.to(end);
        Ok(self.alloc_expr(ExprKind::App(callee, args), span))
    }

    fn parse_index(&mut self, base: ExprId) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::DotLBracket, "'.['")?;
        let indices = self.parse_expr_list(&TokenKind::RBracket)?;
        let end = self.expect(&TokenKind::RBracket, "']'")?;
        let span = self.ast.exprs.get(base).span.to(end);
        Ok(self.alloc_expr(
            ExprKind::Index {
                expr: base,
                indices,
                kind: IndexKind::Point,
            },
            span,
        ))
    }

    fn parse_record_update(&mut self, base: ExprId) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::DotLBrace, "'.{'")?;
        let fields = self.parse_rec_fields()?;
        let end = self.expect(&TokenKind::RBrace, "'}'")?;
        let span = self.ast.exprs.get(base).span.to(end);
        Ok(self.alloc_expr(ExprKind::RecordUpdate { base, fields }, span))
    }

    fn parse_access(&mut self, base: ExprId, mode: AccessMode) -> ParseResult<ExprId> {
        let _ = self.advance(); // consume . or ?. or !.
        let field = self.parse_field_target()?;
        let span = self.ast.exprs.get(base).span.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::Access {
                expr: base,
                field,
                mode,
            },
            span,
        ))
    }

    fn parse_field_target(&mut self) -> ParseResult<FieldTarget> {
        match self.peek_kind() {
            TokenKind::Int(n) => {
                let val = u32::try_from(*n)
                    .map_err(|_| self.err_expected_token("tuple index (0..2^32)"))?;
                let _ = self.advance();
                Ok(FieldTarget::Index(val))
            }
            TokenKind::Ident | TokenKind::EscapedIdent => {
                let ident = self.expect_ident()?;
                Ok(FieldTarget::Name(ident))
            }
            _ => Err(self.err_expected_token("field name or index")),
        }
    }

    fn parse_postfix_op(&mut self, base: ExprId, op: PostfixOp) -> ExprId {
        let end = self.advance().span;
        let span = self.ast.exprs.get(base).span.to(end);
        self.alloc_expr(ExprKind::Postfix { expr: base, op }, span)
    }

    fn parse_type_op(&mut self, base: ExprId, is_test: bool) -> ParseResult<ExprId> {
        let _ = self.advance();
        let ty = self.parse_ty_named_only()?;
        let kind = if is_test {
            let as_name = if self.eat(&TokenKind::KwAs) {
                Some(self.expect_ident()?)
            } else {
                None
            };
            TypeOpKind::Test(as_name)
        } else {
            TypeOpKind::Cast
        };
        let span = self.ast.exprs.get(base).span.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::TypeOp {
                expr: base,
                ty,
                kind,
            },
            span,
        ))
    }

    // ── Literals ──────────────────────────────────────────────────

    fn parse_literal(&mut self) -> ParseResult<ExprId> {
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

    fn parse_fstring_lit(&mut self) -> ParseResult<ExprId> {
        let token = self.advance();
        let span = token.span;
        let lex_parts = match &token.kind {
            // Clone required: advance() returns &Token, so we cannot take ownership.
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

    fn convert_fstr_parts(
        &mut self,
        lex_parts: Vec<music_lex::FStrPart>,
    ) -> ParseResult<Vec<FStrPart>> {
        let mut out = Vec::with_capacity(lex_parts.len());
        for part in lex_parts {
            match part {
                music_lex::FStrPart::Lit(s) => out.push(FStrPart::Lit(s)),
                music_lex::FStrPart::Expr(tokens) => {
                    let expr_id = self.parse_owned_sub_tokens(tokens)?;
                    out.push(FStrPart::Expr(expr_id));
                }
            }
        }
        Ok(out)
    }

    fn parse_owned_sub_tokens(&mut self, tokens: Vec<music_lex::Token>) -> ParseResult<ExprId> {
        let mut sub_tokens = Vec::with_capacity(tokens.len() + 1);
        sub_tokens.extend(tokens);
        let eof_span = sub_tokens
            .last()
            .map_or(Span::DUMMY, |t| Span::new(t.span.end, t.span.end));
        sub_tokens.push(music_lex::Token {
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

    // ── Identifiers ───────────────────────────────────────────────

    fn parse_ident_expr(&mut self) -> ParseResult<ExprId> {
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

    fn expect_ident_or_kw_op(&mut self) -> ParseResult<Ident> {
        match self.peek_kind() {
            TokenKind::KwShl | TokenKind::KwShr => {
                let span = self.span();
                let _ = self.advance();
                Ok(self.make_ident_from_span(span))
            }
            _ => self.expect_ident(),
        }
    }

    // ── Paren forms ───────────────────────────────────────────────

    fn parse_paren(&mut self) -> ParseResult<ExprId> {
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

    fn parse_unit(&mut self, start: Span) -> ParseResult<ExprId> {
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        self.try_lambda(Vec::new(), span)
    }

    fn parse_paren_expr(&mut self, start: Span) -> ParseResult<ExprId> {
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

    fn finish_grouped_or_lambda(&mut self, inner: ExprId, start: Span) -> ParseResult<ExprId> {
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        if self.at(&TokenKind::EqGt) || self.at(&TokenKind::Colon) {
            let params = self.reinterpret_as_params(inner)?;
            return self.try_lambda(params, span);
        }
        Ok(inner)
    }

    fn parse_tuple_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
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

    fn parse_seq_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
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

    fn parse_piecewise_from(
        &mut self,
        first_arm: Option<(ExprId, PwGuard)>,
        start: Span,
    ) -> ParseResult<ExprId> {
        let mut arms = if let Some((value, guard)) = first_arm {
            vec![PiecewiseArm { value, guard }]
        } else {
            let _ = self.advance(); // eat leading |
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

    fn parse_pw_guard(&mut self) -> ParseResult<PwGuard> {
        if self.eat_wildcard() {
            return Ok(PwGuard::Wildcard);
        }
        let expr = self.parse_expr(0)?;
        Ok(PwGuard::Expr(expr))
    }

    // ── Lambda ────────────────────────────────────────────────────

    fn try_lambda(&mut self, params: ParamList, start: Span) -> ParseResult<ExprId> {
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
        let _ = self.advance(); // =>
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

    fn reinterpret_as_params(&self, expr: ExprId) -> ParseResult<ParamList> {
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

    fn reinterpret_exprs_as_params(&self, exprs: &[ExprId]) -> ParseResult<ParamList> {
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

    // ── Arrays ────────────────────────────────────────────────────

    fn parse_array(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::LBracket, "'['")?;
        if self.at(&TokenKind::RBracket) {
            let end = self.expect(&TokenKind::RBracket, "']'")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::ArrayLit(Vec::new()), span));
        }
        let first = self.parse_expr(0)?;
        self.parse_array_rest(first, start)
    }

    fn parse_array_rest(&mut self, first: ExprId, start: Span) -> ParseResult<ExprId> {
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

    fn parse_array_item(&mut self) -> ParseResult<ExprId> {
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

    fn parse_matrix_rest(&mut self, first_row: Vec<ExprId>, start: Span) -> ParseResult<ExprId> {
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

    fn parse_comprehension(&mut self, expr: ExprId, start: Span) -> ParseResult<ExprId> {
        let _ = self.advance(); // consume '|'
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

    fn parse_comp_clause(&mut self) -> ParseResult<CompClause> {
        let expr = self.parse_expr(0)?;
        if self.eat(&TokenKind::KwIn) {
            let pat = self.expr_to_pat(expr)?;
            let iter = self.parse_expr(0)?;
            Ok(CompClause::Generator { pat, iter })
        } else {
            Ok(CompClause::Filter(expr))
        }
    }

    fn expr_to_pat(&mut self, expr_id: ExprId) -> ParseResult<PatId> {
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

    // ── Record literals ───────────────────────────────────────────

    fn parse_rec_lit(&mut self) -> ParseResult<ExprId> {
        let open_span = self.expect(&TokenKind::DotLBrace, "'.{'")?;
        let fields = self.parse_rec_fields()?;
        let end =
            self.expect_closing(&TokenKind::RBrace, "'.{'", open_span, "in record literal")?;
        let span = open_span.to(end);
        Ok(self.alloc_expr(ExprKind::RecordLit(fields), span))
    }

    fn parse_rec_fields(&mut self) -> ParseResult<Vec<RecordField>> {
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

    // ── Dot-prefixed (variant) ────────────────────────────────────

    fn parse_dot_pfx(&mut self) -> ParseResult<ExprId> {
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

    // ── Let binding ───────────────────────────────────────────────

    fn parse_let(&mut self, mut modifiers: ModifierSet, attrs: AttrList) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwLet, "'let'")?;
        if self.eat(&TokenKind::KwMut) {
            modifiers.mutable = true;
        }
        let pat = self.parse_pat()?;
        let mut ty_params = self.parse_opt_bracket_params()?;
        let params = self.parse_opt_params()?;
        if ty_params.is_empty() {
            ty_params = self.parse_opt_bracket_params()?;
        }
        let mut constraints = self.parse_opt_where()?;
        let effects = self.parse_opt_with()?;
        let ret_ty = self.parse_opt_ty_annot()?;
        if constraints.is_empty() {
            constraints = self.parse_opt_where()?;
        }
        let sig = Self::build_signature(params, ty_params, constraints, effects, ret_ty);
        let value = self.parse_opt_default()?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::Let(Box::new(LetBinding {
                modifiers,
                attrs,
                pat,
                sig,
                value,
            })),
            span,
        ))
    }

    fn build_signature(
        params: Option<ParamList>,
        ty_params: IdentList,
        constraints: Vec<Constraint>,
        effects: Vec<EffectItem>,
        ret_ty: Option<music_ast::TyId>,
    ) -> Option<Box<Signature>> {
        let has_sig = params.is_some()
            || !ty_params.is_empty()
            || !constraints.is_empty()
            || !effects.is_empty()
            || ret_ty.is_some();
        if !has_sig {
            return None;
        }
        Some(Box::new(Signature {
            params: params.unwrap_or_default(),
            ty_params,
            constraints,
            effects,
            ret_ty,
        }))
    }

    fn parse_opt_params(&mut self) -> ParseResult<Option<ParamList>> {
        if !self.at(&TokenKind::LParen) {
            return Ok(None);
        }
        // Distinguish param list from call: if next-next is ':' or 'mut' it's params.
        // Actually in let context, parens after pattern are always params.
        let _ = self.advance(); // '('
        let params = self.parse_param_list()?;
        let _ = self.expect(&TokenKind::RParen, "')'")?;
        Ok(Some(params))
    }

    fn parse_param_list(&mut self) -> ParseResult<ParamList> {
        let mut params = Vec::with_capacity(4);
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            params.push(self.parse_param()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(params)
    }

    fn parse_param(&mut self) -> ParseResult<Param> {
        let mutable = self.eat(&TokenKind::KwMut);
        let name = self.expect_ident()?;
        let ty = self.parse_opt_ty_annot()?;
        let default = self.parse_opt_default()?;
        Ok(Param {
            mutable,
            name,
            ty,
            default,
        })
    }

    fn parse_opt_bracket_params(&mut self) -> ParseResult<IdentList> {
        if !self.eat(&TokenKind::LBracket) {
            return Ok(Vec::new());
        }
        let mut params = vec![self.expect_ident()?];
        while self.eat(&TokenKind::Comma) {
            if self.at(&TokenKind::RBracket) {
                break;
            }
            params.push(self.expect_ident()?);
        }
        let _ = self.expect(&TokenKind::RBracket, "']'")?;
        Ok(params)
    }

    fn parse_opt_where(&mut self) -> ParseResult<Vec<Constraint>> {
        if !self.eat(&TokenKind::KwWhere) {
            return Ok(Vec::new());
        }
        let mut constraints = vec![self.parse_constraint()?];
        while self.eat(&TokenKind::Comma) {
            if !matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                break;
            }
            constraints.push(self.parse_constraint()?);
        }
        Ok(constraints)
    }

    fn parse_constraint(&mut self) -> ParseResult<Constraint> {
        let ty = self.expect_ident()?;
        if self.eat(&TokenKind::LtColon) {
            let bound = self.parse_ty_ref()?;
            return Ok(Constraint::Subtype { ty, bound });
        }
        let _ = self.expect(&TokenKind::Colon, "':' or '<:'")?;
        let class = self.parse_ty_ref()?;
        Ok(Constraint::Implements { ty, class })
    }

    fn parse_opt_with(&mut self) -> ParseResult<Vec<EffectItem>> {
        if !self.eat(&TokenKind::KwWith) {
            return Ok(Vec::new());
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let items = self.parse_effect_list()?;
        let _ = self.expect(&TokenKind::RBrace, "'}'")?;
        Ok(items)
    }

    fn parse_effect_list(&mut self) -> ParseResult<Vec<EffectItem>> {
        let mut items = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            if self.eat(&TokenKind::DotDotDot) {
                let name = self.expect_ident()?;
                items.push(EffectItem::Rest(name));
                if self.eat(&TokenKind::Comma) {
                    continue;
                }
                break;
            }
            let name = self.expect_ident()?;
            let arg = if self.at(&TokenKind::LBracket) {
                let args = self.parse_ty_bracket_args()?;
                match args.as_slice() {
                    [] => None,
                    [arg] => Some(*arg),
                    _ => {
                        return Err(ParseError {
                            kind: ParseErrorKind::ExpectedToken {
                                expected: "at most one effect type argument",
                                found: "'['",
                            },
                            span: self.prev_span(),
                            context: Some("in effect row"),
                        });
                    }
                }
            } else if self.at(&TokenKind::KwOf) {
                return Err(ParseError {
                    kind: ParseErrorKind::TypeApplicationUsesBrackets,
                    span: self.span(),
                    context: Some("in effect row"),
                });
            } else {
                None
            };
            items.push(EffectItem::Named { name, arg });
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(items)
    }

    pub(crate) fn parse_opt_ty_annot(&mut self) -> ParseResult<Option<music_ast::TyId>> {
        if !self.eat(&TokenKind::Colon) {
            return Ok(None);
        }
        Ok(Some(self.parse_ty()?))
    }

    // ── TyRef helper ──────────────────────────────────────────────

    pub(crate) fn parse_ty_ref(&mut self) -> ParseResult<TyRef> {
        let name = self.expect_ident()?;
        let args = if self.at(&TokenKind::LBracket) {
            self.parse_ty_bracket_args()?
        } else if self.at(&TokenKind::KwOf) {
            return Err(ParseError {
                kind: ParseErrorKind::TypeApplicationUsesBrackets,
                span: self.span(),
                context: Some("in type reference"),
            });
        } else {
            Vec::new()
        };
        Ok(TyRef { name, args })
    }

    // ── Case ──────────────────────────────────────────────────────

    fn parse_case(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwCase, "'case'")?;
        let scrutinee = self.parse_expr_no_call(0)?;
        let _ = self.expect(&TokenKind::KwOf, "'of'")?;
        let open_span = self.expect(&TokenKind::LParen, "'('")?;
        let _ = self.eat(&TokenKind::Pipe); // optional leading |
        let mut arms = vec![self.parse_case_arm()?];
        while self.eat(&TokenKind::Pipe) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            arms.push(self.parse_case_arm()?);
        }
        let end =
            self.expect_closing(&TokenKind::RParen, "'('", open_span, "in case expression")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Case(Box::new(CaseData { scrutinee, arms })), span))
    }

    fn parse_case_arm(&mut self) -> ParseResult<CaseArm> {
        let attrs = self.parse_attrs()?;
        let pat = self.parse_pat()?;
        let guard = if self.eat(&TokenKind::KwIf) {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
        let body = self.parse_expr(0)?;
        Ok(CaseArm {
            attrs,
            pat,
            guard,
            body,
        })
    }

    // ── Return / Resume / Perform ─────────────────────────────────

    fn parse_return(&mut self) -> ParseResult<ExprId> {
        self.parse_keyword_expr(&TokenKind::KwReturn, "'return'", ExprKind::Return)
    }

    fn parse_resume(&mut self) -> ParseResult<ExprId> {
        self.parse_keyword_expr(&TokenKind::KwResume, "'resume'", ExprKind::Resume)
    }

    fn parse_keyword_expr(
        &mut self,
        kw: &TokenKind,
        kw_str: &'static str,
        make: fn(Option<ExprId>) -> ExprKind,
    ) -> ParseResult<ExprId> {
        let start = self.expect(kw, kw_str)?;
        let value = if self.can_start_expr() {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(make(value), span))
    }

    fn parse_perform(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwPerform, "'perform'")?;
        let expr = self.parse_expr(POSTFIX_BP)?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::Perform(expr), span))
    }

    fn can_start_expr(&self) -> bool {
        !matches!(
            self.peek_kind(),
            TokenKind::Semi
                | TokenKind::RParen
                | TokenKind::RBracket
                | TokenKind::RBrace
                | TokenKind::Pipe
                | TokenKind::Eof
        )
    }

    // ── Import ────────────────────────────────────────────────────

    fn parse_import(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwImport, "'import'")?;
        let path_str = self.expect_string()?;
        let path = self.intern(&path_str);
        let _ = self.expect(&TokenKind::KwAs, "'as'")?;
        let kind = self.parse_import_kind()?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::Import { path, kind }, span))
    }

    fn parse_import_kind(&mut self) -> ParseResult<ImportKind> {
        if self.eat_wildcard() {
            return Ok(ImportKind::Wildcard);
        }
        let name = self.expect_ident()?;
        if self.eat(&TokenKind::DotLBrace) {
            let mut members = vec![self.expect_ident()?];
            while self.eat(&TokenKind::Comma) {
                if self.at(&TokenKind::RBrace) {
                    break;
                }
                members.push(self.expect_ident()?);
            }
            let _ = self.expect(&TokenKind::RBrace, "'}'")?;
            return Ok(ImportKind::Selective(name, members));
        }
        Ok(ImportKind::Qualified(name))
    }

    fn expect_string(&mut self) -> ParseResult<String> {
        if let TokenKind::Str(s) = self.peek_kind() {
            let s = s.clone();
            let _ = self.advance();
            return Ok(s);
        }
        Err(self.err_expected_token("string literal"))
    }

    // ── Foreign ───────────────────────────────────────────────────

    fn parse_foreign(&mut self, mut modifiers: ModifierSet) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwForeign, "'foreign'")?;
        let abi = if matches!(self.peek_kind(), TokenKind::Str(_)) {
            let s = self.expect_string()?;
            Some(self.intern(&s))
        } else {
            None
        };
        modifiers.foreign_abi = abi;
        if self.at(&TokenKind::KwImport) {
            return self.parse_foreign_import(start);
        }
        if self.at(&TokenKind::KwLet) {
            return self.parse_let(modifiers, Vec::new());
        }
        if self.at(&TokenKind::LParen) {
            return self.parse_foreign_block(start, modifiers);
        }
        Err(self.err_expected_token_in("'import', 'let', or '('", Some("in foreign declaration")))
    }

    fn parse_foreign_import(&mut self, start: Span) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::KwImport, "'import'")?;
        let path_str = self.expect_string()?;
        let path = self.intern(&path_str);
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::ForeignImport(path), span))
    }

    fn parse_foreign_block(&mut self, start: Span, modifiers: ModifierSet) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let mut stmts = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            let attrs = self.parse_attrs()?;
            let binding = self.parse_foreign_binding(modifiers, attrs)?;
            stmts.push(binding);
            let _ = self.eat(&TokenKind::Semi);
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Seq(stmts), span))
    }

    fn parse_foreign_binding(
        &mut self,
        modifiers: ModifierSet,
        attrs: AttrList,
    ) -> ParseResult<ExprId> {
        let start = self.span();
        let name = self.expect_ident()?;
        let ty_params = self.parse_opt_bracket_params()?;
        let mut mods = modifiers;
        if self.eat(&TokenKind::KwAs) {
            let alias = self.expect_string()?;
            mods.foreign_alias = Some(self.intern(&alias));
        }
        let constraints = self.parse_opt_where()?;
        let ret_ty = self.parse_opt_ty_annot()?;
        let pat = self.alloc_pat(PatKind::Bind(name), name.span);
        let sig = Self::build_signature(None, ty_params, constraints, Vec::new(), ret_ty);
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::Let(Box::new(LetBinding {
                modifiers: mods,
                attrs,
                pat,
                sig,
                value: None,
            })),
            span,
        ))
    }

    // ── Type definitions ──────────────────────────────────────────

    fn parse_data_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwData, "'data'")?;
        let open_span = self.expect(&TokenKind::LBrace, "'{'")?;

        if self.at(&TokenKind::RBrace) {
            return Err(self.err_expected_token_in(
                "';' or '|' to disambiguate empty data type",
                Some("in data definition"),
            ));
        }

        if self.at(&TokenKind::Pipe) {
            let _ = self.advance();
            let mut variants = Vec::new();
            if !self.at(&TokenKind::RBrace) {
                variants.push(self.parse_variant_def()?);
                variants.extend(self.parse_variant_list()?);
            }
            let end =
                self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::DataDef(Box::new(DataBody::Sum(variants))), span));
        }

        if self.at(&TokenKind::Semi) {
            let _ = self.advance();
            let mut fields = Vec::new();
            if !self.at(&TokenKind::RBrace) {
                fields.push(self.parse_rec_def_field()?);
                while self.eat(&TokenKind::Semi) {
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    fields.push(self.parse_rec_def_field()?);
                }
            }
            let end =
                self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
            let span = start.to(end);
            return Ok(
                self.alloc_expr(ExprKind::DataDef(Box::new(DataBody::Product(fields))), span)
            );
        }

        let body = self.parse_data_body()?;
        let end =
            self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::DataDef(Box::new(body)), span))
    }

    fn parse_data_body(&mut self) -> ParseResult<DataBody> {
        let attrs = self.parse_attrs()?;
        let name = self.expect_ident()?;

        if self.eat(&TokenKind::Colon) {
            let ty = self.parse_ty()?;
            let default = self.parse_opt_default()?;

            match self.peek_kind() {
                TokenKind::Pipe => {
                    let first = VariantDef {
                        attrs,
                        name,
                        payload: Some(ty),
                        default,
                    };
                    let mut variants = vec![first];
                    variants.extend(self.parse_variant_list()?);
                    Ok(DataBody::Sum(variants))
                }
                TokenKind::Semi | TokenKind::RBrace => {
                    let first = RecordDefField { name, ty, default };
                    let mut fields = vec![first];
                    while self.eat(&TokenKind::Semi) {
                        if self.at(&TokenKind::RBrace) {
                            break;
                        }
                        fields.push(self.parse_rec_def_field()?);
                    }
                    Ok(DataBody::Product(fields))
                }
                _ => {
                    Err(self.err_expected_token_in("'|' or ';' or '}'", Some("in data definition")))
                }
            }
        } else {
            let default = self.parse_opt_default()?;
            let first = VariantDef {
                attrs,
                name,
                payload: None,
                default,
            };
            match self.peek_kind() {
                TokenKind::Pipe => {
                    let mut variants = vec![first];
                    variants.extend(self.parse_variant_list()?);
                    Ok(DataBody::Sum(variants))
                }
                TokenKind::RBrace => Ok(DataBody::Sum(vec![first])),
                _ => {
                    Err(self.err_expected_token_in("':', '|', or '}'", Some("in data definition")))
                }
            }
        }
    }

    fn parse_variant_list(&mut self) -> ParseResult<Vec<VariantDef>> {
        let mut variants = Vec::new();
        while self.eat(&TokenKind::Pipe) {
            if self.at(&TokenKind::RBrace) {
                break;
            }
            variants.push(self.parse_variant_def()?);
        }
        Ok(variants)
    }

    fn parse_variant_def(&mut self) -> ParseResult<VariantDef> {
        let attrs = self.parse_attrs()?;
        let name = self.expect_ident()?;
        let payload = if self.eat(&TokenKind::Colon) {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let default = self.parse_opt_default()?;
        Ok(VariantDef {
            attrs,
            name,
            payload,
            default,
        })
    }

    fn parse_rec_def_field(&mut self) -> ParseResult<RecordDefField> {
        let name = self.expect_ident()?;
        let _ = self.expect(&TokenKind::Colon, "':'")?;
        let ty = self.parse_ty()?;
        let default = self.parse_opt_default()?;
        Ok(RecordDefField { name, ty, default })
    }

    fn parse_effect_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwEffect, "'effect'")?;
        let (members, end) = self.parse_braced_members("in effect definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::EffectDef(members), span))
    }

    fn parse_class_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwClass, "'class'")?;
        let constraints = self.parse_opt_where()?;
        let (members, end) = self.parse_braced_members("in class definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(
            ExprKind::ClassDef(Box::new(ClassDefData {
                constraints,
                members,
            })),
            span,
        ))
    }

    fn parse_braced_members(&mut self, ctx: &'static str) -> ParseResult<(Vec<MemberDecl>, Span)> {
        let open_span = self.expect(&TokenKind::LBrace, "'{'")?;
        let members = self.parse_member_decls()?;
        let end = self.expect_closing(&TokenKind::RBrace, "'{'", open_span, ctx)?;
        Ok((members, end))
    }

    fn parse_member_decls(&mut self) -> ParseResult<Vec<MemberDecl>> {
        let mut members = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            members.push(self.parse_member_decl()?);
            let _ = self.eat(&TokenKind::Semi);
        }
        Ok(members)
    }

    fn parse_member_decl(&mut self) -> ParseResult<MemberDecl> {
        let attrs = self.parse_attrs()?;
        if self.at(&TokenKind::KwLaw) {
            return Ok(MemberDecl::Law(self.parse_law_decl()?));
        }
        Ok(MemberDecl::Fn(self.parse_fn_decl(attrs)?))
    }

    fn parse_fn_decl(&mut self, attrs: AttrList) -> ParseResult<FnDecl> {
        let _ = self.expect(&TokenKind::KwLet, "'let'")?;
        let name = self.parse_member_name()?;
        let params = self.parse_opt_params()?;
        let ret_ty = self.parse_opt_ty_annot()?;
        let body = self.parse_opt_default()?;
        Ok(FnDecl {
            attrs,
            name,
            params,
            ret_ty,
            body,
        })
    }

    fn parse_member_name(&mut self) -> ParseResult<MemberName> {
        if self.at(&TokenKind::LParen) {
            let _ = self.advance();
            if self.eat_wildcard() {
                // (_ op _) — infix
                let ident = self.parse_op_ident()?;
                let _ = self.expect_wildcard("'_' after operator in (_ op _)")?;
                let _ = self.expect(&TokenKind::RParen, "')'")?;
                return Ok(MemberName::Op(ident, OpFixity::Infix));
            }
            // (op _) — prefix
            let ident = self.parse_op_ident()?;
            let _ = self.expect_wildcard("'_' after operator in (op _)")?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            return Ok(MemberName::Op(ident, OpFixity::Prefix));
        }
        let ident = self.expect_ident()?;
        Ok(MemberName::Ident(ident))
    }

    fn expect_wildcard(&mut self, expected: &'static str) -> ParseResult<Span> {
        if self.eat_wildcard() {
            Ok(self.prev_span())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected,
                    found: describe_token(&self.peek().kind),
                },
                span: self.span(),
                context: None,
            })
        }
    }

    fn is_op_token(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::SlashEq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::LtEq
                | TokenKind::GtEq
                | TokenKind::ColonColon
                | TokenKind::KwShl
                | TokenKind::KwShr
        )
    }

    fn parse_op_ident(&mut self) -> ParseResult<Ident> {
        if !self.is_op_token() {
            return Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: "operator name",
                    found: describe_token(&self.peek().kind),
                },
                span: self.span(),
                context: None,
            });
        }
        let span = self.span();
        let _ = self.advance();
        Ok(self.make_ident_from_span(span))
    }

    fn parse_law_decl(&mut self) -> ParseResult<LawDecl> {
        let _ = self.expect(&TokenKind::KwLaw, "'law'")?;
        let name = self.expect_ident()?;
        let params = self.parse_opt_params()?;
        let _ = self.expect(&TokenKind::ColonEq, "':='")?;
        let body = self.parse_expr(0)?;
        Ok(LawDecl { name, params, body })
    }

    // ── Instance ──────────────────────────────────────────────────

    fn parse_instance_def(&mut self, exported: bool, attrs: AttrList) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwInstance, "'instance'")?;
        let ty_params = self.parse_opt_bracket_params()?;
        let constraints = self.parse_opt_where()?;
        let ty = self.parse_ty_ref()?;
        let body = self.parse_instance_body()?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::InstanceDef(Box::new(InstanceDef {
                attrs,
                exported,
                ty_params,
                constraints,
                ty,
                body,
            })),
            span,
        ))
    }

    fn parse_instance_body(&mut self) -> ParseResult<InstanceBody> {
        if self.eat(&TokenKind::KwVia) {
            let via = self.parse_ty_ref()?;
            return Ok(InstanceBody::Via(via));
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let members = self.parse_member_decls()?;
        let _ = self.expect(&TokenKind::RBrace, "'}'")?;
        Ok(InstanceBody::Methods(members))
    }

    // ── Handle ────────────────────────────────────────────────────

    fn parse_handle(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwHandle, "'handle'")?;
        let body = self.parse_expr(0)?;
        let _ = self.expect(&TokenKind::KwWith, "'with'")?;
        let name = self.expect_ident()?;
        let args = if self.at(&TokenKind::LBracket) {
            self.parse_ty_bracket_args()?
        } else {
            Vec::new()
        };
        let effect = TyRef { name, args };
        let _ = self.expect(&TokenKind::KwOf, "'of'")?;
        let open = self.expect(&TokenKind::LParen, "'('")?;
        let mut clauses = Vec::new();
        let _ = self.eat(&TokenKind::Pipe);
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            clauses.push(self.parse_handle_clause()?);
            if !self.eat(&TokenKind::Pipe) {
                break;
            }
        }
        let end = self.expect_closing(&TokenKind::RParen, "'('", open, "in handle expression")?;
        let span = start.to(end);
        Ok(self.alloc_expr(
            ExprKind::Handle(Box::new(HandleData {
                effect,
                body,
                clauses,
            })),
            span,
        ))
    }

    fn parse_handle_clause(&mut self) -> ParseResult<HandlerClause> {
        if self.at(&TokenKind::KwReturn) {
            let _ = self.advance();
            let binder = self.expect_ident()?;
            let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
            let body = self.parse_expr(0)?;
            return Ok(HandlerClause::Return { binder, body });
        }

        let name = self.expect_ident_or_kw_op()?;
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let mut binders = Vec::new();
        if !self.at(&TokenKind::RParen) {
            binders.push(self.expect_ident()?);
            while self.eat(&TokenKind::Comma) {
                binders.push(self.expect_ident()?);
            }
        }
        let _ = self.expect(&TokenKind::RParen, "')'")?;
        let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
        let body = self.parse_expr(0)?;
        let Some(cont) = binders.pop() else {
            return Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: "handler continuation binder",
                    found: "')'",
                },
                span: name.span,
                context: Some("handler operation clauses require at least a continuation binder"),
            });
        };
        Ok(HandlerClause::Op {
            name,
            args: binders,
            cont,
            body,
        })
    }

    // ── Quote / Splice ────────────────────────────────────────────

    fn parse_quote(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwQuote, "'quote'")?;
        if self.at(&TokenKind::LParen) {
            let _ = self.advance();
            let expr = self.parse_expr(0)?;
            let end = self.expect(&TokenKind::RParen, "')'")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::Quote(QuoteKind::Expr(expr)), span));
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let mut stmts = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            let expr = self.parse_expr(0)?;
            let _ = self.expect(&TokenKind::Semi, "';'")?;
            stmts.push(expr);
        }
        let end = self.expect(&TokenKind::RBrace, "'}'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Quote(QuoteKind::Block(stmts)), span))
    }

    fn parse_splice(&mut self) -> ParseResult<ExprId> {
        let start = self.span();
        match self.peek_kind().clone() {
            TokenKind::Hash => {
                let _ = self.advance();
                let ident = self.expect_ident()?;
                let span = start.to(ident.span);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Ident(ident)), span))
            }
            TokenKind::HashLParen => {
                let _ = self.advance();
                let expr = self.parse_expr(0)?;
                let end = self.expect(&TokenKind::RParen, "')'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Expr(expr)), span))
            }
            TokenKind::HashLBracket => {
                let _ = self.advance();
                let exprs = self.parse_expr_list(&TokenKind::RBracket)?;
                let end = self.expect(&TokenKind::RBracket, "']'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Array(exprs)), span))
            }
            _ => Err(self.err_expected_expr()),
        }
    }

    // ── Attrs ─────────────────────────────────────────────────────

    fn parse_with_attrs(&mut self) -> ParseResult<ExprId> {
        let attrs = self.parse_attrs()?;
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_export_with_attrs(attrs),
            TokenKind::KwLet => self.parse_let(ModifierSet::default(), attrs),
            TokenKind::KwInstance => self.parse_instance_def(false, attrs),
            _ => Err(self
                .err_expected_token_in("'export', 'let', or 'instance'", Some("after attribute"))),
        }
    }

    fn parse_attrs(&mut self) -> ParseResult<AttrList> {
        let mut attrs = Vec::new();
        while self.at(&TokenKind::At) {
            attrs.push(self.parse_attr()?);
        }
        Ok(attrs)
    }

    fn parse_attr(&mut self) -> ParseResult<music_ast::AttrId> {
        let start = self.expect(&TokenKind::At, "'@'")?;
        let path = self.parse_attr_path()?;
        let args = if self.eat(&TokenKind::LParen) {
            let a = self.parse_attr_args()?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            a
        } else {
            Vec::new()
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_attr(Attr { path, args }, span))
    }

    fn parse_attr_path(&mut self) -> ParseResult<IdentList> {
        let mut path = vec![self.expect_ident()?];
        while self.eat(&TokenKind::Dot) {
            path.push(self.expect_ident()?);
        }
        Ok(path)
    }

    fn parse_attr_args(&mut self) -> ParseResult<Vec<AttrArg>> {
        let mut args = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            args.push(self.parse_attr_arg()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(args)
    }

    fn parse_attr_arg(&mut self) -> ParseResult<AttrArg> {
        // Look ahead: if ident followed by :=, it's named
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            let saved_pos = self.pos;
            let name = self.expect_ident()?;
            if self.eat(&TokenKind::ColonEq) {
                let value = self.parse_expr(0)?;
                return Ok(AttrArg::Named { name, value });
            }
            // Not named, backtrack
            self.pos = saved_pos;
        }
        let value = self.parse_expr(0)?;
        Ok(AttrArg::Positional(value))
    }

    // ── Export ─────────────────────────────────────────────────────

    fn parse_export(&mut self) -> ParseResult<ExprId> {
        self.parse_export_with_attrs(Vec::new())
    }

    fn parse_export_with_attrs(&mut self, attrs: AttrList) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::KwExport, "'export'")?;
        if self.at(&TokenKind::KwInstance) {
            return self.parse_instance_def(true, attrs);
        }
        let mut modifiers = ModifierSet {
            exported: true,
            ..ModifierSet::default()
        };
        if self.eat(&TokenKind::KwOpaque) {
            modifiers.opaque = true;
        }
        if self.at(&TokenKind::KwForeign) {
            return self.parse_foreign(modifiers);
        }
        self.parse_let(modifiers, attrs)
    }

    // ── Helpers ───────────────────────────────────────────────────

    fn parse_opt_default(&mut self) -> ParseResult<Option<ExprId>> {
        if self.eat(&TokenKind::ColonEq) {
            Ok(Some(self.parse_expr(0)?))
        } else {
            Ok(None)
        }
    }

    fn parse_expr_list(&mut self, terminator: &TokenKind) -> ParseResult<ExprList> {
        let mut exprs = Vec::with_capacity(4);
        while !self.at(terminator) && !self.at_eof() {
            exprs.push(self.parse_expr(0)?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(exprs)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
