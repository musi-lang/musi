mod atoms;
mod bindings;
mod control;
mod decls;

use music_ast::common::{ModifierSet, TyRef};
use music_ast::expr::{
    AccessMode, BinOp, ExprKind, FieldTarget, IndexKind, PostfixOp, TypeOpKind, UnaryOp,
};
use music_ast::{ExprId, ExprList, TyId};
use music_lex::TokenKind;
use music_shared::Span;

use crate::errors::{ParseError, ParseErrorKind, ParseResult};
use crate::parser::Parser;

const PREFIX_BP: u8 = 22;
const ASSIGN_BP: u8 = 2;
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
    pub(crate) fn parse_expr(&mut self, min_bp: u8) -> ParseResult<ExprId> {
        self.parse_expr_inner(min_bp, true)
    }

    pub(super) fn parse_expr_no_call(&mut self, min_bp: u8) -> ParseResult<ExprId> {
        self.parse_expr_inner(min_bp, false)
    }

    pub(super) fn parse_expr_inner(&mut self, min_bp: u8, allow_call: bool) -> ParseResult<ExprId> {
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

    pub(super) fn parse_prefix(&mut self) -> ParseResult<ExprId> {
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

    pub(super) fn parse_unary(&mut self, op: UnaryOp, start: Span) -> ParseResult<ExprId> {
        let operand = self.parse_expr(PREFIX_BP)?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::UnaryOp(op, operand), span))
    }

    pub(super) fn parse_atom(&mut self) -> ParseResult<ExprId> {
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
            TokenKind::KwForeign => self.parse_foreign(ModifierSet::default(), Vec::new()),
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

    pub(super) fn peek_infix_bp(&self) -> Option<(BinOp, u8, u8)> {
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

    pub(super) fn try_assign(&mut self, left: ExprId) -> ParseResult<Option<ExprId>> {
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

    pub(super) fn try_postfix(
        &mut self,
        left: ExprId,
        allow_call: bool,
    ) -> ParseResult<Option<ExprId>> {
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

    pub(super) fn parse_call(&mut self, callee: ExprId) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let args = self.parse_expr_list(&TokenKind::RParen)?;
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = self.ast.exprs.get(callee).span.to(end);
        Ok(self.alloc_expr(ExprKind::App(callee, args), span))
    }

    pub(super) fn parse_index(&mut self, base: ExprId) -> ParseResult<ExprId> {
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

    pub(super) fn parse_record_update(&mut self, base: ExprId) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::DotLBrace, "'.{'")?;
        let fields = self.parse_rec_fields()?;
        let end = self.expect(&TokenKind::RBrace, "'}'")?;
        let span = self.ast.exprs.get(base).span.to(end);
        Ok(self.alloc_expr(ExprKind::RecordUpdate { base, fields }, span))
    }

    pub(super) fn parse_access(&mut self, base: ExprId, mode: AccessMode) -> ParseResult<ExprId> {
        let _ = self.advance();
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

    pub(super) fn parse_field_target(&mut self) -> ParseResult<FieldTarget> {
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

    pub(super) fn parse_postfix_op(&mut self, base: ExprId, op: PostfixOp) -> ExprId {
        let end = self.advance().span;
        let span = self.ast.exprs.get(base).span.to(end);
        self.alloc_expr(ExprKind::Postfix { expr: base, op }, span)
    }

    pub(super) fn parse_type_op(&mut self, base: ExprId, is_test: bool) -> ParseResult<ExprId> {
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

    pub(super) fn parse_opt_ty_annot(&mut self) -> ParseResult<Option<TyId>> {
        if !self.eat(&TokenKind::Colon) {
            return Ok(None);
        }
        Ok(Some(self.parse_ty()?))
    }

    pub(super) fn parse_ty_ref(&mut self) -> ParseResult<TyRef> {
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

    pub(super) fn parse_opt_default(&mut self) -> ParseResult<Option<ExprId>> {
        if self.eat(&TokenKind::ColonEq) {
            Ok(Some(self.parse_expr(0)?))
        } else {
            Ok(None)
        }
    }

    pub(super) fn parse_expr_list(&mut self, terminator: &TokenKind) -> ParseResult<ExprList> {
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
