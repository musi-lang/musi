use music_ast::common::{Constraint, EffectItem, ModifierSet, Param, Signature};
use music_ast::expr::{ExprKind, LetBinding};
use music_ast::{AttrList, ExprId, IdentList, ParamList, TyId};
use music_lex::TokenKind;

use crate::errors::{ParseError, ParseErrorKind, ParseResult};
use crate::parser::Parser;

impl Parser<'_> {
    pub(super) fn parse_let(
        &mut self,
        mut modifiers: ModifierSet,
        attrs: AttrList,
    ) -> ParseResult<ExprId> {
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

    pub(super) fn build_signature(
        params: Option<ParamList>,
        ty_params: IdentList,
        constraints: Vec<Constraint>,
        effects: Vec<EffectItem>,
        ret_ty: Option<TyId>,
    ) -> Option<Box<Signature>> {
        let has_param_list = params.is_some();
        let has_sig = params.is_some()
            || !ty_params.is_empty()
            || !constraints.is_empty()
            || !effects.is_empty()
            || ret_ty.is_some();
        if !has_sig {
            return None;
        }
        Some(Box::new(Signature {
            has_param_list,
            params: params.unwrap_or_default(),
            ty_params,
            constraints,
            effects,
            ret_ty,
        }))
    }

    pub(super) fn parse_opt_params(&mut self) -> ParseResult<Option<ParamList>> {
        if !self.at(&TokenKind::LParen) {
            return Ok(None);
        }
        let _ = self.advance();
        let params = self.parse_param_list()?;
        let _ = self.expect(&TokenKind::RParen, "')'")?;
        Ok(Some(params))
    }

    pub(super) fn parse_param_list(&mut self) -> ParseResult<ParamList> {
        let mut params = Vec::with_capacity(4);
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            params.push(self.parse_param()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(params)
    }

    pub(super) fn parse_param(&mut self) -> ParseResult<Param> {
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

    pub(super) fn parse_opt_bracket_params(&mut self) -> ParseResult<IdentList> {
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

    pub(super) fn parse_opt_where(&mut self) -> ParseResult<Vec<Constraint>> {
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

    pub(super) fn parse_constraint(&mut self) -> ParseResult<Constraint> {
        let ty = self.expect_ident()?;
        if self.eat(&TokenKind::LtColon) {
            let bound = self.parse_ty_ref()?;
            return Ok(Constraint::Subtype { ty, bound });
        }
        let _ = self.expect(&TokenKind::Colon, "':' or '<:'")?;
        let class = self.parse_ty_ref()?;
        Ok(Constraint::Implements { ty, class })
    }

    pub(super) fn parse_opt_with(&mut self) -> ParseResult<Vec<EffectItem>> {
        if !self.eat(&TokenKind::KwWith) {
            return Ok(Vec::new());
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let items = self.parse_effect_list()?;
        let _ = self.expect(&TokenKind::RBrace, "'}'")?;
        Ok(items)
    }

    pub(super) fn parse_effect_list(&mut self) -> ParseResult<Vec<EffectItem>> {
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
}
