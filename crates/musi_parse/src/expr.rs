use musi_ast::{
    Attr, AttrArg, AttrArgList, AttrList, Expr, ExprKind, ExprList, Field, FieldList, FnSig,
    LitKind, MatchCase, Modifiers, Stmt, StmtKind, SumCase, SumCaseItem, SumCaseItemList,
    TemplatePart,
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
    ///
    /// # Panics
    /// Never panics - `.expect()` is guarded by prior `Prec::prefix`/`Prec::infix` check.
    pub fn parse_expr_bp(&mut self, min_bp: u8) -> MusiResult<Expr> {
        let start = self.curr_span();

        let mut lhs = if let Some(bp) = self.peek_kind().and_then(Prec::prefix) {
            let op = self.advance().expect("checked by `Prec::prefix`").kind;
            let operand = Box::new(self.parse_expr_bp(bp)?);
            Expr {
                kind: ExprKind::Unary { op, operand },
                span: start.merge(self.prev_span()),
            }
        } else {
            self.parse_expr_primary()?
        };

        loop {
            if let Some(l_bp) = self.peek_kind().and_then(Prec::postfix) {
                if l_bp < min_bp {
                    break;
                }
                lhs = self.parse_expr_postfix(lhs)?;
                continue;
            }

            if self.at(TokenKind::KwNot) && self.peek_nth(1) == Some(TokenKind::KwIn) {
                if 10 < min_bp {
                    break;
                }
                self.advance_by(2);
                let rhs = Box::new(self.parse_expr_bp(11)?);
                let inner = Box::new(Expr {
                    kind: ExprKind::Binary {
                        op: TokenKind::KwIn,
                        lhs: Box::new(lhs),
                        rhs,
                    },
                    span: start.merge(self.prev_span()),
                });
                lhs = Expr {
                    kind: ExprKind::Unary {
                        op: TokenKind::KwNot,
                        operand: inner,
                    },
                    span: start.merge(self.prev_span()),
                };
                continue;
            }

            let Some((l_bp, r_bp)) = self.peek_kind().and_then(Prec::infix) else {
                break;
            };
            if l_bp < min_bp {
                break;
            }

            let op = self.advance().expect("checked by Prec::infix").kind;

            if matches!(op, TokenKind::DotDot | TokenKind::DotDotLt) {
                let end = if self.can_start_expr() {
                    Some(Box::new(self.parse_expr_bp(r_bp)?))
                } else {
                    None
                };
                lhs = Expr {
                    kind: ExprKind::Range {
                        start: Box::new(lhs),
                        end,
                        inclusive: op == TokenKind::DotDot,
                    },
                    span: start.merge(self.prev_span()),
                };
                continue;
            }

            let rhs = Box::new(self.parse_expr_bp(r_bp)?);
            let span = start.merge(rhs.span);
            lhs = if op == TokenKind::LtMinus {
                Expr {
                    kind: ExprKind::Assign {
                        target: Box::new(lhs),
                        value: rhs,
                    },
                    span,
                }
            } else {
                Expr {
                    kind: ExprKind::Binary {
                        op,
                        lhs: Box::new(lhs),
                        rhs,
                    },
                    span,
                }
            };
        }
        Ok(lhs)
    }

    fn parse_expr_postfix(&mut self, lhs: Expr) -> MusiResult<Expr> {
        let start = lhs.span;
        match self.peek_kind() {
            Some(TokenKind::LParen) => {
                let _ = self.advance();
                let args = self.parse_expr_args()?;
                let _ = self.expect(TokenKind::RParen)?;
                Ok(Expr {
                    kind: ExprKind::Call {
                        callee: Box::new(lhs),
                        args,
                    },
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::LBrack) => {
                let _ = self.advance();
                let index = Box::new(self.parse_expr()?);
                let _ = self.expect(TokenKind::RBrack)?;
                Ok(Expr {
                    kind: ExprKind::Index {
                        base: Box::new(lhs),
                        index,
                    },
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::Dot) => {
                let _ = self.advance();
                let field = self.expect_ident()?;
                Ok(Expr {
                    kind: ExprKind::Field {
                        base: Box::new(lhs),
                        field,
                    },
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::DotCaret) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Deref(Box::new(lhs)),
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::Question) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Try {
                        expr: Box::new(lhs),
                        else_binding: None,
                        else_body: None,
                    },
                    span: start.merge(self.prev_span()),
                })
            }
            _ => Ok(lhs),
        }
    }

    fn parse_expr_primary(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::LitInt(id)) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::Int(i64::from(id))),
                    span: start,
                })
            }
            Some(TokenKind::LitReal(id)) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::Real(f64::from(id))),
                    span: start,
                })
            }
            Some(TokenKind::LitString(id) | TokenKind::LitTemplateNoSubst(id)) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::String(id)),
                    span: start,
                })
            }
            Some(TokenKind::LitRune(c)) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::Rune(c)),
                    span: start,
                })
            }
            Some(TokenKind::KwTrue) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::Bool(true)),
                    span: start,
                })
            }
            Some(TokenKind::KwFalse) => {
                let _ = self.advance();
                Ok(Expr {
                    kind: ExprKind::Lit(LitKind::Bool(false)),
                    span: start,
                })
            }
            Some(TokenKind::TemplateHead(_)) => self.parse_expr_template(),

            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                if self.at(TokenKind::Dot) && self.peek_nth(1) == Some(TokenKind::LBrace) {
                    self.advance_by(2);
                    let fields = self.parse_expr_field_list()?;
                    let _ = self.expect(TokenKind::RBrace)?;
                    return Ok(Expr {
                        kind: ExprKind::Record {
                            ty: Some(id),
                            fields,
                        },
                        span: start.merge(self.prev_span()),
                    });
                }
                Ok(Expr {
                    kind: ExprKind::Ident(id),
                    span: start,
                })
            }
            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                self.advance_by(2);
                let fields = self.parse_expr_field_list()?;
                let _ = self.expect(TokenKind::RBrace)?;
                Ok(Expr {
                    kind: ExprKind::Record { ty: None, fields },
                    span: start.merge(self.prev_span()),
                })
            }

            Some(TokenKind::LParen) => self.parse_expr_tuple_or_grouped(),
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
                Ok(Expr {
                    kind: ExprKind::Cycle,
                    span: start,
                })
            }
            Some(TokenKind::KwUnsafe) => self.parse_expr_unsafe(),
            Some(TokenKind::KwImport) => self.parse_expr_import(),

            Some(TokenKind::LBrackLt) => self.parse_expr_with_attrs(),
            Some(TokenKind::KwExport | TokenKind::KwExtern) => {
                self.parse_expr_with_modifiers(vec![], start)
            }
            Some(TokenKind::KwRecord) => self.parse_expr_record(vec![], Modifiers::default()),
            Some(TokenKind::KwSum) => self.parse_expr_sum(vec![], Modifiers::default()),
            Some(TokenKind::KwAlias) => self.parse_expr_alias(vec![], Modifiers::default()),
            Some(TokenKind::KwFn) => self.parse_expr_fn(vec![], Modifiers::default()),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(Modifiers::default()),

            Some(kind) => {
                Err(ParseErrorKind::Unexpected(kind.as_str().into()).into_musi_error(start))
            }
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_expr_tuple_or_grouped(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.at(TokenKind::RParen) {
            let _ = self.advance();
            return Ok(Expr {
                kind: ExprKind::Tuple(vec![]),
                span: start.merge(self.prev_span()),
            });
        }
        let first = self.parse_expr()?;
        if self.at(TokenKind::Comma) {
            let _ = self.advance();
            let mut elems = vec![first];
            if !self.at(TokenKind::RParen) {
                elems.extend(self.parse_expr_list()?);
            }
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Expr {
                kind: ExprKind::Tuple(elems),
                span: start.merge(self.prev_span()),
            })
        } else {
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Expr {
                kind: first.kind,
                span: start.merge(self.prev_span()),
            })
        }
    }

    fn parse_expr_array(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.advance();
        let elems = if self.at(TokenKind::RBrack) {
            vec![]
        } else {
            self.parse_expr_list()?
        };
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(Expr {
            kind: ExprKind::Array(elems),
            span: start.merge(self.prev_span()),
        })
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_expr_block(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::LBrace)?;
        let mut stmts = vec![];
        let mut final_expr = None;
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            let expr = self.parse_expr()?;
            if self.at(TokenKind::Semicolon) {
                let _ = self.advance();
                stmts.push(Stmt {
                    kind: StmtKind::Expr(expr),
                    span: self.prev_span(),
                });
            } else {
                final_expr = Some(Box::new(expr));
                break;
            }
        }
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::Block {
                stmts,
                expr: final_expr,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_if(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwIf)?;
        let cond = Box::new(self.parse_expr()?);
        let then_br = Box::new(self.parse_expr_block()?);
        let else_br = if self.at(TokenKind::KwElse) {
            let _ = self.advance();
            Some(Box::new(if self.at(TokenKind::KwIf) {
                self.parse_expr_if()?
            } else {
                self.parse_expr_block()?
            }))
        } else {
            None
        };
        Ok(Expr {
            kind: ExprKind::If {
                cond,
                then_br,
                else_br,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_while(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwWhile)?;
        let cond = Box::new(self.parse_expr()?);
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr {
            kind: ExprKind::While { cond, body },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_for(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFor)?;
        let pat = self.parse_pat()?;
        let _ = self.expect(TokenKind::KwIn)?;
        let iter = Box::new(self.parse_expr()?);
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr {
            kind: ExprKind::For { pat, iter, body },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_match(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwMatch)?;
        let scrutinee = Box::new(self.parse_expr()?);
        let _ = self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            let _ = self.expect(TokenKind::KwCase)?;
            let pat = self.parse_pat()?;
            let guard = if self.at(TokenKind::KwIf) {
                let _ = self.advance();
                Some(self.parse_expr()?)
            } else {
                None
            };
            let _ = self.expect(TokenKind::EqGt)?;
            let body = self.parse_expr()?;
            cases.push(MatchCase { pat, guard, body });
            if self.at(TokenKind::Comma) {
                let _ = self.advance();
            }
        }
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::Match { scrutinee, cases },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_try(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwTry)?;
        let expr = Box::new(self.parse_expr()?);
        let (else_binding, else_body) = if self.at(TokenKind::KwElse) {
            let _ = self.advance();
            let bind = if let Some(TokenKind::Ident(id)) = self.peek_kind() {
                if self.at(TokenKind::LBrace) {
                    None
                } else {
                    let _ = self.advance();
                    Some(id)
                }
            } else {
                None
            };
            (bind, Some(Box::new(self.parse_expr()?)))
        } else {
            (None, None)
        };
        Ok(Expr {
            kind: ExprKind::Try {
                expr,
                else_binding,
                else_body,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_return(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwReturn)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        Ok(Expr {
            kind: ExprKind::Return(expr),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_defer(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwDefer)?;
        Ok(Expr {
            kind: ExprKind::Defer(Box::new(self.parse_expr()?)),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_break(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwBreak)?;
        let expr = if self.can_start_expr() && !self.at(TokenKind::Semicolon) {
            Some(Box::new(self.parse_expr()?))
        } else {
            None
        };
        Ok(Expr {
            kind: ExprKind::Break(expr),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_unsafe(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwUnsafe)?;
        Ok(Expr {
            kind: ExprKind::Unsafe(Box::new(self.parse_expr_block()?)),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_import(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwImport)?;
        let Some(TokenKind::LitString(path)) = self.peek_kind() else {
            return Err(
                ParseErrorKind::Expected("string literal").into_musi_error(self.curr_span())
            );
        };
        let _ = self.advance();
        Ok(Expr {
            kind: ExprKind::Import(path),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_template(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let Some(TokenKind::TemplateHead(id)) = self.peek_kind() else {
            return Err(ParseErrorKind::Expected("template string").into_musi_error(start));
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
                    return Err(ParseErrorKind::Unclosed("template string")
                        .into_musi_error(self.curr_span()));
                }
            }
        }
        Ok(Expr {
            kind: ExprKind::Lit(LitKind::Template(parts)),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_with_attrs(&mut self) -> MusiResult<Expr> {
        let start = self.curr_span();
        let attrs = self.parse_attrs()?;
        self.parse_expr_with_modifiers(attrs, start)
    }

    fn parse_expr_with_modifiers(&mut self, attrs: AttrList, start: Span) -> MusiResult<Expr> {
        let mods = self.parse_modifiers();
        match self.peek_kind() {
            Some(TokenKind::KwRecord) => self.parse_expr_record(attrs, mods),
            Some(TokenKind::KwSum) => self.parse_expr_sum(attrs, mods),
            Some(TokenKind::KwAlias) => self.parse_expr_alias(attrs, mods),
            Some(TokenKind::KwFn) => self.parse_expr_fn(attrs, mods),
            Some(TokenKind::KwVal | TokenKind::KwVar) => self.parse_expr_bind(mods),
            _ => Err(ParseErrorKind::Expected("definition").into_musi_error(start)),
        }
    }

    fn parse_attrs(&mut self) -> MusiResult<AttrList> {
        let mut attrs = vec![];
        while self.at(TokenKind::LBrackLt) {
            let _ = self.advance();
            loop {
                let name = self.expect_ident()?;
                let args = if self.at(TokenKind::LParen) {
                    let _ = self.advance();
                    let a = self.parse_attr_args()?;
                    let _ = self.advance();
                    a
                } else {
                    vec![]
                };
                attrs.push(Attr { name, args });
                if !self.at(TokenKind::Comma) {
                    break;
                }
                let _ = self.advance();
            }
            let _ = self.expect(TokenKind::GtRBrack)?;
        }
        Ok(attrs)
    }

    fn parse_attr_args(&mut self) -> MusiResult<AttrArgList> {
        let mut args = vec![];
        while !self.at(TokenKind::RParen) && !self.is_eof() {
            if let Some(TokenKind::Ident(id)) = self.peek_kind() {
                if self.peek_nth(1) == Some(TokenKind::ColonEq) {
                    self.advance_by(2);
                    args.push(AttrArg {
                        name: Some(id),
                        value: Some(self.parse_expr()?),
                        lit: None,
                    });
                } else {
                    let _ = self.advance();
                    args.push(AttrArg {
                        name: Some(id),
                        value: None,
                        lit: None,
                    });
                }
            } else {
                args.push(AttrArg {
                    name: None,
                    value: None,
                    lit: Some(self.parse_lit()?),
                });
            }
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        Ok(args)
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

    fn parse_expr_record(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwRecord)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let _ = self.expect(TokenKind::LBrace)?;
        let fields = self.parse_field_list_semi()?;
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::RecordDef {
                attrs,
                mods,
                name,
                ty_params,
                fields,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_sum(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwSum)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let _ = self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        while self.at(TokenKind::KwCase) {
            let _ = self.advance();
            let case_name = self.expect_ident()?;
            let ty_args = self.parse_typ_args()?;
            let fields = if self.at(TokenKind::LParen) {
                let _ = self.advance();
                let items = self.parse_sum_case_items()?;
                let _ = self.expect(TokenKind::RParen)?;
                items
            } else {
                vec![]
            };
            cases.push(SumCase {
                name: case_name,
                ty_args,
                fields,
            });
            if self.at(TokenKind::Comma) {
                let _ = self.advance();
            }
        }
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Expr {
            kind: ExprKind::SumDef {
                attrs,
                mods,
                name,
                ty_params,
                cases,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_sum_case_items(&mut self) -> MusiResult<SumCaseItemList> {
        let mut items = vec![];
        while !self.at(TokenKind::RParen) && !self.is_eof() {
            if self.at(TokenKind::KwVar)
                || (matches!(self.peek_kind(), Some(TokenKind::Ident(_)))
                    && matches!(
                        self.peek_nth(1),
                        Some(TokenKind::Colon | TokenKind::ColonEq)
                    ))
            {
                items.push(SumCaseItem::Field(self.parse_field()?));
            } else {
                items.push(SumCaseItem::Type(self.parse_typ()?));
            }
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        Ok(items)
    }

    fn parse_expr_alias(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwAlias)?;
        let name = self.expect_ident()?;
        let ty_params = self.parse_typ_params()?;
        let _ = self.expect(TokenKind::ColonEq)?;
        let ty = self.parse_typ()?;
        Ok(Expr {
            kind: ExprKind::Alias {
                attrs,
                mods,
                name,
                ty_params,
                ty,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_fn(&mut self, attrs: AttrList, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let _ = self.expect(TokenKind::KwFn)?;
        let name = self.try_ident();
        let ty_params = self.parse_typ_params()?;
        let params = if self.at(TokenKind::LParen) {
            let _ = self.advance();
            let p = self.parse_field_list()?;
            let _ = self.expect(TokenKind::RParen)?;
            p
        } else {
            vec![]
        };
        let ret = if self.at(TokenKind::Colon) {
            let _ = self.advance();
            Some(self.parse_typ()?)
        } else {
            None
        };
        let body = Box::new(self.parse_expr_block()?);
        Ok(Expr {
            kind: ExprKind::Fn {
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
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_bind(&mut self, mods: Modifiers) -> MusiResult<Expr> {
        let start = self.curr_span();
        let mutable = self.at(TokenKind::KwVar);
        let _ = self.advance();
        let pat = self.parse_pat()?;
        let ty = if self.at(TokenKind::Colon) {
            let _ = self.advance();
            Some(self.parse_typ()?)
        } else {
            None
        };
        let _ = self.expect(TokenKind::ColonEq)?;
        let init = Box::new(self.parse_expr()?);
        Ok(Expr {
            kind: ExprKind::Bind {
                mods,
                mutable,
                pat,
                ty,
                init,
            },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_expr_list(&mut self) -> MusiResult<ExprList> {
        let mut exprs = vec![self.parse_expr()?];
        while self.at(TokenKind::Comma) {
            let _ = self.advance();
            if self.at(TokenKind::RParen)
                || self.at(TokenKind::RBrack)
                || self.at(TokenKind::RBrace)
            {
                break;
            }
            exprs.push(self.parse_expr()?);
        }
        Ok(exprs)
    }

    fn parse_expr_args(&mut self) -> MusiResult<ExprList> {
        if self.at(TokenKind::RParen) {
            Ok(vec![])
        } else {
            self.parse_expr_list()
        }
    }

    fn parse_field_list(&mut self) -> MusiResult<FieldList> {
        let mut fields = vec![];
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::RBrace) && !self.is_eof() {
            fields.push(self.parse_field()?);
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        Ok(fields)
    }

    fn parse_field_list_semi(&mut self) -> MusiResult<FieldList> {
        let mut fields = vec![];
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            fields.push(self.parse_field()?);
            if !self.at(TokenKind::Semicolon) {
                break;
            }
            let _ = self.advance();
        }
        Ok(fields)
    }

    fn parse_expr_field_list(&mut self) -> MusiResult<FieldList> {
        let mut fields = vec![];
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            fields.push(self.parse_field()?);
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        Ok(fields)
    }

    fn parse_field(&mut self) -> MusiResult<Field> {
        let mutable = if self.at(TokenKind::KwVar) {
            let _ = self.advance();
            true
        } else {
            false
        };
        let name = self.expect_ident()?;
        let ty = if self.at(TokenKind::Colon) {
            let _ = self.advance();
            Some(self.parse_typ()?)
        } else {
            None
        };
        let init = if self.at(TokenKind::ColonEq) {
            let _ = self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };
        Ok(Field {
            mutable,
            name,
            ty,
            init,
        })
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
            Some(TokenKind::LitString(id)) => {
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
            _ => Err(ParseErrorKind::Expected("literal").into_musi_error(self.curr_span())),
        }
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
                    | TokenKind::LBrackLt
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

    /// # Errors
    /// Returns `ParseErrorKind::Expected` if current token is not an identifier.
    pub fn expect_ident(&mut self) -> MusiResult<u32> {
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                Ok(id)
            }
            _ => Err(ParseErrorKind::Expected("identifier").into_musi_error(self.curr_span())),
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
}
