use music_ast::{SyntaxElement, SyntaxNode, SyntaxNodeKind};
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirChainKind, HirConstraint, HirConstraintKind,
    HirDeclMods, HirEffectItem, HirEffectSet, HirExprId, HirExprKind, HirFStringPart, HirFieldDef,
    HirHandleClause, HirLit, HirLitKind, HirMemberKey, HirOrigin, HirPatKind, HirPrefixOp,
    HirRecordItem, HirRecordItems, HirSplice, HirSpliceId, HirSpliceKind, HirStringLit, HirTy,
    HirTyKind, HirTyParam, HirTypeParams, HirVariantDef,
};
use music_lex::{FStringPartKind, TokenKind};
use music_names::{Ident, NameBindingKind, Symbol};

use super::{Resolver, cursor::AstCursor};

impl<'tree> Resolver<'_, 'tree, '_> {
    pub(super) fn lower_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        match node.kind() {
            SyntaxNodeKind::SequenceExpr => self.lower_sequence_expr(node),
            SyntaxNodeKind::LetExpr => self.lower_let_expr(node),
            SyntaxNodeKind::ImportExpr => self.lower_import_expr(node),
            SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node),
            SyntaxNodeKind::DataExpr => self.lower_data_expr(node),
            SyntaxNodeKind::EffectExpr => self.lower_effect_expr(node),
            SyntaxNodeKind::ClassExpr => self.lower_class_expr(node),
            SyntaxNodeKind::InstanceExpr => self.lower_instance_expr(node),

            SyntaxNodeKind::NameExpr => {
                let Some(tok) = node
                    .child_tokens()
                    .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
                else {
                    self.error(node.span(), "expected identifier");
                    return self.alloc_expr(origin, HirExprKind::Error);
                };
                let ident = self.intern_ident_token(tok);
                self.check_use(ident);
                self.alloc_expr(origin, HirExprKind::Named { ident })
            }
            SyntaxNodeKind::LiteralExpr => {
                let Some(tok) = node.child_tokens().next() else {
                    self.error(node.span(), "expected literal");
                    return self.alloc_expr(origin, HirExprKind::Error);
                };
                let lit = self.lower_lit_token(tok);
                self.alloc_expr(origin, HirExprKind::Lit { lit })
            }
            SyntaxNodeKind::FStringExpr => {
                let lit = self.lower_fstring_expr(node);
                self.alloc_expr(origin, HirExprKind::Lit { lit })
            }

            SyntaxNodeKind::TupleExpr => {
                let items: Vec<_> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_expr())
                    .map(|expr| self.lower_expr(expr))
                    .collect();
                self.alloc_expr(
                    origin,
                    HirExprKind::Tuple {
                        items: items.into_boxed_slice(),
                    },
                )
            }
            SyntaxNodeKind::ArrayExpr => self.lower_array_expr(node),
            SyntaxNodeKind::RecordExpr => self.lower_record_expr(node),
            SyntaxNodeKind::VariantExpr => self.lower_variant_expr(node),
            SyntaxNodeKind::LambdaExpr => self.lower_lambda_expr(node),
            SyntaxNodeKind::CallExpr => self.lower_call_expr(node),
            SyntaxNodeKind::FieldExpr => self.lower_field_expr(node),
            SyntaxNodeKind::IndexExpr => self.lower_index_expr(node),
            SyntaxNodeKind::RecordUpdateExpr => self.lower_record_update_expr(node),
            SyntaxNodeKind::TypeTestExpr => self.lower_type_test_expr(node),
            SyntaxNodeKind::TypeCastExpr => self.lower_type_cast_expr(node),
            SyntaxNodeKind::PrefixExpr => self.lower_prefix_expr(node),
            SyntaxNodeKind::BinaryExpr => self.lower_binary_expr(node),
            SyntaxNodeKind::CaseExpr => self.lower_case_expr(node),
            SyntaxNodeKind::PerformExpr => self.lower_perform_expr(node),
            SyntaxNodeKind::HandleExpr => self.lower_handle_expr(node),
            SyntaxNodeKind::ResumeExpr => self.lower_resume_expr(node),
            SyntaxNodeKind::QuoteExpr => self.lower_quote_expr(node),
            SyntaxNodeKind::SpliceExpr => self.lower_splice_expr(node),

            SyntaxNodeKind::Error => self.alloc_expr(origin, HirExprKind::Error),
            _ => {
                self.error(node.span(), "expected expression");
                self.alloc_expr(origin, HirExprKind::Error)
            }
        }
    }

    fn lower_fstring_expr(&mut self, node: SyntaxNode<'tree>) -> HirLit {
        let Some(tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::FStringLit(_)))
        else {
            self.error(node.span(), "expected f-string literal");
            return HirLit {
                kind: HirLitKind::String(HirStringLit::new(node.span(), None)),
            };
        };

        let TokenKind::FStringLit(parts) = tok.kind().clone() else {
            self.error(node.span(), "expected f-string literal");
            return HirLit {
                kind: HirLitKind::String(HirStringLit::new(node.span(), None)),
            };
        };

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());

        let mut hir_parts = vec![];
        for part in parts {
            match part.kind {
                FStringPartKind::Literal => {
                    hir_parts.push(HirFStringPart::Literal { span: part.span });
                }
                FStringPartKind::Interpolation => {
                    let (origin, expr) = if let Some(expr_node) = exprs.next() {
                        let origin = HirOrigin::new(part.span, Some(expr_node.id()));
                        let expr = self.lower_expr(expr_node);
                        (origin, expr)
                    } else {
                        let origin = HirOrigin::new(part.span, None);
                        let expr = self.alloc_expr(origin, HirExprKind::Error);
                        (origin, expr)
                    };
                    hir_parts.push(HirFStringPart::Expr { origin, expr });
                }
            }
        }

        HirLit {
            kind: HirLitKind::FString {
                span: tok.span(),
                syntax: Some(tok.id()),
                parts: hir_parts.into_boxed_slice(),
            },
        }
    }

    fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let has_lparen = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::LParen));

        if has_lparen {
            self.push_scope();
        }

        let yields_unit = sequence_yields_unit(node);
        let mut exprs = vec![];
        let mut last_stmt_expr: Option<SyntaxNode<'tree>> = None;

        for el in node.children() {
            match el {
                SyntaxElement::Token(tok) => {
                    if has_lparen && matches!(tok.kind(), TokenKind::Semi) {
                        if let Some(stmt_expr) = last_stmt_expr.take() {
                            if stmt_expr.kind() == SyntaxNodeKind::ImportExpr {
                                self.open_import_expr(stmt_expr);
                            }
                        }
                    }
                }
                SyntaxElement::Node(child) => {
                    if !child.kind().is_expr() {
                        continue;
                    }
                    exprs.push(self.lower_expr(child));
                    if has_lparen {
                        last_stmt_expr = Some(child);
                    }
                }
            }
        }

        if has_lparen {
            self.pop_scope();
        }

        self.alloc_expr(
            origin,
            HirExprKind::Sequence {
                exprs: exprs.into_boxed_slice(),
                yields_unit,
            },
        )
    }

    fn lower_let_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

        let mut exported = false;
        let mut opaque = false;
        let mut is_foreign = false;
        let mut external_abi = None;

        if cursor.eat_token(&TokenKind::KwExport).is_some() {
            exported = true;
            if cursor.eat_token(&TokenKind::KwOpaque).is_some() {
                opaque = true;
            }
        }

        if cursor.eat_token(&TokenKind::KwForeign).is_some() {
            is_foreign = true;
            if let Some(tok) = cursor.eat_token(&TokenKind::StringLit) {
                external_abi = Some(Self::string_lit_token(tok));
            }
        }

        let _let_kw = cursor.eat_token(&TokenKind::KwLet);
        let mutable = cursor.eat_token(&TokenKind::KwMut).is_some();

        let pat_node = cursor
            .bump_node()
            .filter(|n| n.kind().is_pat())
            .unwrap_or_else(|| {
                self.error(node.span(), "expected binding pattern");
                node
            });
        let pat = self.lower_pat(pat_node, true, NameBindingKind::Let);

        let params_node = cursor.eat_node(SyntaxNodeKind::ParamList);
        let ty_params_node = cursor.eat_node(SyntaxNodeKind::TypeParamList);
        let has_params = params_node.is_some();

        let where_node = cursor
            .eat_token(&TokenKind::KwWhere)
            .and_then(|_| cursor.eat_node(SyntaxNodeKind::ConstraintList));

        let effects_node = cursor
            .eat_token(&TokenKind::KwWith)
            .and_then(|_| cursor.eat_node(SyntaxNodeKind::EffectSet));

        let annot_node = cursor
            .eat_token(&TokenKind::Colon)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_ty());

        let value_node = cursor
            .eat_token(&TokenKind::ColonEq)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_expr());

        // `import "..."` in statement position opens all exports into scope.
        // A wildcard-bound let binding is semantically equivalent.
        if let Some(import_node) = value_node.filter(|n| n.kind() == SyntaxNodeKind::ImportExpr) {
            if matches!(self.store.pats[pat].kind, HirPatKind::Wildcard) {
                self.open_import_expr(import_node);
            }
        }

        // Type params and value params are scoped to the bound value.
        self.push_scope();

        let ty_params = ty_params_node.map_or_else(
            || vec![].into_boxed_slice(),
            |list| self.lower_ty_params(list),
        );

        let params = params_node.map_or_else(
            || vec![].into_boxed_slice(),
            |list| self.lower_param_list(list),
        );

        let where_ = where_node.map_or_else(
            || vec![].into_boxed_slice(),
            |list| self.lower_constraint_list(list),
        );

        let effects = effects_node.map(|set| self.lower_effect_set(set));
        let annot = annot_node.map(|ty| self.lower_ty(ty));
        let value = value_node.map(|expr| self.lower_expr(expr));

        self.pop_scope();

        let mods = HirDeclMods {
            attrs,
            exported,
            opaque,
            is_foreign,
            external_abi,
        };

        if exported {
            self.record_exports_from_pat(pat);
        }

        self.alloc_expr(
            origin,
            HirExprKind::Let {
                mods,
                mutable,
                pat,
                has_params,
                params,
                ty_params,
                where_,
                effects,
                annot,
                value,
            },
        )
    }

    fn lower_import_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let Some(path_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::StringLit))
        else {
            self.error(node.span(), "expected string literal import path");
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let path = Self::string_lit_token(path_tok);

        let mut exports = Vec::<Symbol>::new();
        if let Some(env) = self.import_env {
            let raw_path = self.decode_string_lit_span(path_tok.span());
            let path_key = self.normalize_import_key(raw_path.as_str());
            env.for_each_export(self.source_id, path_key.as_str(), &mut |name| {
                exports.push(self.interner.intern(name));
            });
        }
        exports.sort();
        exports.dedup();

        self.alloc_expr(
            origin,
            HirExprKind::Import {
                path,
                exports: exports.into_boxed_slice(),
            },
        )
    }

    fn lower_foreign_block_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let block_attrs = self.lower_attr_prefix(&mut cursor);

        if cursor.at_token(&TokenKind::KwExport) {
            let _ = cursor.bump_token();
            let _ = cursor.eat_token(&TokenKind::KwOpaque);
        }

        let _foreign = cursor.eat_token(&TokenKind::KwForeign);
        let abi = cursor
            .eat_token(&TokenKind::StringLit)
            .map(Self::string_lit_token);

        let _ = cursor.eat_token(&TokenKind::LParen);

        let mut items = vec![];
        while !cursor.at_token(&TokenKind::RParen) {
            let Some(el) = cursor.bump() else {
                break;
            };
            if let Some(n) = el.into_node() {
                if n.kind().is_expr() {
                    let id = self.lower_expr(n);
                    // `foreign "abi" ( let ...; )` desugars to each inner `let` carrying the ABI and
                    // inheriting the outer attrs.
                    if let HirExprKind::Let { mods, .. } = &mut self.store.exprs.get_mut(id).kind {
                        mods.is_foreign = true;
                        if mods.external_abi.is_none() {
                            mods.external_abi = abi;
                        }
                        if !block_attrs.is_empty() {
                            let mut merged =
                                Vec::with_capacity(block_attrs.len() + mods.attrs.len());
                            merged.extend(block_attrs.iter().copied());
                            merged.extend(mods.attrs.iter().copied());
                            mods.attrs = merged.into_boxed_slice();
                        }
                    }
                    items.push(id);
                }
            }
        }

        self.alloc_expr(
            origin,
            HirExprKind::ForeignBlock {
                abi,
                items: items.into_boxed_slice(),
            },
        )
    }

    fn lower_data_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let has_pipe = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::Pipe));
        let has_semi = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::Semi));

        let variants: Vec<HirVariantDef> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Variant)
            .map(|v| self.lower_variant_def(v))
            .collect();

        let fields: Vec<HirFieldDef> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Field)
            .map(|f| self.lower_field_def(f))
            .collect();

        let (variants, fields) = if has_pipe && !has_semi {
            (Some(variants.into_boxed_slice()), None)
        } else if has_semi && !has_pipe {
            (None, Some(fields.into_boxed_slice()))
        } else if !variants.is_empty() {
            (Some(variants.into_boxed_slice()), None)
        } else if !fields.is_empty() {
            (None, Some(fields.into_boxed_slice()))
        } else {
            (None, None)
        };

        self.alloc_expr(origin, HirExprKind::Data { variants, fields })
    }

    fn lower_variant_def(&mut self, node: SyntaxNode<'tree>) -> HirVariantDef {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);
        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );

        let payload_ty = cursor
            .eat_token(&TokenKind::Colon)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        let value = cursor
            .eat_token(&TokenKind::ColonEq)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_expr())
            .map(|expr| self.lower_expr(expr));

        HirVariantDef {
            origin,
            attrs,
            name,
            payload_ty,
            value,
        }
    }

    fn lower_field_def(&mut self, node: SyntaxNode<'tree>) -> HirFieldDef {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );

        let _ = cursor.eat_token(&TokenKind::Colon);
        let error_ty = self.store.tys.alloc(HirTy {
            origin,
            kind: HirTyKind::Error,
        });
        let ty = cursor
            .bump_node()
            .filter(|n| n.kind().is_ty())
            .map_or(error_ty, |ty| self.lower_ty(ty));

        let value = cursor
            .eat_token(&TokenKind::ColonEq)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_expr())
            .map(|expr| self.lower_expr(expr));

        HirFieldDef {
            origin,
            name,
            ty,
            value,
        }
    }

    fn lower_effect_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let members = self.lower_member_defs(node);
        self.alloc_expr(origin, HirExprKind::Effect { members })
    }

    fn lower_class_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let where_ = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map_or_else(
                || vec![].into_boxed_slice(),
                |list| self.lower_constraint_list(list),
            );

        let members = self.lower_member_defs(node);
        self.alloc_expr(origin, HirExprKind::Class { where_, members })
    }

    fn lower_instance_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();
        let exported = cursor.eat_token(&TokenKind::KwExport).is_some();

        let mods = HirDeclMods {
            attrs,
            exported,
            opaque: false,
            is_foreign: false,
            external_abi: None,
        };

        let _ = cursor.eat_token(&TokenKind::KwInstance);

        self.push_scope();

        let ty_params = cursor.eat_node(SyntaxNodeKind::TypeParamList).map_or_else(
            || vec![].into_boxed_slice(),
            |list| self.lower_ty_params(list),
        );

        let where_ = cursor
            .eat_token(&TokenKind::KwWhere)
            .and_then(|_| cursor.eat_node(SyntaxNodeKind::ConstraintList))
            .map_or_else(
                || vec![].into_boxed_slice(),
                |list| self.lower_constraint_list(list),
            );

        let target = match cursor.bump_node().filter(|n| n.kind().is_ty()) {
            Some(ty) => self.lower_ty(ty),
            None => self.store.tys.alloc(HirTy {
                origin,
                kind: HirTyKind::Error,
            }),
        };

        let members = self.lower_member_defs(node);

        self.pop_scope();

        self.alloc_expr(
            origin,
            HirExprKind::Instance {
                mods,
                ty_params,
                where_,
                target,
                members,
            },
        )
    }

    fn lower_ty_params(&mut self, node: SyntaxNode<'tree>) -> HirTypeParams {
        let mut params = vec![];
        for tp in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::TypeParam)
        {
            let origin = Self::origin_node(tp);
            let Some(name_tok) = tp
                .child_tokens()
                .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
            else {
                continue;
            };
            let name = self.intern_ident_token(name_tok);
            self.define(NameBindingKind::TypeParam, name);
            params.push(HirTyParam { origin, name });
        }
        params.into_boxed_slice()
    }

    fn lower_constraint_list(&mut self, node: SyntaxNode<'tree>) -> Box<[HirConstraint]> {
        let mut items = vec![];
        for c in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Constraint)
        {
            items.push(self.lower_constraint(c));
        }
        items.into_boxed_slice()
    }

    fn lower_constraint(&mut self, node: SyntaxNode<'tree>) -> HirConstraint {
        let origin = Self::origin_node(node);
        let mut toks = node.child_tokens();

        let name_tok = toks
            .next()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );
        self.check_use(name);

        let is_subtype = toks
            .find_map(|t| match t.kind() {
                TokenKind::LtColon => Some(true),
                TokenKind::Colon => Some(false),
                _ => None,
            })
            .unwrap_or(false);

        let error_ty = self.store.tys.alloc(HirTy {
            origin,
            kind: HirTyKind::Error,
        });
        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map_or(error_ty, |t| self.lower_ty(t));

        let kind = if is_subtype {
            HirConstraintKind::Subtype { name, bound: ty }
        } else {
            HirConstraintKind::Implements { name, class: ty }
        };

        HirConstraint { origin, kind }
    }

    fn lower_effect_set(&mut self, node: SyntaxNode<'tree>) -> HirEffectSet {
        let origin = Self::origin_node(node);
        let items: Vec<HirEffectItem> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::EffectItem)
            .map(|n| self.lower_effect_item(n))
            .collect();

        let mut rest = None;
        let mut saw_dots = false;
        for tok in node.child_tokens() {
            if saw_dots && matches!(tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                let ident = self.intern_ident_token(tok);
                rest = Some(ident);
                break;
            }
            if matches!(tok.kind(), TokenKind::DotDotDot) {
                saw_dots = true;
            }
        }

        HirEffectSet {
            origin,
            items: items.into_boxed_slice(),
            rest,
        }
    }

    fn lower_effect_item(&mut self, node: SyntaxNode<'tree>) -> HirEffectItem {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );
        self.check_use(name);

        let arg = cursor
            .eat_token(&TokenKind::LBracket)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        HirEffectItem { origin, name, arg }
    }

    fn lower_array_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let items: Vec<HirArrayItem> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::ArrayItem)
            .map(|n| self.lower_array_item(n))
            .collect();

        self.alloc_expr(
            origin,
            HirExprKind::Array {
                items: items.into_boxed_slice(),
            },
        )
    }

    fn lower_array_item(&mut self, node: SyntaxNode<'tree>) -> HirArrayItem {
        let item_origin = Self::origin_node(node);
        let spread = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::DotDotDot));
        let error_expr = self.alloc_expr(item_origin, HirExprKind::Error);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map_or(error_expr, |e| self.lower_expr(e));

        if spread {
            HirArrayItem::Spread {
                origin: item_origin,
                expr,
            }
        } else {
            HirArrayItem::Expr(expr)
        }
    }

    fn lower_record_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let items = self.lower_record_items(node);
        self.alloc_expr(origin, HirExprKind::Record { items })
    }

    fn lower_record_items(&mut self, node: SyntaxNode<'tree>) -> HirRecordItems {
        let mut out = vec![];
        for item_node in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::RecordItem)
        {
            out.push(self.lower_record_item(item_node));
        }
        out.into_boxed_slice()
    }

    fn lower_record_item(&mut self, node: SyntaxNode<'tree>) -> HirRecordItem {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);
        if cursor.eat_token(&TokenKind::DotDotDot).is_some() {
            let error_expr = self.alloc_expr(origin, HirExprKind::Error);
            let expr = cursor
                .bump_node()
                .filter(|n| n.kind().is_expr())
                .map_or(error_expr, |e| self.lower_expr(e));
            return HirRecordItem::Spread { origin, expr };
        }

        let Some(name_tok) = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
        else {
            self.error(node.span(), "expected record field name");
            return HirRecordItem::Field {
                origin,
                name: Ident::dummy(Symbol::synthetic(u32::MAX)),
                value: None,
            };
        };

        let name = self.intern_ident_token(name_tok);
        let value = cursor
            .eat_token(&TokenKind::ColonEq)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_expr())
            .map(|e| self.lower_expr(e));

        if value.is_none() {
            // Field shorthand `{ x }` desugars to `{ x := x }`.
            self.check_use(name);
        }

        HirRecordItem::Field {
            origin,
            name,
            value,
        }
    }

    fn lower_variant_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut tokens = node.child_tokens();
        let _dot = tokens.next();
        let Some(name_tok) = tokens.next() else {
            self.error(node.span(), "expected variant name");
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let name = self.intern_ident_token(name_tok);

        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_expr())
            .map(|expr| self.lower_expr(expr))
            .collect();

        let payload = match args.len() {
            0 => None,
            1 => Some(args[0]),
            _ => {
                let tuple = self.alloc_expr(
                    origin,
                    HirExprKind::Tuple {
                        items: args.into_boxed_slice(),
                    },
                );
                Some(tuple)
            }
        };

        self.alloc_expr(origin, HirExprKind::Variant { name, payload })
    }

    fn lower_lambda_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let params_node = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList);

        self.push_scope();
        let params =
            params_node.map_or_else(|| vec![].into_boxed_slice(), |n| self.lower_param_list(n));

        let ret = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let body = node
            .child_nodes()
            .filter(|n| n.kind().is_expr())
            .last()
            .map_or(error_expr, |expr| self.lower_expr(expr));

        self.pop_scope();

        self.alloc_expr(origin, HirExprKind::Lambda { params, ret, body })
    }

    fn lower_call_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut expr_nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let callee = expr_nodes.next().map_or(error_expr, |c| self.lower_expr(c));

        let args: Vec<HirArg> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Arg)
            .map(|arg| self.lower_arg(arg))
            .collect();

        self.alloc_expr(
            origin,
            HirExprKind::Call {
                callee,
                args: args.into_boxed_slice(),
            },
        )
    }

    fn lower_arg(&mut self, node: SyntaxNode<'tree>) -> HirArg {
        let origin = Self::origin_node(node);
        let spread = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::DotDotDot));
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map_or(error_expr, |e| self.lower_expr(e));

        if spread {
            HirArg::Spread { origin, expr }
        } else {
            HirArg::Expr(expr)
        }
    }

    fn lower_field_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let base = exprs.next().map_or(error_expr, |n| self.lower_expr(n));

        let chain = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::Dot => Some(HirChainKind::Normal),
                TokenKind::QuestionDot => Some(HirChainKind::Optional),
                TokenKind::BangDot => Some(HirChainKind::Forced),
                _ => None,
            })
            .unwrap_or(HirChainKind::Normal);

        let Some(target_tok) = node
            .child_tokens()
            .filter(|t| {
                matches!(
                    t.kind(),
                    TokenKind::Ident | TokenKind::EscapedIdent | TokenKind::IntLit
                )
            })
            .last()
        else {
            self.error(node.span(), "expected field target");
            return self.alloc_expr(origin, HirExprKind::Error);
        };

        let key = match target_tok.kind() {
            TokenKind::IntLit => HirMemberKey::IntLit {
                span: target_tok.span(),
                syntax: Some(target_tok.id()),
            },
            _ => HirMemberKey::Name(self.intern_ident_token(target_tok)),
        };

        self.alloc_expr(origin, HirExprKind::Member { base, chain, key })
    }

    fn lower_index_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let base = exprs.next().map_or(error_expr, |n| self.lower_expr(n));

        let indices: Vec<_> = exprs.map(|n| self.lower_expr(n)).collect();
        self.alloc_expr(
            origin,
            HirExprKind::Index {
                base,
                indices: indices.into_boxed_slice(),
            },
        )
    }

    fn lower_record_update_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let base = exprs.next().map_or(error_expr, |n| self.lower_expr(n));

        let items = self.lower_record_items(node);
        self.alloc_expr(origin, HirExprKind::RecordUpdate { base, items })
    }

    fn lower_type_test_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = exprs.next().map_or(error_expr, |n| self.lower_expr(n));

        let error_ty = self.store.tys.alloc(HirTy {
            origin,
            kind: HirTyKind::Error,
        });
        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map_or(error_ty, |n| self.lower_ty(n));

        let alias = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::KwAs))
            .and_then(|_| {
                node.child_tokens()
                    .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
                    .last()
            })
            .map(|t| self.intern_ident_token(t));

        self.alloc_expr(origin, HirExprKind::TypeTest { expr, ty, alias })
    }

    fn lower_type_cast_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = exprs.next().map_or(error_expr, |n| self.lower_expr(n));

        let error_ty = self.store.tys.alloc(HirTy {
            origin,
            kind: HirTyKind::Error,
        });
        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map_or(error_ty, |n| self.lower_ty(n));

        self.alloc_expr(origin, HirExprKind::TypeCast { expr, ty })
    }

    fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let op = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::Minus => Some(HirPrefixOp::Negate),
                TokenKind::KwNot => Some(HirPrefixOp::Not),
                TokenKind::KwMut => Some(HirPrefixOp::Mut),
                _ => None,
            })
            .unwrap_or(HirPrefixOp::Not);

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map_or(error_expr, |e| self.lower_expr(e));

        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    fn lower_binary_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let left = exprs.next().map_or(error_expr, |e| self.lower_expr(e));
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let right = exprs.next().map_or(error_expr, |e| self.lower_expr(e));

        let op_tok = node.child_tokens().find(|t| {
            matches!(
                t.kind(),
                TokenKind::LtMinus
                    | TokenKind::PipeGt
                    | TokenKind::KwOr
                    | TokenKind::KwXor
                    | TokenKind::KwAnd
                    | TokenKind::Eq
                    | TokenKind::SlashEq
                    | TokenKind::Lt
                    | TokenKind::Gt
                    | TokenKind::LtEq
                    | TokenKind::GtEq
                    | TokenKind::KwShl
                    | TokenKind::KwShr
                    | TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Star
                    | TokenKind::Slash
                    | TokenKind::Percent
                    | TokenKind::SymOp
                    | TokenKind::Amp
                    | TokenKind::Caret
                    | TokenKind::Tilde
            )
        });

        let op = op_tok.map_or(HirBinaryOp::Add, |tok| match tok.kind() {
            TokenKind::LtMinus => HirBinaryOp::Assign,
            TokenKind::PipeGt => HirBinaryOp::Pipe,
            TokenKind::KwOr => HirBinaryOp::Or,
            TokenKind::KwXor => HirBinaryOp::Xor,
            TokenKind::KwAnd => HirBinaryOp::And,
            TokenKind::Eq => HirBinaryOp::Eq,
            TokenKind::SlashEq => HirBinaryOp::NotEq,
            TokenKind::Lt => HirBinaryOp::Lt,
            TokenKind::Gt => HirBinaryOp::Gt,
            TokenKind::LtEq => HirBinaryOp::LtEq,
            TokenKind::GtEq => HirBinaryOp::GtEq,
            TokenKind::KwShl => HirBinaryOp::Shl,
            TokenKind::KwShr => HirBinaryOp::Shr,
            TokenKind::Plus => HirBinaryOp::Add,
            TokenKind::Minus => HirBinaryOp::Sub,
            TokenKind::Star => HirBinaryOp::Mul,
            TokenKind::Slash => HirBinaryOp::Div,
            TokenKind::Percent => HirBinaryOp::Mod,
            _ => HirBinaryOp::Symbolic(self.intern_op_token(tok)),
        });

        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }

    fn lower_case_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let _ = cursor.eat_token(&TokenKind::KwCase);
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let scrut = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map_or(error_expr, |n| self.lower_expr(n));

        let _ = cursor.eat_token(&TokenKind::KwOf);
        let _ = cursor.eat_token(&TokenKind::LParen);
        let _ = cursor.eat_token(&TokenKind::Pipe);

        let mut arms = vec![];

        while !cursor.at_token(&TokenKind::RParen) {
            let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

            self.push_scope();
            let error_pat = self.alloc_pat(origin, HirPatKind::Error);
            let pat = cursor
                .bump_node()
                .filter(|n| n.kind().is_pat())
                .map_or(error_pat, |p| {
                    self.lower_pat(p, true, NameBindingKind::PatternBind)
                });

            let guard = cursor
                .eat_token(&TokenKind::KwIf)
                .and_then(|_| cursor.bump_node())
                .filter(|n| n.kind().is_expr())
                .map(|g| {
                    self.define_type_test_aliases_in_guard(g);
                    self.lower_expr(g)
                });

            let _ = cursor.eat_token(&TokenKind::EqGt);
            let error_expr = self.alloc_expr(origin, HirExprKind::Error);
            let body = cursor
                .bump_node()
                .filter(|n| n.kind().is_expr())
                .map_or(error_expr, |b| self.lower_expr(b));

            self.pop_scope();

            arms.push(HirCaseArm {
                origin,
                attrs,
                pat,
                guard,
                body,
            });

            if cursor.eat_token(&TokenKind::Pipe).is_some() {
                if cursor.at_token(&TokenKind::RParen) {
                    break;
                }
                continue;
            }
            break;
        }

        self.alloc_expr(
            origin,
            HirExprKind::Case {
                scrut,
                arms: arms.into_boxed_slice(),
            },
        )
    }

    fn define_type_test_aliases_in_guard(&mut self, guard: SyntaxNode<'tree>) {
        let mut stack = vec![guard];
        while let Some(node) = stack.pop() {
            if node.kind() == SyntaxNodeKind::TypeTestExpr {
                let mut saw_as = false;
                for tok in node.child_tokens() {
                    if matches!(tok.kind(), TokenKind::KwAs) {
                        saw_as = true;
                        continue;
                    }
                    if saw_as && matches!(tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                        let ident = self.intern_ident_token(tok);
                        self.define(NameBindingKind::PatternBind, ident);
                        break;
                    }
                }
            }
            stack.extend(node.child_nodes());
        }
    }

    fn lower_perform_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map_or(error_expr, |n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Perform { expr })
    }

    fn lower_handle_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let _ = cursor.eat_token(&TokenKind::KwHandle);
        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let expr = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map_or(error_expr, |n| self.lower_expr(n));

        let _ = cursor.eat_token(&TokenKind::KwWith);
        let handler_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let handler = handler_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );
        self.check_use(handler);

        let _ = cursor.eat_token(&TokenKind::KwOf);
        let _ = cursor.eat_token(&TokenKind::LParen);
        let _ = cursor.eat_token(&TokenKind::Pipe);

        let mut clauses = vec![];
        while !cursor.at_token(&TokenKind::RParen) {
            let Some(clause_node) = cursor
                .bump_node()
                .filter(|n| n.kind() == SyntaxNodeKind::HandlerClause)
            else {
                break;
            };

            clauses.push(self.lower_handle_clause(clause_node, handler));

            if cursor.eat_token(&TokenKind::Pipe).is_some() {
                if cursor.at_token(&TokenKind::RParen) {
                    break;
                }
                continue;
            }
            break;
        }

        self.alloc_expr(
            origin,
            HirExprKind::Handle {
                expr,
                handler,
                clauses: clauses.into_boxed_slice(),
            },
        )
    }

    fn lower_handle_clause(&mut self, node: SyntaxNode<'tree>, _handler: Ident) -> HirHandleClause {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok.map_or_else(
            || Ident::dummy(Symbol::synthetic(u32::MAX)),
            |t| self.intern_ident_token(t),
        );

        let mut params = vec![];
        let is_value = cursor.eat_token(&TokenKind::LParen).is_none();
        if !is_value {
            while !cursor.at_token(&TokenKind::RParen) {
                let Some(tok) = cursor.bump_token() else {
                    break;
                };
                if matches!(tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                    params.push(self.intern_ident_token(tok));
                }
            }
            let _ = cursor.eat_token(&TokenKind::RParen);
        }

        let _ = cursor.eat_token(&TokenKind::EqGt);

        self.push_scope();
        if is_value {
            self.define(NameBindingKind::HandleClauseResult, name);
        } else {
            for p in &params {
                self.define(NameBindingKind::HandleClauseParam, *p);
            }
        }

        let error_expr = self.alloc_expr(origin, HirExprKind::Error);
        let body = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map_or(error_expr, |n| self.lower_expr(n));

        self.pop_scope();

        HirHandleClause {
            origin,
            is_value,
            name,
            params: params.into_boxed_slice(),
            body,
        }
    }

    fn lower_resume_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let value = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Resume { value })
    }

    fn lower_quote_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let mut splice_nodes = vec![];
        collect_quote_splices(node, &mut splice_nodes);
        let mut splices = Vec::with_capacity(splice_nodes.len());
        for splice_node in splice_nodes {
            splices.push(self.alloc_splice_from_node(splice_node));
        }
        self.alloc_expr(
            origin,
            HirExprKind::Quote {
                body_syntax: node.id(),
                splices: splices.into_boxed_slice(),
            },
        )
    }

    fn lower_splice_expr(&mut self, node: SyntaxNode<'tree>) -> HirExprId {
        let origin = Self::origin_node(node);
        let splice = self.alloc_splice_from_node(node);
        self.alloc_expr(origin, HirExprKind::Splice { splice })
    }

    fn alloc_splice_from_node(&mut self, node: SyntaxNode<'tree>) -> HirSpliceId {
        let origin = Self::origin_node(node);
        let mut cursor = AstCursor::new(node);

        let first = cursor.bump_token();
        let kind = match first.as_ref().map(|t| t.kind()) {
            Some(TokenKind::Hash) => {
                let target = cursor
                    .bump_token()
                    .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
                    .map_or_else(
                        || Ident::dummy(Symbol::synthetic(u32::MAX)),
                        |t| self.intern_ident_token(t),
                    );
                self.check_use(target);
                HirSpliceKind::Name(target)
            }
            Some(TokenKind::SpliceLParen) => {
                let error_expr = self.alloc_expr(origin, HirExprKind::Error);
                let expr = cursor
                    .bump_node()
                    .filter(|n| n.kind().is_expr())
                    .map_or(error_expr, |n| self.lower_expr(n));
                HirSpliceKind::Expr(expr)
            }
            Some(TokenKind::SpliceLBracket) => {
                let exprs: Vec<_> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_expr())
                    .map(|n| self.lower_expr(n))
                    .collect();
                HirSpliceKind::ExprArray(exprs.into_boxed_slice())
            }
            _ => HirSpliceKind::Expr(self.alloc_expr(origin, HirExprKind::Error)),
        };

        self.store.splices.alloc(HirSplice { origin, kind })
    }
}

fn collect_quote_splices<'tree>(node: SyntaxNode<'tree>, out: &mut Vec<SyntaxNode<'tree>>) {
    for child in node.child_nodes() {
        match child.kind() {
            SyntaxNodeKind::QuoteExpr => {}
            SyntaxNodeKind::SpliceExpr => out.push(child),
            _ => collect_quote_splices(child, out),
        }
    }
}

fn sequence_yields_unit(node: SyntaxNode<'_>) -> bool {
    let mut last_semi = false;
    for el in node.children() {
        match el {
            SyntaxElement::Token(tok) => match tok.kind() {
                TokenKind::Semi => last_semi = true,
                TokenKind::RParen => return last_semi,
                _ => {}
            },
            SyntaxElement::Node(n) => {
                if n.kind().is_expr() {
                    // An expr after the last ';' means the sequence yields that expr's value.
                    last_semi = false;
                }
            }
        }
    }
    last_semi
}
