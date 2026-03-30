use music_ast::{SyntaxElement, SyntaxNode, SyntaxNodeKind};
use music_hir::{
    HirArg, HirArrayItem, HirBinaryOp, HirCaseArm, HirConstraint, HirConstraintKind, HirDeclMods,
    HirEffectItem, HirEffectSet, HirExprKind, HirFieldDef, HirHandleClause, HirMemberKey,
    HirRecordItem, HirRecordItems, HirTypeParam, HirTypeParams, HirVariantDef,
};
use music_lex::TokenKind;
use music_names::{Ident, NameBindingKind, Symbol};

use super::{Resolver, cursor::AstCursor};

impl<'a, 'tree, 'env> Resolver<'a, 'tree, 'env> {
    pub(super) fn lower_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

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
                self.alloc_expr(origin, HirExprKind::Name { ident })
            }
            SyntaxNodeKind::LiteralExpr => {
                let Some(tok) = node.child_tokens().next() else {
                    self.error(node.span(), "expected literal");
                    return self.alloc_expr(origin, HirExprKind::Error);
                };
                let lit = self.lower_lit_token(tok);
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
            SyntaxNodeKind::QuoteExpr => self.alloc_expr(
                origin,
                HirExprKind::Quote {
                    body_syntax: node.id(),
                },
            ),
            SyntaxNodeKind::SpliceExpr => self.lower_splice_expr(node),

            SyntaxNodeKind::Error => self.alloc_expr(origin, HirExprKind::Error),
            _ => {
                self.error(node.span(), "expected expression");
                self.alloc_expr(origin, HirExprKind::Error)
            }
        }
    }

    fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let has_lparen = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::LParen));

        if has_lparen {
            self.push_scope();
        }

        let yields_unit = sequence_yields_unit(node);
        let mut exprs = Vec::new();
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

    fn lower_let_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

        let mut exported = false;
        let mut opaque = false;
        let mut external_abi = None;

        if cursor.eat_token(&TokenKind::KwExport).is_some() {
            exported = true;
            if cursor.eat_token(&TokenKind::KwOpaque).is_some() {
                opaque = true;
            }
        }

        if cursor.eat_token(&TokenKind::KwForeign).is_some() {
            if let Some(tok) = cursor.eat_token(&TokenKind::StringLit) {
                external_abi = Some(self.string_lit_token(tok));
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
        let type_params_node = cursor.eat_node(SyntaxNodeKind::TypeParamList);
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
            if matches!(self.store.pats[pat].kind, music_hir::HirPatKind::Wildcard) {
                self.open_import_expr(import_node);
            }
        }

        // Type params and value params are scoped to the bound value.
        self.push_scope();

        let type_params = type_params_node
            .map(|list| self.lower_type_params(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let params = params_node
            .map(|list| self.lower_param_list(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let where_ = where_node
            .map(|list| self.lower_constraint_list(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let effects = effects_node.map(|set| self.lower_effect_set(set));
        let annot = annot_node.map(|ty| self.lower_ty(ty));
        let value = value_node.map(|expr| self.lower_expr(expr));

        self.pop_scope();

        let mods = HirDeclMods {
            attrs,
            exported,
            opaque,
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
                type_params,
                where_,
                effects,
                annot,
                value,
            },
        )
    }

    fn lower_import_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let Some(path_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::StringLit))
        else {
            self.error(node.span(), "expected string literal import path");
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let path = self.string_lit_token(path_tok);
        self.alloc_expr(origin, HirExprKind::Import { path })
    }

    fn lower_foreign_block_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let _attrs = self.lower_attr_prefix(&mut cursor);

        if cursor.at_token(&TokenKind::KwExport) {
            let _ = cursor.bump_token();
            let _ = cursor.eat_token(&TokenKind::KwOpaque);
        }

        let _foreign = cursor.eat_token(&TokenKind::KwForeign);
        let abi = cursor
            .eat_token(&TokenKind::StringLit)
            .map(|t| self.string_lit_token(t));

        let _ = cursor.eat_token(&TokenKind::LParen);

        let mut items = Vec::new();
        while !cursor.at_token(&TokenKind::RParen) {
            let Some(el) = cursor.bump() else {
                break;
            };
            if let Some(n) = el.into_node() {
                if n.kind().is_expr() {
                    items.push(self.lower_expr(n));
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

    fn lower_data_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

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
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);
        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));

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
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));

        let _ = cursor.eat_token(&TokenKind::Colon);
        let ty = cursor
            .bump_node()
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty))
            .unwrap_or_else(|| {
                self.store.tys.alloc(music_hir::HirTy {
                    origin,
                    kind: music_hir::HirTyKind::Error,
                })
            });

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

    fn lower_effect_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let members = self.lower_member_defs(node);
        self.alloc_expr(origin, HirExprKind::Effect { members })
    }

    fn lower_class_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let where_ = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map(|list| self.lower_constraint_list(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let members = self.lower_member_defs(node);
        self.alloc_expr(origin, HirExprKind::Class { where_, members })
    }

    fn lower_instance_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();
        let exported = cursor.eat_token(&TokenKind::KwExport).is_some();

        let mods = HirDeclMods {
            attrs,
            exported,
            opaque: false,
            external_abi: None,
        };

        let _ = cursor.eat_token(&TokenKind::KwInstance);

        self.push_scope();

        let type_params = cursor
            .eat_node(SyntaxNodeKind::TypeParamList)
            .map(|list| self.lower_type_params(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let where_ = cursor
            .eat_token(&TokenKind::KwWhere)
            .and_then(|_| cursor.eat_node(SyntaxNodeKind::ConstraintList))
            .map(|list| self.lower_constraint_list(list))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let target = cursor
            .bump_node()
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty))
            .unwrap_or_else(|| {
                self.store.tys.alloc(music_hir::HirTy {
                    origin,
                    kind: music_hir::HirTyKind::Error,
                })
            });

        let members = self.lower_member_defs(node);

        self.pop_scope();

        self.alloc_expr(
            origin,
            HirExprKind::Instance {
                mods,
                type_params,
                where_,
                target,
                members,
            },
        )
    }

    fn lower_type_params(&mut self, node: SyntaxNode<'tree>) -> HirTypeParams {
        let mut params = Vec::new();
        for tp in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::TypeParam)
        {
            let origin = self.origin_node(tp);
            let Some(name_tok) = tp
                .child_tokens()
                .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
            else {
                continue;
            };
            let name = self.intern_ident_token(name_tok);
            self.define(NameBindingKind::TypeParam, name);
            params.push(HirTypeParam { origin, name });
        }
        params.into_boxed_slice()
    }

    fn lower_constraint_list(&mut self, node: SyntaxNode<'tree>) -> Box<[HirConstraint]> {
        let mut items = Vec::new();
        for c in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Constraint)
        {
            items.push(self.lower_constraint(c));
        }
        items.into_boxed_slice()
    }

    fn lower_constraint(&mut self, node: SyntaxNode<'tree>) -> HirConstraint {
        let origin = self.origin_node(node);
        let mut toks = node.child_tokens();

        let name_tok = toks
            .next()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));
        self.check_use(name);

        let is_subtype = toks
            .find_map(|t| match t.kind() {
                TokenKind::LtColon => Some(true),
                TokenKind::Colon => Some(false),
                _ => None,
            })
            .unwrap_or(false);

        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|t| self.lower_ty(t))
            .unwrap_or_else(|| {
                self.store.tys.alloc(music_hir::HirTy {
                    origin,
                    kind: music_hir::HirTyKind::Error,
                })
            });

        let kind = if is_subtype {
            HirConstraintKind::Subtype { name, bound: ty }
        } else {
            HirConstraintKind::Implements { name, class: ty }
        };

        HirConstraint { origin, kind }
    }

    fn lower_effect_set(&mut self, node: SyntaxNode<'tree>) -> HirEffectSet {
        let origin = self.origin_node(node);
        let items: Vec<HirEffectItem> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::EffectItem)
            .map(|n| self.lower_effect_item(n))
            .collect();

        let mut rest = None;
        let mut saw_dots = false;
        for tok in node.child_tokens() {
            if saw_dots {
                if matches!(tok.kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
                    let ident = self.intern_ident_token(tok);
                    rest = Some(ident);
                    break;
                }
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
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));
        self.check_use(name);

        let arg = cursor
            .eat_token(&TokenKind::LBracket)
            .and_then(|_| cursor.bump_node())
            .filter(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        HirEffectItem { origin, name, arg }
    }

    fn lower_array_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
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
        let item_origin = self.origin_node(node);
        let spread = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::DotDotDot));
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|e| self.lower_expr(e))
            .unwrap_or_else(|| self.alloc_expr(item_origin, HirExprKind::Error));

        if spread {
            HirArrayItem::Spread {
                origin: item_origin,
                expr,
            }
        } else {
            HirArrayItem::Expr(expr)
        }
    }

    fn lower_record_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let items = self.lower_record_items(node);
        self.alloc_expr(origin, HirExprKind::Record { items })
    }

    fn lower_record_items(&mut self, node: SyntaxNode<'tree>) -> HirRecordItems {
        let mut out = Vec::new();
        for item_node in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::RecordItem)
        {
            out.push(self.lower_record_item(item_node));
        }
        out.into_boxed_slice()
    }

    fn lower_record_item(&mut self, node: SyntaxNode<'tree>) -> HirRecordItem {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);
        if cursor.eat_token(&TokenKind::DotDotDot).is_some() {
            let expr = cursor
                .bump_node()
                .filter(|n| n.kind().is_expr())
                .map(|e| self.lower_expr(e))
                .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));
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

    fn lower_variant_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

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

    fn lower_lambda_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let params_node = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList);

        self.push_scope();
        let params = params_node
            .map(|n| self.lower_param_list(n))
            .unwrap_or_else(|| Vec::new().into_boxed_slice());

        let ret = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|ty| self.lower_ty(ty));

        let body = node
            .child_nodes()
            .filter(|n| n.kind().is_expr())
            .last()
            .map(|expr| self.lower_expr(expr))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        self.pop_scope();

        self.alloc_expr(origin, HirExprKind::Lambda { params, ret, body })
    }

    fn lower_call_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut expr_nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let callee = expr_nodes
            .next()
            .map(|c| self.lower_expr(c))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

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
        let origin = self.origin_node(node);
        let spread = node
            .child_tokens()
            .any(|t| matches!(t.kind(), TokenKind::DotDotDot));
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|e| self.lower_expr(e))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        if spread {
            HirArg::Spread { origin, expr }
        } else {
            HirArg::Expr(expr)
        }
    }

    fn lower_field_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let base = exprs
            .next()
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let chain = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::Dot => Some(music_hir::HirChainKind::Normal),
                TokenKind::QuestionDot => Some(music_hir::HirChainKind::Optional),
                TokenKind::BangDot => Some(music_hir::HirChainKind::Forced),
                _ => None,
            })
            .unwrap_or(music_hir::HirChainKind::Normal);

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

    fn lower_index_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());

        let base = exprs
            .next()
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let indices: Vec<_> = exprs.map(|n| self.lower_expr(n)).collect();
        self.alloc_expr(
            origin,
            HirExprKind::Index {
                base,
                indices: indices.into_boxed_slice(),
            },
        )
    }

    fn lower_record_update_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());

        let base = exprs
            .next()
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let items = self.lower_record_items(node);
        self.alloc_expr(origin, HirExprKind::RecordUpdate { base, items })
    }

    fn lower_type_test_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let expr = exprs
            .next()
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| {
                self.store.tys.alloc(music_hir::HirTy {
                    origin,
                    kind: music_hir::HirTyKind::Error,
                })
            });

        let alias = node
            .child_tokens()
            .filter(|t| matches!(t.kind(), TokenKind::KwAs))
            .next()
            .and_then(|_| {
                node.child_tokens()
                    .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
                    .last()
            })
            .map(|t| self.intern_ident_token(t));

        self.alloc_expr(origin, HirExprKind::TypeTest { expr, ty, alias })
    }

    fn lower_type_cast_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let expr = exprs
            .next()
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let ty = node
            .child_nodes()
            .find(|n| n.kind().is_ty())
            .map(|n| self.lower_ty(n))
            .unwrap_or_else(|| {
                self.store.tys.alloc(music_hir::HirTy {
                    origin,
                    kind: music_hir::HirTyKind::Error,
                })
            });

        self.alloc_expr(origin, HirExprKind::TypeCast { expr, ty })
    }

    fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let op = node
            .child_tokens()
            .find_map(|t| match t.kind() {
                TokenKind::Minus => Some(music_hir::HirPrefixOp::Negate),
                TokenKind::KwNot => Some(music_hir::HirPrefixOp::Not),
                TokenKind::KwMut => Some(music_hir::HirPrefixOp::Mut),
                _ => None,
            })
            .unwrap_or(music_hir::HirPrefixOp::Not);

        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|e| self.lower_expr(e))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    fn lower_binary_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let left = exprs
            .next()
            .map(|e| self.lower_expr(e))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));
        let right = exprs
            .next()
            .map(|e| self.lower_expr(e))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

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

        let op = op_tok
            .map(|tok| match tok.kind() {
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
            })
            .unwrap_or(HirBinaryOp::Add);

        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }

    fn lower_case_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let _ = cursor.eat_token(&TokenKind::KwCase);
        let scrut = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let _ = cursor.eat_token(&TokenKind::KwOf);
        let _ = cursor.eat_token(&TokenKind::LParen);
        let _ = cursor.eat_token(&TokenKind::Pipe);

        let mut arms = Vec::new();

        while !cursor.at_token(&TokenKind::RParen) {
            let attrs = self.lower_attr_prefix(&mut cursor).into_boxed_slice();

            self.push_scope();
            let pat = cursor
                .bump_node()
                .filter(|n| n.kind().is_pat())
                .map(|p| self.lower_pat(p, true, NameBindingKind::PatternBind))
                .unwrap_or_else(|| self.alloc_pat(origin, music_hir::HirPatKind::Error));

            let guard = cursor
                .eat_token(&TokenKind::KwIf)
                .and_then(|_| cursor.bump_node())
                .filter(|n| n.kind().is_expr())
                .map(|g| self.lower_expr(g));

            let _ = cursor.eat_token(&TokenKind::EqGt);
            let body = cursor
                .bump_node()
                .filter(|n| n.kind().is_expr())
                .map(|b| self.lower_expr(b))
                .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

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

    fn lower_perform_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));
        self.alloc_expr(origin, HirExprKind::Perform { expr })
    }

    fn lower_handle_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let _ = cursor.eat_token(&TokenKind::KwHandle);
        let expr = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        let _ = cursor.eat_token(&TokenKind::KwWith);
        let handler_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let handler = handler_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));
        self.check_use(handler);

        let _ = cursor.eat_token(&TokenKind::KwOf);
        let _ = cursor.eat_token(&TokenKind::LParen);
        let _ = cursor.eat_token(&TokenKind::Pipe);

        let mut clauses = Vec::new();
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
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let name_tok = cursor
            .bump_token()
            .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent));
        let name = name_tok
            .map(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));

        let mut params = Vec::new();
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

        let body = cursor
            .bump_node()
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));

        self.pop_scope();

        HirHandleClause {
            origin,
            is_value,
            name,
            params: params.into_boxed_slice(),
            body,
        }
    }

    fn lower_resume_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let value = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Resume { value })
    }

    fn lower_splice_expr(&mut self, node: SyntaxNode<'tree>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut cursor = AstCursor::new(node);

        let first = cursor.bump_token();
        let kind = match first.as_ref().map(|t| t.kind()) {
            Some(TokenKind::Hash) => {
                let target = cursor
                    .bump_token()
                    .filter(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::EscapedIdent))
                    .map(|t| self.intern_ident_token(t))
                    .unwrap_or_else(|| Ident::dummy(Symbol::synthetic(u32::MAX)));
                self.check_use(target);
                music_hir::HirSpliceKind::Name(target)
            }
            Some(TokenKind::SpliceLParen) => {
                let expr = cursor
                    .bump_node()
                    .filter(|n| n.kind().is_expr())
                    .map(|n| self.lower_expr(n))
                    .unwrap_or_else(|| self.alloc_expr(origin, HirExprKind::Error));
                music_hir::HirSpliceKind::Expr(expr)
            }
            Some(TokenKind::SpliceLBracket) => {
                let exprs: Vec<_> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_expr())
                    .map(|n| self.lower_expr(n))
                    .collect();
                music_hir::HirSpliceKind::ExprArray(exprs.into_boxed_slice())
            }
            _ => music_hir::HirSpliceKind::Expr(self.alloc_expr(origin, HirExprKind::Error)),
        };

        let splice = self
            .store
            .splices
            .alloc(music_hir::HirSplice { origin, kind });
        self.alloc_expr(origin, HirExprKind::Splice { splice })
    }
}

fn sequence_yields_unit(node: SyntaxNode<'_>) -> bool {
    let mut last_semi = false;
    for el in node.children() {
        match el {
            music_ast::SyntaxElement::Token(tok) => match tok.kind() {
                TokenKind::Semi => last_semi = true,
                TokenKind::RParen => return last_semi,
                _ => {}
            },
            music_ast::SyntaxElement::Node(n) => {
                if n.kind().is_expr() {
                    // An expr after the last ';' means the sequence yields that expr's value.
                    last_semi = false;
                }
            }
        }
    }
    last_semi
}
