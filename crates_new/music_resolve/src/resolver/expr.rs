use super::*;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinaryOp, HirCaseArm,
    HirConstraint, HirConstraintKind, HirDim, HirEffectItem, HirEffectSet, HirExprId, HirFieldDef,
    HirForeignDecl, HirHandleClause, HirLetMods, HirMemberDef, HirMemberKind, HirParam, HirPat,
    HirPatKind, HirPrefixOp, HirQuoteKind, HirRecordItem, HirSpliceKind, HirTemplatePart,
    HirVariantDef,
};
use music_syntax::{SyntaxElement, SyntaxNodeKind};

use crate::string_lit::{
    decode_string_lit, decode_template_head, decode_template_middle, decode_template_no_subst,
    decode_template_tail,
};

use super::util::{parse_u32_lit, stmt_inner_expr};

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src> {
    pub(super) fn lower_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        match node.kind() {
            SyntaxNodeKind::SequenceExpr => {
                if let Some(inner) = stmt_inner_expr(node) {
                    self.lower_expr(inner)
                } else {
                    self.lower_sequence_expr(node)
                }
            }

            SyntaxNodeKind::LetExpr => self.lower_let_expr(node),
            SyntaxNodeKind::ImportExpr => self.lower_import_expr(node),
            SyntaxNodeKind::ExportExpr => self.lower_export_expr(node),
            SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node),
            SyntaxNodeKind::DataExpr => self.lower_data_expr(node),
            SyntaxNodeKind::EffectExpr => self.lower_effect_expr(node),
            SyntaxNodeKind::ClassExpr => self.lower_class_expr(node),
            SyntaxNodeKind::InstanceExpr => self.lower_instance_expr(node),

            SyntaxNodeKind::NameExpr => self.lower_name_expr(node),
            SyntaxNodeKind::LiteralExpr => self.lower_literal_expr(node),
            SyntaxNodeKind::TemplateExpr => self.lower_template_expr(node),

            SyntaxNodeKind::TupleExpr => self.lower_tuple_expr(node),
            SyntaxNodeKind::ArrayExpr => self.lower_array_expr_or_ty(node),
            SyntaxNodeKind::RecordExpr => self.lower_record_expr(node),
            SyntaxNodeKind::VariantExpr => self.lower_variant_expr(node),

            SyntaxNodeKind::PiExpr => self.lower_pi_expr(node),
            SyntaxNodeKind::LambdaExpr => self.lower_lambda_expr(node),

            SyntaxNodeKind::CallExpr => self.lower_call_expr(node),
            SyntaxNodeKind::ApplyExpr => self.lower_apply_expr(node),
            SyntaxNodeKind::IndexExpr => self.lower_index_expr(node),
            SyntaxNodeKind::RecordUpdateExpr => self.lower_record_update_expr(node),
            SyntaxNodeKind::FieldExpr => self.lower_field_expr(node),
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
            SyntaxNodeKind::AttributedExpr => self.lower_attributed_expr(node),

            _ => self.error_expr(origin),
        }
    }

    fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let exprs: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(origin, HirExprKind::Sequence { exprs })
    }

    fn lower_name_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent))
        else {
            self.diags.push(Diag::error("expected name").with_label(
                node.span(),
                self.source_id,
                "identifier missing",
            ));
            return self.error_expr(origin);
        };
        let Some(ident) = self.intern_ident_token(tok) else {
            return self.error_expr(origin);
        };
        self.record_use(ident);
        self.alloc_expr(origin, HirExprKind::Name { name: ident })
    }

    fn lower_literal_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node.child_tokens().next() else {
            return self.error_expr(origin);
        };
        let Some(lit) = self.alloc_lit_from_token(tok) else {
            return self.error_expr(origin);
        };
        self.alloc_expr(origin, HirExprKind::Lit { lit })
    }

    fn lower_template_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut parts = Vec::<HirTemplatePart>::new();
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => {
                    let Some(raw) = tok.text() else {
                        continue;
                    };
                    let decoded = match tok.kind() {
                        TokenKind::TemplateNoSubst => decode_template_no_subst(raw),
                        TokenKind::TemplateHead => decode_template_head(raw),
                        TokenKind::TemplateMiddle => decode_template_middle(raw),
                        TokenKind::TemplateTail => decode_template_tail(raw),
                        _ => continue,
                    };
                    if let Ok(text) = decoded {
                        parts.push(HirTemplatePart::Text { value: text.into() });
                    }
                }
                SyntaxElement::Node(expr) => {
                    let hir = self.lower_expr(expr);
                    parts.push(HirTemplatePart::Expr { expr: hir });
                }
            }
        }
        let parts = self.store.template_parts.alloc_from_iter(parts);
        self.alloc_expr(origin, HirExprKind::Template { parts })
    }

    fn lower_tuple_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let items: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let items = self.store.alloc_expr_list(items);
        self.alloc_expr(origin, HirExprKind::Tuple { items })
    }

    fn lower_array_expr_or_ty(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        if node
            .child_nodes()
            .any(|n| n.kind() == SyntaxNodeKind::ArrayItem)
        {
            return self.lower_array_expr(node);
        }
        self.lower_array_ty_expr(node)
    }

    fn lower_array_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut items = Vec::<HirArrayItem>::new();
        for item in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::ArrayItem)
        {
            let spread = item
                .child_tokens()
                .any(|t| t.kind() == TokenKind::DotDotDot);
            let expr = self.lower_opt_expr(origin, item.child_nodes().next());
            items.push(HirArrayItem { spread, expr });
        }
        let items = self.store.array_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::Array { items })
    }

    fn lower_array_ty_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut dims = Vec::<HirDim>::new();
        let mut item_expr = None::<HirExprId>;
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::Underscore => dims.push(HirDim::Unknown),
                    TokenKind::Int => {
                        if let Some(raw) = tok.text() {
                            if let Some(v) = parse_u32_lit(raw) {
                                dims.push(HirDim::Int(v));
                            } else {
                                dims.push(HirDim::Unknown);
                            }
                        }
                    }
                    TokenKind::Ident => {
                        if let Some(raw) = tok.text() {
                            let ident = self.intern_ident_text(tok.kind(), raw, tok.span());
                            self.record_use(ident);
                            dims.push(HirDim::Name(ident));
                        }
                    }
                    _ => {}
                },
                SyntaxElement::Node(expr) => {
                    item_expr = Some(self.lower_expr(expr));
                }
            }
        }
        let item = item_expr.unwrap_or_else(|| self.error_expr(origin));
        let dims = self.store.dims.alloc_from_iter(dims);
        self.alloc_expr(origin, HirExprKind::ArrayTy { dims, item })
    }

    fn lower_record_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut items = Vec::<HirRecordItem>::new();
        for item in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::RecordItem)
        {
            items.push(self.lower_record_item(item));
        }
        let items = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::Record { items })
    }

    fn lower_record_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirRecordItem {
        let origin = self.origin_node(node);
        if node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::DotDotDot)
        {
            let value = self.lower_opt_expr(origin, node.child_nodes().next());
            return HirRecordItem {
                spread: true,
                name: None,
                value,
            };
        }

        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let has_bind = node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq);
        let value = if has_bind {
            self.lower_opt_expr(origin, node.child_nodes().next())
        } else {
            self.record_use(name);
            let name_origin = name_tok.map_or(origin, |tok| self.origin_token(tok));
            self.alloc_expr(name_origin, HirExprKind::Name { name })
        };

        HirRecordItem {
            spread: false,
            name: Some(name),
            value,
        }
    }

    fn lower_variant_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let tag_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let tag = self.intern_ident_token_or_placeholder(tag_tok, node.span());
        self.record_use(tag);
        let args: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let args = self.store.alloc_expr_list(args);
        self.alloc_expr(origin, HirExprKind::Variant { tag, args })
    }

    fn lower_pi_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let binder_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let binder = self.intern_ident_token_or_placeholder(binder_tok, node.span());

        self.push_scope();
        let _ = self.insert_binding(binder, NameBindingKind::PiBinder);

        let is_effectful = node.child_tokens().any(|t| t.kind() == TokenKind::TildeGt);
        let mut exprs = node.child_nodes();
        let binder_ty = self.lower_opt_expr(origin, exprs.next());
        let ret = self.lower_opt_expr(origin, exprs.next());

        self.pop_scope();
        self.alloc_expr(
            origin,
            HirExprKind::Pi {
                binder,
                binder_ty,
                ret,
                is_effectful,
            },
        )
    }

    fn lower_lambda_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let param_list = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList);
        self.push_scope();
        let params = param_list.map_or(SliceRange::EMPTY, |list| self.lower_param_list(list));

        let mut exprs = node
            .child_nodes()
            .filter(|n| n.kind() != SyntaxNodeKind::ParamList);
        let ret_ty = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let body = match exprs.next() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(origin),
        };
        self.pop_scope();

        self.alloc_expr(
            origin,
            HirExprKind::Lambda {
                params,
                ret_ty,
                body,
            },
        )
    }

    fn lower_call_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let callee = self.lower_opt_expr(origin, nodes.next());

        let mut args = Vec::<HirArg>::new();
        for arg_node in nodes.filter(|n| n.kind() == SyntaxNodeKind::Arg) {
            let spread = arg_node
                .child_tokens()
                .any(|t| t.kind() == TokenKind::DotDotDot);
            let expr = self.lower_opt_expr(origin, arg_node.child_nodes().next());
            args.push(HirArg { spread, expr });
        }
        let args = self.store.args.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Call { callee, args })
    }

    fn lower_apply_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let callee = self.lower_opt_expr(origin, nodes.next());
        let args: Vec<_> = nodes.map(|n| self.lower_expr(n)).collect();
        let args = self.store.expr_ids.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Apply { callee, args })
    }

    fn lower_index_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let args: Vec<_> = nodes.map(|n| self.lower_expr(n)).collect();
        let args = self.store.expr_ids.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Index { base, args })
    }

    fn lower_record_update_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let mut items = Vec::<HirRecordItem>::new();
        for item in nodes.filter(|n| n.kind() == SyntaxNodeKind::RecordItem) {
            items.push(self.lower_record_item(item));
        }
        let items = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::RecordUpdate { base, items })
    }

    fn lower_field_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());

        let access = if node.child_tokens().any(|t| t.kind() == TokenKind::QDot) {
            HirAccessKind::Optional
        } else if node.child_tokens().any(|t| t.kind() == TokenKind::BangDot) {
            HirAccessKind::Unwrap
        } else {
            HirAccessKind::Direct
        };

        let name_tok = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::Int));
        let name = name_tok
            .and_then(|tok| {
                tok.text()
                    .map(|raw| self.intern_ident_text(tok.kind(), raw, tok.span()))
            })
            .unwrap_or_else(|| self.placeholder_ident(node.span()));

        self.alloc_expr(origin, HirExprKind::Field { base, access, name })
    }

    fn lower_type_test_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let ty = self.lower_opt_expr(origin, nodes.next());

        let as_name = node
            .child_tokens()
            .find(|t| t.kind() == TokenKind::Ident)
            .and_then(|t| self.intern_ident_token(t));
        self.alloc_expr(origin, HirExprKind::TypeTest { base, ty, as_name })
    }

    fn lower_type_cast_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base = self.lower_opt_expr(origin, nodes.next());
        let ty = self.lower_opt_expr(origin, nodes.next());
        self.alloc_expr(origin, HirExprKind::TypeCast { base, ty })
    }

    fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let op_tok = node.child_tokens().next();
        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::Minus) => HirPrefixOp::Neg,
            Some(TokenKind::KwMut) => HirPrefixOp::Mut,
            _ => HirPrefixOp::Not,
        };
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    fn lower_binary_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let left = self.lower_opt_expr(origin, nodes.next());
        let right = self.lower_opt_expr(origin, nodes.next());

        let op_tok = node.child_tokens().find(|t| t.kind() != TokenKind::Eof);
        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::LtMinus) => HirBinaryOp::Assign,
            Some(TokenKind::PipeGt) => HirBinaryOp::Pipe,
            Some(TokenKind::MinusGt) => HirBinaryOp::Arrow,
            Some(TokenKind::TildeGt) => HirBinaryOp::EffectArrow,
            Some(TokenKind::KwXor) => HirBinaryOp::Xor,
            Some(TokenKind::KwAnd) => HirBinaryOp::And,
            Some(TokenKind::Eq) => HirBinaryOp::Eq,
            Some(TokenKind::SlashEq) => HirBinaryOp::Ne,
            Some(TokenKind::Lt) => HirBinaryOp::Lt,
            Some(TokenKind::Gt) => HirBinaryOp::Gt,
            Some(TokenKind::LtEq) => HirBinaryOp::Le,
            Some(TokenKind::GtEq) => HirBinaryOp::Ge,
            Some(TokenKind::KwIn) => HirBinaryOp::In,
            Some(TokenKind::KwShl) => HirBinaryOp::Shl,
            Some(TokenKind::KwShr) => HirBinaryOp::Shr,
            Some(TokenKind::Plus) => HirBinaryOp::Add,
            Some(TokenKind::Minus) => HirBinaryOp::Sub,
            Some(TokenKind::Star) => HirBinaryOp::Mul,
            Some(TokenKind::Slash) => HirBinaryOp::Div,
            Some(TokenKind::Percent) => HirBinaryOp::Rem,
            Some(TokenKind::SymbolicOp) => {
                let tok = op_tok.expect("symbolic op token must exist");
                let raw = tok.text().unwrap_or("");
                let ident = self.intern_ident_text(tok.kind(), raw, tok.span());
                self.record_use(ident);
                HirBinaryOp::UserOp(ident)
            }
            _ => HirBinaryOp::Or,
        };
        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }

    fn lower_case_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let scrutinee = self.lower_opt_expr(origin, node.child_nodes().next());

        let mut arms = Vec::<HirCaseArm>::new();
        for arm in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::CaseArm)
        {
            arms.push(self.lower_case_arm(arm));
        }
        let arms = self.store.case_arms.alloc_from_iter(arms);
        self.alloc_expr(origin, HirExprKind::Case { scrutinee, arms })
    }

    fn lower_case_arm(&mut self, node: SyntaxNode<'tree, 'src>) -> HirCaseArm {
        self.push_scope();

        let attrs = self.lower_attrs(node);
        let pat_node = node.child_nodes().find(|n| n.kind().is_pat());
        let pat_node = pat_node.unwrap_or(node);
        let binders = if pat_node.kind().is_pat() {
            self.collect_pat_binders(pat_node)
        } else {
            Vec::new()
        };
        for b in binders {
            let _ = self.insert_binding(b, NameBindingKind::PatternBind);
        }
        let pat = if pat_node.kind().is_pat() {
            self.lower_pat(pat_node)
        } else {
            self.store.alloc_pat(HirPat {
                origin: self.origin_node(node),
                kind: HirPatKind::Error,
            })
        };

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let guard = if node.child_tokens().any(|t| t.kind() == TokenKind::KwIf) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let expr = match exprs.next() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };

        self.pop_scope();
        HirCaseArm {
            attrs,
            pat,
            guard,
            expr,
        }
    }

    fn lower_perform_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let expr = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Perform { expr })
    }

    fn lower_handle_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let expr = self.lower_opt_expr(origin, nodes.next());

        let handler_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let handler = self.intern_ident_token_or_placeholder(handler_tok, node.span());
        self.record_use(handler);

        let mut clauses = Vec::<HirHandleClause>::new();
        for clause in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::HandlerClause)
        {
            clauses.push(self.lower_handle_clause(clause));
        }
        let clauses = self.store.handle_clauses.alloc_from_iter(clauses);
        self.alloc_expr(
            origin,
            HirExprKind::Handle {
                expr,
                handler,
                clauses,
            },
        )
    }

    fn lower_handle_clause(&mut self, node: SyntaxNode<'tree, 'src>) -> HirHandleClause {
        let op_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let op = self.intern_ident_token_or_placeholder(op_tok, node.span());
        self.record_use(op);

        let mut idents = node
            .child_tokens()
            .filter(|t| t.kind() == TokenKind::Ident)
            .collect::<Vec<_>>();
        if !idents.is_empty() {
            let _ = idents.remove(0);
        }

        let (params, result) = if idents.is_empty() {
            (Vec::new(), None)
        } else {
            let mut ps = Vec::<Ident>::new();
            for tok in &idents[0..idents.len().saturating_sub(1)] {
                if let Some(id) = self.intern_ident_token(*tok) {
                    ps.push(id);
                }
            }
            let result = idents.last().and_then(|t| self.intern_ident_token(*t));
            (ps, result)
        };

        self.push_scope();
        for p in &params {
            let _ = self.insert_binding(*p, NameBindingKind::HandleClauseParam);
        }
        if let Some(r) = result {
            let _ = self.insert_binding(r, NameBindingKind::HandleClauseResult);
        }
        let body = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        self.pop_scope();

        let params = self.store.idents.alloc_from_iter(params);
        HirHandleClause {
            op,
            result,
            params,
            body,
        }
    }

    fn lower_resume_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let expr = node.child_nodes().next().map(|n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Resume { expr })
    }

    fn lower_quote_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        if node.child_tokens().any(|t| t.kind() == TokenKind::LParen) {
            let expr = self.lower_opt_expr(origin, node.child_nodes().find(|n| n.kind().is_expr()));
            return self.alloc_expr(
                origin,
                HirExprKind::Quote {
                    kind: HirQuoteKind::Expr { expr },
                },
            );
        }

        let mut exprs = Vec::<HirExprId>::new();
        for stmt in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::SequenceExpr)
        {
            if let Some(inner) = stmt_inner_expr(stmt) {
                exprs.push(self.lower_expr(inner));
            }
        }
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            origin,
            HirExprKind::Quote {
                kind: HirQuoteKind::Block { exprs },
            },
        )
    }

    fn lower_splice_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        if let Some(tok) = node.child_tokens().find(|t| t.kind() == TokenKind::Ident) {
            let name = self.intern_ident_token_or_placeholder(Some(tok), tok.span());
            self.record_use(name);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Name { name },
                },
            );
        }
        if let Some(expr) = node.child_nodes().find(|n| n.kind().is_expr()) {
            let expr = self.lower_expr(expr);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Expr { expr },
                },
            );
        }
        let exprs: Vec<_> = node.child_nodes().map(|n| self.lower_expr(n)).collect();
        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            origin,
            HirExprKind::Splice {
                kind: HirSpliceKind::Exprs { exprs },
            },
        )
    }

    fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let expr = self.lower_opt_expr(origin, node.child_nodes().find(|n| n.kind().is_expr()));
        self.alloc_expr(origin, HirExprKind::Attributed { attrs, expr })
    }

    fn lower_attrs(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirAttr> {
        let attrs: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Attr)
            .map(|n| self.lower_attr(n))
            .collect();
        self.store.attrs.alloc_from_iter(attrs)
    }

    fn lower_attr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttr {
        let origin = self.origin_node(node);
        let path: Vec<_> = node
            .child_tokens()
            .filter(|t| t.kind() == TokenKind::Ident)
            .filter_map(|t| self.intern_ident_token(t))
            .collect();
        let path = self.store.idents.alloc_from_iter(path);

        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::AttrArg)
            .map(|n| self.lower_attr_arg(n))
            .collect();
        let args = self.store.attr_args.alloc_from_iter(args);

        HirAttr { origin, path, args }
    }

    fn lower_attr_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttrArg {
        let name = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            node.child_tokens()
                .find(|t| t.kind() == TokenKind::Ident)
                .and_then(|t| self.intern_ident_token(t))
        } else {
            None
        };
        let value = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirAttrArg { name, value }
    }

    fn lower_import_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let arg = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Import { arg })
    }

    fn lower_export_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let opaque = node.child_tokens().any(|t| t.kind() == TokenKind::KwOpaque);
        let foreign_abi = if node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::KwForeign)
        {
            node.child_tokens()
                .find(|t| t.kind() == TokenKind::String)
                .and_then(SyntaxToken::text)
                .and_then(|raw| decode_string_lit(raw).ok())
                .map(String::into_boxed_str)
        } else {
            None
        };

        let expr_node = node
            .child_nodes()
            .find(|n| n.kind() != SyntaxNodeKind::Attr);
        let expr = match expr_node.map(SyntaxNode::kind) {
            Some(SyntaxNodeKind::MemberList) => {
                self.lower_foreign_group(expr_node.unwrap(), foreign_abi.clone())
            }
            Some(_) => self.lower_expr(expr_node.unwrap()),
            None => self.error_expr(origin),
        };

        self.alloc_expr(
            origin,
            HirExprKind::Export {
                opaque,
                foreign_abi,
                expr,
            },
        )
    }

    fn lower_foreign_block_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let abi = node
            .child_tokens()
            .find(|t| t.kind() == TokenKind::String)
            .and_then(SyntaxToken::text)
            .and_then(|raw| decode_string_lit(raw).ok())
            .map(String::into_boxed_str);

        let decls_node = node.child_nodes().find(|n| {
            matches!(
                n.kind(),
                SyntaxNodeKind::MemberList | SyntaxNodeKind::LetExpr
            )
        });
        let decls = if let Some(n) = decls_node {
            match n.kind() {
                SyntaxNodeKind::MemberList => self.lower_foreign_group_decls(n),
                SyntaxNodeKind::LetExpr => {
                    let member = n.child_nodes().find(|m| m.kind() == SyntaxNodeKind::Member);
                    if let Some(member) = member {
                        let decl = self.lower_foreign_decl(member);
                        self.store.foreign_decls.alloc_from_iter([decl])
                    } else {
                        SliceRange::EMPTY
                    }
                }
                _ => SliceRange::EMPTY,
            }
        } else {
            SliceRange::EMPTY
        };

        self.alloc_expr(origin, HirExprKind::Foreign { abi, decls })
    }

    fn lower_foreign_group(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        abi: Option<Box<str>>,
    ) -> HirExprId {
        let origin = self.origin_node(node);
        let decls = self.lower_foreign_group_decls(node);
        self.alloc_expr(origin, HirExprKind::Foreign { abi, decls })
    }

    fn lower_foreign_group_decls(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirForeignDecl> {
        let decls: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Member)
            .map(|n| self.lower_foreign_decl(n))
            .collect();
        self.store.foreign_decls.alloc_from_iter(decls)
    }

    fn lower_foreign_decl(&mut self, node: SyntaxNode<'tree, 'src>) -> HirForeignDecl {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);

        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let _ = self.insert_binding(name, NameBindingKind::Let);

        self.push_scope();
        let type_params = self.lower_type_params_clause(node);
        let params = self.lower_params_clause(node);
        let constraints = self.lower_constraints_clause(node);
        let sig = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            node.child_nodes()
                .find(|n| n.kind().is_expr() && n.kind() != SyntaxNodeKind::ParamList)
                .map(|n| self.lower_expr(n))
        } else {
            None
        };
        self.pop_scope();

        HirForeignDecl {
            origin,
            attrs,
            name,
            type_params,
            params,
            constraints,
            sig,
        }
    }

    fn lower_data_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let mut variants = Vec::<HirVariantDef>::new();
        let mut fields = Vec::<HirFieldDef>::new();
        for child in node.child_nodes() {
            match child.kind() {
                SyntaxNodeKind::Variant => variants.push(self.lower_variant_def(child)),
                SyntaxNodeKind::Field => fields.push(self.lower_field_def(child)),
                SyntaxNodeKind::VariantList => {
                    for v in child
                        .child_nodes()
                        .filter(|n| n.kind() == SyntaxNodeKind::Variant)
                    {
                        variants.push(self.lower_variant_def(v));
                    }
                }
                SyntaxNodeKind::FieldList => {
                    for f in child
                        .child_nodes()
                        .filter(|n| n.kind() == SyntaxNodeKind::Field)
                    {
                        fields.push(self.lower_field_def(f));
                    }
                }
                _ => {}
            }
        }
        let variants = self.store.variants.alloc_from_iter(variants);
        let fields = self.store.fields.alloc_from_iter(fields);
        self.alloc_expr(origin, HirExprKind::Data { variants, fields })
    }

    fn lower_variant_def(&mut self, node: SyntaxNode<'tree, 'src>) -> HirVariantDef {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let arg = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let value = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        HirVariantDef {
            origin,
            attrs,
            name,
            arg,
            value,
        }
    }

    fn lower_field_def(&mut self, node: SyntaxNode<'tree, 'src>) -> HirFieldDef {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let ty = self.lower_opt_expr(origin, exprs.next());
        let value = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        HirFieldDef {
            origin,
            attrs,
            name,
            ty,
            value,
        }
    }

    fn lower_effect_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();
        let members = self.lower_members(node);
        self.pop_scope();
        self.alloc_expr(origin, HirExprKind::Effect { members })
    }

    fn lower_class_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();
        let constraints = self.lower_constraints_clause(node);
        let members = self.lower_members(node);
        self.pop_scope();
        self.alloc_expr(
            origin,
            HirExprKind::Class {
                constraints,
                members,
            },
        )
    }

    fn lower_instance_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();

        let type_params = self.lower_type_params_clause(node);
        let constraints = self.lower_constraints_clause(node);
        let class = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(origin),
        };
        let members = self.lower_members(node);

        self.pop_scope();
        self.alloc_expr(
            origin,
            HirExprKind::Instance {
                type_params,
                constraints,
                class,
                members,
            },
        )
    }

    fn lower_members(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirMemberDef> {
        let members: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Member)
            .map(|n| self.lower_member_def(n))
            .collect();
        self.store.members.alloc_from_iter(members)
    }

    fn lower_member_def(&mut self, node: SyntaxNode<'tree, 'src>) -> HirMemberDef {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let kind = if node.child_tokens().any(|t| t.kind() == TokenKind::KwLaw) {
            HirMemberKind::Law
        } else {
            HirMemberKind::Let
        };

        let name_tok = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent));
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        let _ = self.insert_binding(name, NameBindingKind::Let);

        self.push_scope();
        let params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |list| self.lower_param_list(list));

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let sig = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let value = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        self.pop_scope();

        HirMemberDef {
            origin,
            attrs,
            kind,
            name,
            params,
            sig,
            value,
        }
    }

    fn lower_let_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);

        let is_mut = node.child_tokens().any(|t| t.kind() == TokenKind::KwMut);
        let is_rec = node.child_tokens().any(|t| t.kind() == TokenKind::KwRec);
        let mods = HirLetMods { is_mut, is_rec };

        let pat_node = node.child_nodes().find(|n| n.kind().is_pat());
        let pat_node = pat_node.unwrap_or(node);
        let binders = if pat_node.kind().is_pat() {
            self.collect_pat_binders(pat_node)
        } else {
            Vec::new()
        };

        let mut pending = Vec::<(Ident, NameBindingId)>::new();
        for b in binders {
            let id = self.names.alloc_binding(NameBinding {
                name: b.name,
                site: NameSite::new(self.source_id, b.span),
                kind: NameBindingKind::Let,
            });
            pending.push((b, id));
        }
        if is_rec {
            for (b, id) in &pending {
                if let Some(scope) = self.scopes.last_mut() {
                    let _prev = scope.names.insert(b.name, *id);
                }
            }
        }

        self.push_scope();
        let type_params = self.lower_type_params_clause(node);
        let has_param_clause =
            super::util::child_of_kind(node, SyntaxNodeKind::ParamList).is_some();
        let params = self.lower_params_clause(node);
        let constraints = self.lower_constraints_clause(node);
        let effects = super::util::child_of_kind(node, SyntaxNodeKind::EffectSet)
            .map(|effect_set| self.lower_effect_set(effect_set));

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let sig = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let value = match exprs.last() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(origin),
        };
        let pat = if pat_node.kind().is_pat() {
            self.lower_pat(pat_node)
        } else {
            self.store.alloc_pat(HirPat {
                origin,
                kind: HirPatKind::Error,
            })
        };
        self.pop_scope();

        if !is_rec {
            for (b, id) in pending {
                if let Some(scope) = self.scopes.last_mut() {
                    let _prev = scope.names.insert(b.name, id);
                }
            }
        }

        self.alloc_expr(
            origin,
            HirExprKind::Let {
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            },
        )
    }

    pub(super) fn lower_type_param_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<Ident> {
        let params: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::TypeParam)
            .filter_map(|n| n.child_tokens().find(|t| t.kind() == TokenKind::Ident))
            .filter_map(|t| self.intern_ident_token(t))
            .collect();
        for p in &params {
            let _ = self.insert_binding(*p, NameBindingKind::TypeParam);
        }
        self.store.idents.alloc_from_iter(params)
    }

    pub(super) fn lower_param_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirParam> {
        let mut params = Vec::<HirParam>::new();
        for p in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Param)
        {
            params.push(self.lower_param(p));
        }
        self.store.params.alloc_from_iter(params)
    }

    fn lower_param(&mut self, node: SyntaxNode<'tree, 'src>) -> HirParam {
        let is_mut = node.child_tokens().any(|t| t.kind() == TokenKind::KwMut);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        let _ = self.insert_binding(name, NameBindingKind::Param);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let ty = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let default = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };

        HirParam {
            is_mut,
            name,
            ty,
            default,
        }
    }

    pub(super) fn lower_constraint_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirConstraint> {
        let constraints: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Constraint)
            .map(|n| self.lower_constraint(n))
            .collect();
        self.store.constraints.alloc_from_iter(constraints)
    }

    fn lower_constraint(&mut self, node: SyntaxNode<'tree, 'src>) -> HirConstraint {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        self.record_use(name);

        let kind = if node.child_tokens().any(|t| t.kind() == TokenKind::LtColon) {
            HirConstraintKind::Subtype
        } else {
            HirConstraintKind::Implements
        };
        let value = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirConstraint { name, kind, value }
    }

    fn lower_effect_set(&mut self, node: SyntaxNode<'tree, 'src>) -> HirEffectSet {
        let mut items = Vec::<HirEffectItem>::new();
        let mut open = None::<Ident>;
        let mut saw_dots = false;
        for child in node.children() {
            match child {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::DotDotDot => saw_dots = true,
                    TokenKind::Ident if saw_dots => {
                        if let Some(id) = self.intern_ident_token(tok) {
                            self.record_use(id);
                            open = Some(id);
                        }
                        saw_dots = false;
                    }
                    _ => {}
                },
                SyntaxElement::Node(n) if n.kind() == SyntaxNodeKind::EffectItem => {
                    items.push(self.lower_effect_item(n));
                }
                SyntaxElement::Node(_) => {}
            }
        }
        let items = self.store.effect_items.alloc_from_iter(items);
        HirEffectSet { items, open }
    }

    fn lower_effect_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirEffectItem {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        self.record_use(name);

        let arg = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n));
        HirEffectItem { name, arg }
    }
}
