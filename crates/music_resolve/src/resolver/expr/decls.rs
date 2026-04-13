use super::*;
use crate::resolver::util::is_expr_or_ty;
use music_hir::{HirBinder, HirLetReceiver, HirPrefixOp};

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_import_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let arg = self.lower_opt_expr(origin, node.child_nodes().next());
        self.alloc_expr(origin, HirExprKind::Import { arg })
    }

    pub(super) fn lower_foreign_block_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        self.lower_foreign_block_expr_with_mods(node, HirMods::EMPTY)
    }

    pub(super) fn lower_foreign_block_expr_with_mods(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        outer_mods: HirMods,
    ) -> HirExprId {
        let origin = self.origin_node(node);
        let outer_attrs = self.lower_attrs(node);

        let abi = node
            .child_tokens()
            .find(|t| t.kind() == TokenKind::String)
            .and_then(SyntaxToken::text)
            .and_then(|raw| decode_string_lit(raw).ok())
            .map(|abi| self.interner.intern(abi.as_str()));
        let foreign_mod = HirForeignMod::new(abi);

        let inherited_attrs = outer_mods.attrs.clone();
        let merged_attrs = self.merge_attrs(inherited_attrs, outer_attrs);
        let base_mods = outer_mods
            .with_foreign(foreign_mod)
            .with_attrs(merged_attrs);

        let decls_node = node.child_nodes().find(|n| {
            matches!(
                n.kind(),
                SyntaxNodeKind::MemberList | SyntaxNodeKind::LetExpr
            )
        });

        let mut exprs = Vec::<HirExprId>::new();
        if let Some(n) = decls_node {
            match n.kind() {
                SyntaxNodeKind::MemberList => {
                    for member in n
                        .child_nodes()
                        .filter(|m| m.kind() == SyntaxNodeKind::Member)
                    {
                        exprs.push(self.lower_foreign_member_let(member, base_mods.clone()));
                    }
                }
                SyntaxNodeKind::LetExpr => {
                    let member = n.child_nodes().find(|m| m.kind() == SyntaxNodeKind::Member);
                    if let Some(member) = member {
                        exprs.push(self.lower_foreign_member_let(member, base_mods));
                    }
                }
                _ => {}
            }
        }

        let exprs = self.store.alloc_expr_list(exprs);
        self.alloc_expr(origin, HirExprKind::Sequence { exprs })
    }

    pub(super) fn lower_foreign_member_let(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        mods: HirMods,
    ) -> HirExprId {
        debug_assert_eq!(node.kind(), SyntaxNodeKind::Member);
        let origin = self.origin_node(node);

        let member_attrs = self.lower_attrs(node);
        let merged_attrs = self.merge_attrs(mods.attrs.clone(), member_attrs);
        let mods = mods.with_attrs(merged_attrs);

        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());
        let _ = self.insert_binding(name, NameBindingKind::Let);

        let pat = self
            .store
            .alloc_pat(HirPat::new(origin, HirPatKind::Bind { name }));

        self.push_scope();
        let type_params = self.lower_type_params_clause(node);
        let has_param_clause = child_of_kind(node, SyntaxNodeKind::ParamList).is_some();
        let params = self.lower_params_clause(node);
        let constraints = self.lower_constraints_clause(node);
        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let sig = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        self.pop_scope();

        let value = self.error_expr(origin);
        let expr_id = self.alloc_expr(
            origin,
            HirExprKind::Let {
                mods: HirLetMods::new(false),
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects: None,
                sig,
                value,
            },
        );
        self.apply_mods(expr_id, mods);
        expr_id
    }

    pub(super) fn lower_data_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
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

        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let arg = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        let value = self.lower_optional_expr_clause(node, TokenKind::ColonEq, &mut exprs);
        HirVariantDef::new(origin, attrs, name, arg, value)
    }

    fn lower_field_def(&mut self, node: SyntaxNode<'tree, 'src>) -> HirFieldDef {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = self.intern_ident_token_or_placeholder(name_tok, node.span());

        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let ty = self.lower_opt_expr(origin, exprs.next());
        let value = self.lower_optional_expr_clause(node, TokenKind::ColonEq, &mut exprs);
        HirFieldDef::new(origin, attrs, name, ty, value)
    }

    pub(super) fn lower_effect_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();
        let members = self.lower_members(node);
        self.pop_scope();
        self.alloc_expr(origin, HirExprKind::Effect { members })
    }

    pub(super) fn lower_class_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
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

    pub(super) fn lower_instance_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();

        let type_params = self.lower_type_params_clause(node);
        let constraints = self.lower_constraints_clause(node);
        let class = match node.child_nodes().find(|n| is_expr_or_ty(n.kind())) {
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
        let params = child_of_kind(node, SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |list| self.lower_param_list(list));

        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let sig = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        let value = self.lower_optional_expr_clause(node, TokenKind::ColonEq, &mut exprs);
        self.pop_scope();

        HirMemberDef::new(origin, attrs, kind, name, params, sig, value)
    }

    pub(super) fn lower_let_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);

        let is_rec = node.child_tokens().any(|t| t.kind() == TokenKind::KwRec);
        let pat_node = node.child_nodes().find(|n| n.kind().is_pat());
        let binders = match (pat_node, self.receiver_member_ident(node)) {
            (Some(pat), _) if pat.kind().is_pat() => self.collect_pat_binders(pat),
            (_, Some(member)) => vec![member],
            _ => Vec::new(),
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
        let type_params = self.lower_let_type_params(node);
        let receiver = self.lower_let_receiver(node);
        let mods = receiver.map_or_else(
            || HirLetMods::new(is_rec),
            |(receiver, _)| HirLetMods::new(is_rec).with_receiver(receiver),
        );
        if let Some(receiver) = mods.receiver {
            let _ = self.insert_binding(receiver.binder, NameBindingKind::Let);
        }
        let has_param_clause = child_of_kind(node, SyntaxNodeKind::ParamList).is_some();
        let params = self.lower_let_params_clause(node, mods.receiver);
        let constraints = self.lower_constraints_clause(node);
        let effects = child_of_kind(node, SyntaxNodeKind::EffectSet)
            .map(|effect_set| self.lower_effect_set(effect_set));

        let mut exprs = node
            .child_nodes()
            .filter(|child| is_expr_or_ty(child.kind()));
        let sig = self.lower_optional_expr_clause(node, TokenKind::Colon, &mut exprs);
        let value = match exprs.last() {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(origin),
        };
        let pat = if let Some(pat_node) = pat_node.filter(|n| n.kind().is_pat()) {
            self.lower_pat(pat_node)
        } else if let Some((_, member)) = receiver {
            self.store.alloc_pat(HirPat::new(
                HirOrigin::new(self.source_id, member.span),
                HirPatKind::Bind { name: member },
            ))
        } else {
            self.store.alloc_pat(HirPat::new(origin, HirPatKind::Error))
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

    fn lower_let_receiver(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> Option<(HirLetReceiver, Ident)> {
        let receiver_node = child_of_kind(node, SyntaxNodeKind::ReceiverSpec)?;
        let mut names = receiver_node
            .child_tokens()
            .filter(|token| token.kind() == TokenKind::Ident);
        let binder = self.intern_ident_token_or_placeholder(names.next(), receiver_node.span());
        let member = self.intern_ident_token_or_placeholder(names.next(), receiver_node.span());
        let ty = self.lower_opt_expr(
            self.origin_node(receiver_node),
            receiver_node
                .child_nodes()
                .find(|child| is_expr_or_ty(child.kind())),
        );
        let is_mut = receiver_node
            .child_tokens()
            .any(|token| token.kind() == TokenKind::KwMut);
        Some((HirLetReceiver::new(is_mut, binder, ty, member), member))
    }

    fn receiver_member_ident(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<Ident> {
        let receiver_node = child_of_kind(node, SyntaxNodeKind::ReceiverSpec)?;
        let mut names = receiver_node
            .child_tokens()
            .filter(|token| token.kind() == TokenKind::Ident);
        let _ = names.next()?;
        Some(self.intern_ident_token_or_placeholder(names.next(), receiver_node.span()))
    }

    fn lower_let_type_params(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirBinder> {
        let mut params = Vec::new();
        for child in node
            .child_nodes()
            .filter(|child| child.kind() == SyntaxNodeKind::TypeParamList)
        {
            let range = self.lower_type_param_list(child);
            params.extend(self.store.binders.get(range).to_vec());
        }
        self.store.binders.alloc_from_iter(params)
    }

    fn lower_let_params_clause(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        receiver: Option<HirLetReceiver>,
    ) -> SliceRange<HirParam> {
        let mut params = Vec::new();
        if let Some(receiver) = receiver {
            let receiver_ty = if receiver.is_mut {
                self.alloc_expr(
                    HirOrigin::new(self.source_id, receiver.binder.span),
                    HirExprKind::Prefix {
                        op: HirPrefixOp::Mut,
                        expr: receiver.ty,
                    },
                )
            } else {
                receiver.ty
            };
            params.push(HirParam::new(receiver.binder, Some(receiver_ty), None));
        }
        if let Some(list) = child_of_kind(node, SyntaxNodeKind::ParamList) {
            let lowered = self.lower_param_list(list);
            params.extend(self.store.params.get(lowered).to_vec());
        }
        self.store.params.alloc_from_iter(params)
    }
}
