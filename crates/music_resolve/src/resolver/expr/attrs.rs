use super::*;

use music_names::Symbol;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);

        let export_mod_node = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ExportMod);
        let (export_mod, export_foreign_abi) =
            export_mod_node.map_or((None, None), |node| self.parse_export_mod(node));

        let mut mods = HirMods::EMPTY.with_attrs(attrs);
        if let Some(export_mod) = export_mod {
            mods = mods.with_export(export_mod);
        }
        if node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::KwPartial)
        {
            mods = mods.with_partial();
        }
        if export_mod_node.is_some()
            && (export_foreign_abi.is_some() || is_foreign_group_target(&node))
        {
            mods = mods.with_foreign(HirForeignMod::new(export_foreign_abi));
        }

        let target = node.child_nodes().find(|n| {
            !matches!(n.kind(), SyntaxNodeKind::Attr | SyntaxNodeKind::ExportMod)
                && (n.kind().is_expr() || n.kind() == SyntaxNodeKind::MemberList)
        });

        match target {
            Some(target) if target.kind() == SyntaxNodeKind::MemberList => {
                let mut exprs = Vec::<HirExprId>::new();
                for member in target
                    .child_nodes()
                    .filter(|n| n.kind() == SyntaxNodeKind::Member)
                {
                    exprs.push(self.lower_foreign_member_let(member, mods.clone()));
                }
                let exprs = self.store.alloc_expr_list(exprs);
                self.alloc_expr(origin, HirExprKind::Sequence { exprs })
            }
            Some(target) if target.kind() == SyntaxNodeKind::ForeignBlockExpr => {
                self.lower_foreign_block_expr_with_mods(target, mods)
            }
            Some(target) => {
                let expr_id = self.lower_expr(target);
                self.apply_mods(expr_id, mods);
                expr_id
            }
            None => self.error_expr(origin),
        }
    }

    pub(super) fn lower_attrs(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirAttr> {
        let attrs: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Attr)
            .map(|n| self.lower_attr(n))
            .collect();
        self.store.attrs.alloc_from_iter(attrs)
    }

    pub(super) fn lower_attr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttr {
        let origin = self.origin_node(node);
        let path: Vec<_> = node
            .child_tokens()
            .filter(|t| Self::is_ident_token_kind(t.kind()))
            .filter_map(|t| self.intern_ident_token(t))
            .collect();
        let path = self.store.idents.alloc_from_iter(path);

        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::AttrArg)
            .map(|n| self.lower_attr_arg(n))
            .collect();
        let args = self.store.attr_args.alloc_from_iter(args);

        HirAttr::new(origin, path, args)
    }

    pub(super) fn lower_attr_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttrArg {
        let name = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            node.child_tokens()
                .find(|t| Self::is_ident_token_kind(t.kind()))
                .and_then(|t| self.intern_ident_token(t))
        } else {
            None
        };
        let value = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirAttrArg::new(name, value)
    }

    fn parse_export_mod(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> (Option<HirExportMod>, Option<Symbol>) {
        debug_assert_eq!(node.kind(), SyntaxNodeKind::ExportMod);
        let opaque = node.child_tokens().any(|t| t.kind() == TokenKind::KwOpaque);
        let foreign_abi = if node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::KwForeign)
        {
            node.child_tokens()
                .find(|t| t.kind() == TokenKind::String)
                .and_then(SyntaxToken::text)
                .and_then(|raw| decode_string_lit(raw).ok())
                .map(|abi| self.interner.intern(abi.as_str()))
        } else {
            None
        };
        (Some(HirExportMod::new(opaque)), foreign_abi)
    }
}

fn is_foreign_group_target(node: &SyntaxNode<'_, '_>) -> bool {
    node.child_nodes()
        .any(|n| n.kind() == SyntaxNodeKind::MemberList)
}
