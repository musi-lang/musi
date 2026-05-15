use super::*;

use music_hir::HirLitKind;
use music_names::Symbol;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let foreign_abi = self.foreign_abi_from_attrs(attrs.clone());

        let export_mod_node = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ExportMod);
        let (export_mod, export_foreign_abi) =
            export_mod_node.map_or((None, None), Self::parse_export_mod);

        let mut mods = HirMods::EMPTY.with_attrs(attrs);
        if let Some(export_mod) = export_mod {
            mods = mods.with_export(export_mod);
        }
        if export_mod_node.is_some()
            && (export_foreign_abi.is_some() || is_foreign_group_target(&node))
        {
            mods = mods.with_native(HirNativeMod::new(export_foreign_abi));
        }
        if let Some(foreign_abi) = foreign_abi {
            mods = mods.with_native(HirNativeMod::new(Some(foreign_abi)));
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
        let arg_value = match node.child_nodes().find(|n| n.kind().is_expr()) {
            Some(expr) => self.lower_expr(expr),
            None => self.error_expr(self.origin_node(node)),
        };
        HirAttrArg::new(name, arg_value)
    }

    fn parse_export_mod(node: SyntaxNode<'tree, 'src>) -> (Option<HirExportMod>, Option<Symbol>) {
        debug_assert_eq!(node.kind(), SyntaxNodeKind::ExportMod);
        (Some(HirExportMod::new(false)), None)
    }

    fn foreign_abi_from_attrs(&mut self, attrs: SliceRange<HirAttr>) -> Option<Symbol> {
        for attr in self.store.attrs.get(attrs) {
            let path = self
                .store
                .idents
                .get(attr.path)
                .iter()
                .map(|ident| self.interner.resolve(ident.name))
                .collect::<Vec<_>>();
            if path.as_slice() != ["foreign"] {
                continue;
            }
            let mut positional_index: usize = 0;
            for arg in self.store.attr_args.get(attr.args.clone()) {
                let is_abi_arg = match arg.name.map(|ident| self.interner.resolve(ident.name)) {
                    Some("abi") => true,
                    Some(_) => false,
                    None => {
                        let is_first_positional = positional_index == 0;
                        positional_index += 1;
                        is_first_positional
                    }
                };
                if !is_abi_arg {
                    continue;
                }
                match self.store.exprs.get(arg.value).kind {
                    HirExprKind::Variant { tag, .. } | HirExprKind::Name { name: tag } => {
                        return Some(tag.name);
                    }
                    HirExprKind::Lit { lit } => {
                        if let HirLitKind::String { value } = &self.store.lits.get(lit).kind {
                            return Some(self.interner.intern(value));
                        }
                    }
                    _ => {}
                }
            }
            return Some(self.interner.intern("c"));
        }
        None
    }
}

fn is_foreign_group_target(node: &SyntaxNode<'_, '_>) -> bool {
    node.child_nodes()
        .any(|n| n.kind() == SyntaxNodeKind::MemberList)
}
