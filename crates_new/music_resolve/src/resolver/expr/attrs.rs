use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);

        let export_mod = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ExportMod);
        let (export_opaque, export_foreign_abi) = export_mod
            .map(|n| parse_export_mod(n))
            .unwrap_or((false, None));

        let target = node.child_nodes().find(|n| {
            !matches!(n.kind(), SyntaxNodeKind::Attr | SyntaxNodeKind::ExportMod)
                && (n.kind().is_expr() || n.kind() == SyntaxNodeKind::MemberList)
        });

        let mut expr = match target {
            Some(target) if target.kind() == SyntaxNodeKind::MemberList => {
                let decls = self.lower_foreign_group_decls(target);
                self.alloc_expr(
                    self.origin_node(target),
                    HirExprKind::Foreign {
                        abi: export_foreign_abi.clone(),
                        decls,
                    },
                )
            }
            Some(target) => self.lower_expr(target),
            None => self.error_expr(origin),
        };

        if export_mod.is_some() {
            expr = self.alloc_expr(
                origin,
                HirExprKind::Export {
                    opaque: export_opaque,
                    foreign_abi: export_foreign_abi,
                    expr,
                },
            );
        }

        if attrs.clone().is_empty() {
            return expr;
        }
        self.alloc_expr(origin, HirExprKind::Attributed { attrs, expr })
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

    pub(super) fn lower_attr_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttrArg {
        let name = if node.child_tokens().any(|t| t.kind() == TokenKind::Eq) {
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
}

fn parse_export_mod(node: SyntaxNode<'_, '_>) -> (bool, Option<Box<str>>) {
    debug_assert_eq!(node.kind(), SyntaxNodeKind::ExportMod);
    let opaque = node.child_tokens().any(|t| t.kind() == TokenKind::KwOpaque);
    let foreign_abi = if node.child_tokens().any(|t| t.kind() == TokenKind::KwForeign) {
        node.child_tokens()
            .find(|t| t.kind() == TokenKind::String)
            .and_then(SyntaxToken::text)
            .and_then(|raw| decode_string_lit(raw).ok())
            .map(String::into_boxed_str)
    } else {
        None
    };
    (opaque, foreign_abi)
}
