use super::*;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src> {
    pub(super) fn collect_pat_binders(&mut self, node: SyntaxNode<'tree, 'src>) -> Vec<Ident> {
        let mut out = Vec::new();
        let mut seen = HashSet::<Symbol>::new();
        self.collect_pat_binders_rec(node, &mut out, &mut seen);
        out
    }

    fn collect_pat_binders_rec(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut HashSet<Symbol>,
    ) {
        match node.kind() {
            SyntaxNodeKind::BindPat => self.collect_bind_pat_binders(node, out, seen),
            SyntaxNodeKind::AsPat => self.collect_as_pat_binders(node, out, seen),
            SyntaxNodeKind::OrPat => {
                for child in node.child_nodes().filter(|n| n.kind().is_pat()) {
                    self.collect_pat_binders_rec(child, out, seen);
                }
            }
            SyntaxNodeKind::VariantPat | SyntaxNodeKind::TuplePat | SyntaxNodeKind::ArrayPat => {
                for child in node.child_nodes().filter(|n| n.kind().is_pat()) {
                    self.collect_pat_binders_rec(child, out, seen);
                }
            }
            SyntaxNodeKind::RecordPat => self.collect_record_pat_binders(node, out, seen),
            _ => {}
        }
    }

    fn collect_bind_pat_binders(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut HashSet<Symbol>,
    ) {
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let Some(ident) = tok.and_then(|t| self.intern_ident_token(t)) else {
            return;
        };
        if seen.insert(ident.name) {
            out.push(ident);
        }
    }

    fn collect_as_pat_binders(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut HashSet<Symbol>,
    ) {
        if let Some(inner) = node.child_nodes().find(|n| n.kind().is_pat()) {
            self.collect_pat_binders_rec(inner, out, seen);
        }
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let Some(ident) = tok.and_then(|t| self.intern_ident_token(t)) else {
            return;
        };
        if seen.insert(ident.name) {
            out.push(ident);
        }
    }

    fn collect_record_pat_binders(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut HashSet<Symbol>,
    ) {
        let children: Vec<_> = node.children().collect();
        let mut i: usize = 0;
        while i < children.len() {
            if children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::KwMut)
            {
                i += 1;
            }
            let Some(name_tok) = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .filter(|t| t.kind() == TokenKind::Ident)
            else {
                i += 1;
                continue;
            };
            let name = self.intern_ident_token(name_tok);
            i += 1;
            let has_colon = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::Colon);
            if has_colon {
                i += 1;
                if let Some(pat) = children
                    .get(i)
                    .copied()
                    .and_then(SyntaxElement::into_node)
                    .filter(|n| n.kind().is_pat())
                {
                    self.collect_pat_binders_rec(pat, out, seen);
                    i += 1;
                }
                continue;
            }
            if let Some(name) = name {
                if seen.insert(name.name) {
                    out.push(name);
                }
            }
        }
    }

    pub(super) fn lower_pat(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        match node.kind() {
            SyntaxNodeKind::WildcardPat => self.lower_pat_wildcard(node),
            SyntaxNodeKind::LiteralPat => self.lower_pat_lit(node),
            SyntaxNodeKind::BindPat => self.lower_pat_bind(node),
            SyntaxNodeKind::VariantPat => self.lower_pat_variant(node),
            SyntaxNodeKind::TuplePat => self.lower_pat_tuple(node),
            SyntaxNodeKind::ArrayPat => self.lower_pat_array(node),
            SyntaxNodeKind::RecordPat => self.lower_pat_record(node),
            SyntaxNodeKind::OrPat => self.lower_pat_or(node),
            SyntaxNodeKind::AsPat => self.lower_pat_as(node),
            _ => self.alloc_error_pat(node),
        }
    }

    fn alloc_error_pat(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Error,
        })
    }

    fn lower_pat_wildcard(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Wildcard,
        })
    }

    fn lower_pat_lit(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let Some(tok) = node.child_tokens().next() else {
            return self.alloc_error_pat(node);
        };
        let Some(raw) = tok.text() else {
            return self.alloc_error_pat(node);
        };

        let kind = match tok.kind() {
            TokenKind::Int => HirLitKind::Int { raw: raw.into() },
            TokenKind::Float => HirLitKind::Float { raw: raw.into() },
            TokenKind::String => decode_string_lit(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |s| HirLitKind::String { value: s.into() },
            ),
            TokenKind::Rune => decode_rune_lit(raw).map_or_else(
                |_| HirLitKind::Rune { value: 0 },
                |v| HirLitKind::Rune { value: v },
            ),
            TokenKind::TemplateNoSubst => decode_template_lit(raw).map_or_else(
                |_| HirLitKind::String { value: "".into() },
                |s| HirLitKind::String { value: s.into() },
            ),
            _ => HirLitKind::Int { raw: "0".into() },
        };

        let lit = self.store.alloc_lit(HirLit {
            origin: self.origin_token(tok),
            kind,
        });
        let expr = self.alloc_expr(origin, HirExprKind::Lit { lit });
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Lit { expr },
        })
    }

    fn lower_pat_bind(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Bind { name },
        })
    }

    fn lower_pat_variant(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let tag_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let tag = tag_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_pat())
            .map(|n| self.lower_pat(n))
            .collect();
        let args = self.store.alloc_pat_list(args);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Variant { tag, args },
        })
    }

    fn lower_pat_tuple(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let items: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_pat())
            .map(|n| self.lower_pat(n))
            .collect();
        let items = self.store.alloc_pat_list(items);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Tuple { items },
        })
    }

    fn lower_pat_array(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let items: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_pat())
            .map(|n| self.lower_pat(n))
            .collect();
        let items = self.store.alloc_pat_list(items);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Array { items },
        })
    }

    fn lower_pat_record(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let fields = self.lower_record_pat_fields(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Record { fields },
        })
    }

    fn lower_pat_or(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let mut pats = node.child_nodes().filter(|n| n.kind().is_pat());
        let left = if let Some(n) = pats.next() {
            self.lower_pat(n)
        } else {
            self.alloc_error_pat(node)
        };
        let right = if let Some(n) = pats.next() {
            self.lower_pat(n)
        } else {
            self.alloc_error_pat(node)
        };
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Or { left, right },
        })
    }

    fn lower_pat_as(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes().filter(|n| n.kind().is_pat());
        let pat = if let Some(n) = nodes.next() {
            self.lower_pat(n)
        } else {
            self.alloc_error_pat(node)
        };
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::As { pat, name },
        })
    }

    fn lower_record_pat_fields(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirRecordPatField> {
        let mut out = Vec::new();
        let children: Vec<_> = node.children().collect();
        let mut i: usize = 0;
        while i < children.len() {
            let mut is_mut = false;
            if children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::KwMut)
            {
                is_mut = true;
                i += 1;
            }
            let Some(name_tok) = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .filter(|t| t.kind() == TokenKind::Ident)
            else {
                i += 1;
                continue;
            };
            let name = self
                .intern_ident_token(name_tok)
                .unwrap_or_else(|| Ident::new(self.interner.intern("_"), name_tok.span()));
            i += 1;
            let has_colon = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::Colon);
            if has_colon {
                i += 1;
                let value = children
                    .get(i)
                    .copied()
                    .and_then(SyntaxElement::into_node)
                    .filter(|n| n.kind().is_pat())
                    .map(|n| self.lower_pat(n));
                if let Some(value) = value {
                    out.push(HirRecordPatField {
                        is_mut,
                        name,
                        value: Some(value),
                    });
                    i += 1;
                    continue;
                }
            }
            out.push(HirRecordPatField {
                is_mut,
                name,
                value: None,
            });
        }
        self.store.record_pat_fields.alloc_from_iter(out)
    }
}
