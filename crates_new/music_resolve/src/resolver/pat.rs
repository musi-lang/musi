use super::*;

use music_syntax::SyntaxElement;

use crate::string_lit::{decode_rune_lit, decode_string_lit, decode_template_no_subst};

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src> {
    pub(super) fn collect_pat_binders(&mut self, node: SyntaxNode<'tree, 'src>) -> Vec<Ident> {
        let mut out = Vec::new();
        let mut seen = std::collections::HashSet::<Symbol>::new();
        self.collect_pat_binders_rec(node, &mut out, &mut seen);
        out
    }

    fn collect_pat_binders_rec(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut std::collections::HashSet<Symbol>,
    ) {
        match node.kind() {
            music_syntax::SyntaxNodeKind::BindPat => self.collect_bind_pat_binders(node, out, seen),
            music_syntax::SyntaxNodeKind::AsPat => self.collect_as_pat_binders(node, out, seen),
            music_syntax::SyntaxNodeKind::OrPat => {
                for child in node.child_nodes().filter(|n| n.kind().is_pat()) {
                    self.collect_pat_binders_rec(child, out, seen);
                }
            }
            music_syntax::SyntaxNodeKind::RecordPat => self.collect_record_pat_binders(node, out, seen),
            kind if kind.is_pat() => {
                for child in node.child_nodes().filter(|n| n.kind().is_pat()) {
                    self.collect_pat_binders_rec(child, out, seen);
                }
            }
            _ => {}
        }
    }

    fn collect_bind_pat_binders(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        out: &mut Vec<Ident>,
        seen: &mut std::collections::HashSet<Symbol>,
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
        seen: &mut std::collections::HashSet<Symbol>,
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
        seen: &mut std::collections::HashSet<Symbol>,
    ) {
        let children: Vec<_> = node.children().collect();
        let mut i: usize = 0;
        while i < children.len() {
            let is_mut = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::KwMut);
            if is_mut {
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
            music_syntax::SyntaxNodeKind::WildcardPat => self.lower_pat_wildcard(node),
            music_syntax::SyntaxNodeKind::LiteralPat => self.lower_pat_lit(node),
            music_syntax::SyntaxNodeKind::BindPat => self.lower_pat_bind(node),
            music_syntax::SyntaxNodeKind::VariantPat => self.lower_pat_variant(node),
            music_syntax::SyntaxNodeKind::TuplePat => self.lower_pat_tuple(node),
            music_syntax::SyntaxNodeKind::ArrayPat => self.lower_pat_array(node),
            music_syntax::SyntaxNodeKind::RecordPat => self.lower_pat_record(node),
            music_syntax::SyntaxNodeKind::OrPat => self.lower_pat_or(node),
            music_syntax::SyntaxNodeKind::AsPat => self.lower_pat_as(node),
            _ => self.alloc_error_pat(node),
        }
    }

    fn alloc_error_pat(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Error,
        })
    }

    fn lower_pat_wildcard(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Wildcard,
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
            TokenKind::Int => music_hir::HirLitKind::Int { raw: raw.into() },
            TokenKind::Float => music_hir::HirLitKind::Float { raw: raw.into() },
            TokenKind::String => decode_string_lit(raw).map_or_else(
                |_| music_hir::HirLitKind::String { value: "".into() },
                |s| music_hir::HirLitKind::String { value: s.into() },
            ),
            TokenKind::Rune => decode_rune_lit(raw).map_or_else(
                |_| music_hir::HirLitKind::Rune { value: 0 },
                |v| music_hir::HirLitKind::Rune { value: v },
            ),
            TokenKind::TemplateNoSubst => decode_template_no_subst(raw).map_or_else(
                |_| music_hir::HirLitKind::String { value: "".into() },
                |s| music_hir::HirLitKind::String { value: s.into() },
            ),
            _ => music_hir::HirLitKind::Int { raw: "0".into() },
        };

        let lit = self.store.alloc_lit(music_hir::HirLit {
            origin: self.origin_token(tok),
            kind,
        });
        let expr = self.alloc_expr(origin, HirExprKind::Lit { lit });
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Lit { expr },
        })
    }

    fn lower_pat_bind(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| {
                let sym = self.interner.intern("_");
                Ident::new(sym, node.span())
            });
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Bind { name },
        })
    }

    fn lower_pat_variant(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let tag_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let tag = tag_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| {
                let sym = self.interner.intern("_");
                Ident::new(sym, node.span())
            });
        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_pat())
            .map(|n| self.lower_pat(n))
            .collect();
        let args = self.store.alloc_pat_list(args);
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Variant { tag, args },
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
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Tuple { items },
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
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Array { items },
        })
    }

    fn lower_pat_record(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let fields = self.lower_record_pat_fields(node);
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Record { fields },
        })
    }

    fn lower_pat_or(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let mut pats = node.child_nodes().filter(|n| n.kind().is_pat());
        let left = pats.next().map(|n| self.lower_pat(n)).unwrap_or_else(|| {
            self.alloc_error_pat(node)
        });
        let right = pats.next().map(|n| self.lower_pat(n)).unwrap_or_else(|| {
            self.alloc_error_pat(node)
        });
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::Or { left, right },
        })
    }

    fn lower_pat_as(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirPatId {
        let origin = self.origin_node(node);
        let pat = node
            .child_nodes()
            .find(|n| n.kind().is_pat())
            .map(|n| self.lower_pat(n))
            .unwrap_or_else(|| self.alloc_error_pat(node));
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| {
                let sym = self.interner.intern("_");
                Ident::new(sym, node.span())
            });
        self.store.alloc_pat(music_hir::HirPat {
            origin,
            kind: music_hir::HirPatKind::As { pat, name },
        })
    }

    fn lower_record_pat_fields(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> music_arena::SliceRange<music_hir::HirRecordPatField> {
        let children: Vec<_> = node.children().collect();
        let mut fields = Vec::<music_hir::HirRecordPatField>::new();
        let mut i: usize = 0;
        while i < children.len() {
            let is_mut = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .is_some_and(|t| t.kind() == TokenKind::KwMut);
            if is_mut {
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
            let value = if has_colon {
                i += 1;
                let v = children
                    .get(i)
                    .copied()
                    .and_then(SyntaxElement::into_node)
                    .filter(|n| n.kind().is_pat())
                    .map(|n| self.lower_pat(n));
                if v.is_some() {
                    i += 1;
                }
                v
            } else {
                None
            };

            fields.push(music_hir::HirRecordPatField {
                is_mut,
                name,
                value,
            });
        }
        self.store.record_pat_fields.alloc_from_iter(fields)
    }
}

