use super::*;

use std::collections::HashSet;

use music_arena::SliceRange;
use music_hir::{HirExprId, HirPat, HirPatId, HirPatKind, HirRecordPatField};
use music_syntax::SyntaxElement;
use music_syntax::{SyntaxNodeKind, pattern_binder_tokens};

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn collect_pat_binders(&mut self, node: SyntaxNode<'tree, 'src>) -> Vec<Ident> {
        let mut out = Vec::new();
        let mut seen = HashSet::<Symbol>::new();
        for token in pattern_binder_tokens(node) {
            let Some(ident) = self.intern_ident_token(token) else {
                continue;
            };
            if seen.insert(ident.name) {
                out.push(ident);
            }
        }
        out
    }

    pub(super) fn lower_pat(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
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

    fn alloc_error_pat(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Error,
        })
    }

    fn lower_pat_wildcard(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Wildcard,
        })
    }

    fn lower_pat_lit(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        let Some(tok) = node.child_tokens().next() else {
            return self.alloc_error_pat(node);
        };
        let Some(lit) = self.alloc_lit_from_token(tok) else {
            return self.alloc_error_pat(node);
        };
        let expr: HirExprId = self.alloc_expr(origin, HirExprKind::Lit { lit });
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Lit { expr },
        })
    }

    fn lower_pat_bind(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        let tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| {
                let sym = self.interner.intern("_");
                Ident::new(sym, node.span())
            });
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Bind { name },
        })
    }

    fn lower_pat_variant(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
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
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Variant { tag, args },
        })
    }

    fn lower_pat_tuple(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
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

    fn lower_pat_array(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
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

    fn lower_pat_record(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        let fields = self.lower_record_pat_fields(node);
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Record { fields },
        })
    }

    fn lower_pat_or(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        let mut pats = node.child_nodes().filter(|n| n.kind().is_pat());
        let left = match pats.next() {
            Some(pat) => self.lower_pat(pat),
            None => self.alloc_error_pat(node),
        };
        let right = match pats.next() {
            Some(pat) => self.lower_pat(pat),
            None => self.alloc_error_pat(node),
        };
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::Or { left, right },
        })
    }

    fn lower_pat_as(&mut self, node: SyntaxNode<'tree, 'src>) -> HirPatId {
        let origin = self.origin_node(node);
        let pat = node.child_nodes().find(|n| n.kind().is_pat());
        let pat = match pat {
            Some(pat) => self.lower_pat(pat),
            None => self.alloc_error_pat(node),
        };
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| {
                let sym = self.interner.intern("_");
                Ident::new(sym, node.span())
            });
        self.store.alloc_pat(HirPat {
            origin,
            kind: HirPatKind::As { pat, name },
        })
    }

    fn lower_record_pat_fields(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirRecordPatField> {
        let children: Vec<_> = node.children().collect();
        let mut fields = Vec::<HirRecordPatField>::new();
        let mut i: usize = 0;
        while i < children.len() {
            let Some(name_tok) = children
                .get(i)
                .copied()
                .and_then(SyntaxElement::into_token)
                .filter(|t| t.kind() == TokenKind::Ident)
            else {
                i += 1;
                continue;
            };
            let name = self.intern_ident_token_or_placeholder(Some(name_tok), name_tok.span());
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

            fields.push(HirRecordPatField {
                name,
                value,
            });
        }
        self.store.record_pat_fields.alloc_from_iter(fields)
    }
}
