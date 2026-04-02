use std::collections::{HashMap, HashSet};

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_base::{SourceId, Span};
use music_hir::{
    HirAccessKind, HirArg, HirArrayItem, HirAttr, HirAttrArg, HirBinaryOp, HirCaseArm,
    HirConstraint, HirConstraintKind, HirDim, HirEffectItem, HirEffectSet, HirExpr, HirExprKind,
    HirFieldDef, HirForeignDecl, HirHandleClause, HirLit, HirLitKind, HirMemberDef, HirMemberKind,
    HirModule, HirOrigin, HirParam, HirPat, HirPatKind, HirPrefixOp, HirQuoteKind, HirRecordItem,
    HirRecordPatField, HirSpliceKind, HirStore, HirTemplatePart, HirTy, HirTyKind, HirVariantDef,
};
use music_module::{ImportEnv, ImportSiteKind, ModuleKey, ModuleSpecifier};
use music_names::{
    Ident, Interner, KnownSymbols, NameBinding, NameBindingId, NameBindingKind, NameResolution,
    NameSite, Symbol,
};
use music_syntax::{
    SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree, TokenKind,
    canonical_name_text,
};

use crate::string_lit::{
    decode_rune_lit, decode_string_lit, decode_template_chunk, decode_template_lit,
};

mod pat;

#[derive(Default)]
pub struct ResolveOptions<'env> {
    pub prelude: Vec<Symbol>,
    pub import_env: Option<&'env dyn ImportEnv>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedImport {
    pub span: Span,
    pub spec: ModuleSpecifier,
    pub to: ModuleKey,
}

#[derive(Debug)]
pub struct ResolvedModule {
    pub module: HirModule,
    pub imports: Vec<ResolvedImport>,
    pub names: NameResolution,
    pub diags: Vec<Diag>,
}

#[must_use]
pub fn resolve_module(
    source_id: SourceId,
    module_key: &ModuleKey,
    tree: &SyntaxTree<'_>,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
) -> ResolvedModule {
    let mut resolver = Resolver::new(source_id, tree, interner, options);
    let imports = resolver.discover_imports(module_key);
    let root = resolver.lower_source_file();
    let module = HirModule::new(source_id, resolver.store, root);
    ResolvedModule {
        module,
        imports,
        names: resolver.names,
        diags: resolver.diags,
    }
}

#[derive(Debug, Default)]
struct Scope {
    names: HashMap<Symbol, NameBindingId>,
}

struct Resolver<'a, 'env, 'tree, 'src> {
    source_id: SourceId,
    tree: &'tree SyntaxTree<'src>,
    interner: &'a mut Interner,
    import_env: Option<&'env dyn ImportEnv>,

    store: HirStore,
    error_ty: music_hir::HirTyId,

    names: NameResolution,
    diags: Vec<Diag>,
    scopes: Vec<Scope>,
}

impl<'a, 'env, 'tree, 'src> Resolver<'a, 'env, 'tree, 'src> {
    fn new(
        source_id: SourceId,
        tree: &'tree SyntaxTree<'src>,
        interner: &'a mut Interner,
        options: ResolveOptions<'env>,
    ) -> Self {
        let mut store = HirStore::new();
        let error_ty = store.alloc_ty(HirTy {
            origin: HirOrigin::dummy(),
            kind: HirTyKind::Error,
        });

        let mut names = NameResolution::new();
        let mut root = Scope::default();

        let known = KnownSymbols::new(interner);
        for sym in known.compiler_prelude() {
            let binding = names.alloc_binding(NameBinding {
                name: sym,
                site: NameSite::new(source_id, Span::DUMMY),
                kind: NameBindingKind::Prelude,
            });
            let _prev = root.names.insert(sym, binding);
        }
        for sym in options.prelude {
            if root.names.contains_key(&sym) {
                continue;
            }
            let binding = names.alloc_binding(NameBinding {
                name: sym,
                site: NameSite::new(source_id, Span::DUMMY),
                kind: NameBindingKind::Prelude,
            });
            let _prev = root.names.insert(sym, binding);
        }

        Self {
            source_id,
            tree,
            interner,
            import_env: options.import_env,
            store,
            error_ty,
            names,
            diags: Vec::new(),
            scopes: vec![root],
        }
    }

    fn discover_imports(&mut self, module_key: &ModuleKey) -> Vec<ResolvedImport> {
        let Some(env) = self.import_env else {
            return Vec::new();
        };

        let sites = music_module::collect_import_sites(self.source_id, self.tree);
        let mut imports = Vec::new();
        for site in sites {
            match site.kind {
                ImportSiteKind::Static { spec } => match env.resolve(module_key, &spec) {
                    Ok(to) => imports.push(ResolvedImport {
                        span: site.span,
                        spec,
                        to,
                    }),
                    Err(err) => {
                        self.diags
                            .push(Diag::error("import resolve failed").with_label(
                                site.span,
                                self.source_id,
                                format!("{:?}", err.kind),
                            ));
                    }
                },
                ImportSiteKind::InvalidStringLit => {
                    self.diags
                        .push(Diag::error("invalid import spec").with_label(
                            site.span,
                            self.source_id,
                            "string literal invalid",
                        ));
                }
                ImportSiteKind::Dynamic => {}
            }
        }
        imports
    }

    fn alloc_expr(&mut self, origin: HirOrigin, kind: HirExprKind) -> music_hir::HirExprId {
        self.store.alloc_expr(HirExpr {
            origin,
            kind,
            ty: self.error_ty,
        })
    }

    fn origin_node(&self, node: SyntaxNode<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, node.span())
    }

    fn origin_token(&self, tok: SyntaxToken<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, tok.span())
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(Scope::default());
        }
    }

    fn insert_binding(&mut self, ident: Ident, kind: NameBindingKind) -> NameBindingId {
        let binding = self.names.alloc_binding(NameBinding {
            name: ident.name,
            site: NameSite::new(self.source_id, ident.span),
            kind,
        });
        if let Some(scope) = self.scopes.last_mut() {
            let _prev = scope.names.insert(ident.name, binding);
        }
        binding
    }

    fn lookup(&self, sym: Symbol) -> Option<NameBindingId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.names.get(&sym).copied() {
                return Some(id);
            }
        }
        None
    }

    fn record_use(&mut self, ident: Ident) {
        let site = NameSite::new(self.source_id, ident.span);
        let Some(binding) = self.lookup(ident.name) else {
            self.diags.push(Diag::error("unbound name").with_label(
                ident.span,
                self.source_id,
                "name not found",
            ));
            return;
        };
        self.names.record_ref(site, binding);
    }

    fn intern_ident_token(&mut self, tok: SyntaxToken<'tree, 'src>) -> Option<Ident> {
        let raw = tok.text()?;
        let canon = canonical_name_text(tok.kind(), raw);
        let sym = self.interner.intern(canon);
        Some(Ident::new(sym, tok.span()))
    }

    fn lower_source_file(&mut self) -> music_hir::HirExprId {
        let root = self.tree.root();
        if root.kind() != SyntaxNodeKind::SourceFile {
            return self.alloc_expr(self.origin_node(root), HirExprKind::Error);
        }

        let mut exprs = Vec::new();
        for child in root.child_nodes() {
            if let Some(stmt) = stmt_wrapper_expr(child) {
                let hir = self.lower_stmt_expr(stmt);
                exprs.push(hir);
            } else if child.kind().is_expr() {
                let hir = self.lower_expr(child);
                exprs.push(hir);
            }
        }
        let range = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            self.origin_node(root),
            HirExprKind::Sequence { exprs: range },
        )
    }

    fn lower_stmt_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        match node.kind() {
            SyntaxNodeKind::LetExpr => self.lower_let_expr(node, NameBindingKind::Let, true),
            SyntaxNodeKind::ExportExpr => self.lower_export_expr(node),
            SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node, true),
            _ => self.lower_expr(node),
        }
    }

    fn lower_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        match node.kind() {
            SyntaxNodeKind::Error => self.alloc_expr(origin, HirExprKind::Error),

            SyntaxNodeKind::SequenceExpr => self.lower_sequence_expr(node),
            SyntaxNodeKind::LetExpr => self.lower_let_expr(node, NameBindingKind::Let, false),
            SyntaxNodeKind::ImportExpr => self.lower_import_expr(node),
            SyntaxNodeKind::ExportExpr => self.lower_export_expr(node),
            SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(node, false),
            SyntaxNodeKind::DataExpr => self.lower_data_expr(node),
            SyntaxNodeKind::EffectExpr => self.lower_effect_expr(node),
            SyntaxNodeKind::ClassExpr => self.lower_class_expr(node),
            SyntaxNodeKind::InstanceExpr => self.lower_instance_expr(node),

            SyntaxNodeKind::NameExpr => {
                let Some(tok) = node
                    .child_tokens()
                    .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent))
                else {
                    self.diags
                        .push(Diag::error("expected identifier").with_label(
                            node.span(),
                            self.source_id,
                            "identifier missing",
                        ));
                    return self.alloc_expr(origin, HirExprKind::Error);
                };
                let Some(ident) = self.intern_ident_token(tok) else {
                    return self.alloc_expr(origin, HirExprKind::Error);
                };
                self.record_use(ident);
                self.alloc_expr(origin, HirExprKind::Name { name: ident })
            }

            SyntaxNodeKind::LiteralExpr => self.lower_literal_expr(node),
            SyntaxNodeKind::TemplateExpr => self.lower_template_expr(node),

            SyntaxNodeKind::TupleExpr => {
                let items: Vec<_> = node
                    .child_nodes()
                    .filter(|n| n.kind().is_expr())
                    .map(|n| self.lower_expr(n))
                    .collect();
                let range = self.store.alloc_expr_list(items);
                self.alloc_expr(origin, HirExprKind::Tuple { items: range })
            }
            SyntaxNodeKind::ArrayExpr => self.lower_array_expr(node),
            SyntaxNodeKind::RecordExpr => self.lower_record_expr(node),
            SyntaxNodeKind::VariantExpr => self.lower_variant_expr(node),
            SyntaxNodeKind::PiExpr => self.lower_pi_expr(node),
            SyntaxNodeKind::LambdaExpr => self.lower_lambda_expr(node),

            SyntaxNodeKind::CallExpr => self.lower_call_expr(node),
            SyntaxNodeKind::ApplyExpr => self.lower_apply_expr(node),
            SyntaxNodeKind::IndexExpr => self.lower_index_expr(node),
            SyntaxNodeKind::FieldExpr => self.lower_field_expr(node),
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
            SyntaxNodeKind::AttributedExpr => self.lower_attributed_expr(node),
            _ => {
                self.diags
                    .push(Diag::error("expected expression").with_label(
                        node.span(),
                        self.source_id,
                        "expr form unsupported",
                    ));
                self.alloc_expr(origin, HirExprKind::Error)
            }
        }
    }

    fn lower_sequence_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();
        let mut exprs = Vec::new();
        for child in node.child_nodes().filter(|n| n.kind().is_expr()) {
            let hir = match child.kind() {
                SyntaxNodeKind::LetExpr => self.lower_let_expr(child, NameBindingKind::Let, true),
                SyntaxNodeKind::ExportExpr => self.lower_export_expr(child),
                SyntaxNodeKind::ForeignBlockExpr => self.lower_foreign_block_expr(child, true),
                _ => self.lower_expr(child),
            };
            exprs.push(hir);
        }
        self.pop_scope();
        let range = self.store.alloc_expr_list(exprs);
        self.alloc_expr(origin, HirExprKind::Sequence { exprs: range })
    }

    fn lower_literal_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let Some(tok) = node.child_tokens().next() else {
            self.diags.push(Diag::error("expected literal").with_label(
                node.span(),
                self.source_id,
                "literal missing",
            ));
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let Some(raw) = tok.text() else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let lit = match tok.kind() {
            TokenKind::Int => HirLitKind::Int { raw: raw.into() },
            TokenKind::Float => HirLitKind::Float { raw: raw.into() },
            TokenKind::String => match decode_string_lit(raw) {
                Ok(s) => HirLitKind::String { value: s.into() },
                Err(err) => {
                    self.diags
                        .push(Diag::error("string decode failed").with_label(
                            tok.span(),
                            self.source_id,
                            err.to_string(),
                        ));
                    HirLitKind::String { value: "".into() }
                }
            },
            TokenKind::Rune => match decode_rune_lit(raw) {
                Ok(v) => HirLitKind::Rune { value: v },
                Err(err) => {
                    self.diags
                        .push(Diag::error("rune decode failed").with_label(
                            tok.span(),
                            self.source_id,
                            err.to_string(),
                        ));
                    HirLitKind::Rune { value: 0 }
                }
            },
            _ => {
                self.diags.push(Diag::error("expected literal").with_label(
                    tok.span(),
                    self.source_id,
                    "literal kind unsupported",
                ));
                HirLitKind::Int { raw: "0".into() }
            }
        };
        let lit_id = self.store.alloc_lit(HirLit {
            origin: self.origin_token(tok),
            kind: lit,
        });
        self.alloc_expr(origin, HirExprKind::Lit { lit: lit_id })
    }

    fn lower_template_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        let mut parts = Vec::new();
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        for tok in node.child_tokens() {
            let Some(raw) = tok.text() else {
                continue;
            };
            match tok.kind() {
                TokenKind::TemplateNoSubst => match decode_template_lit(raw) {
                    Ok(s) => parts.push(HirTemplatePart::Text { value: s.into() }),
                    Err(err) => {
                        self.diags
                            .push(Diag::error("template decode failed").with_label(
                                tok.span(),
                                self.source_id,
                                err.to_string(),
                            ));
                    }
                },
                TokenKind::TemplateHead => {
                    let text = raw.strip_prefix('`').and_then(|s| s.strip_suffix("${"));
                    if let Some(text) = text {
                        if let Ok(s) = decode_template_chunk(text) {
                            parts.push(HirTemplatePart::Text { value: s.into() });
                        }
                    }
                    if let Some(expr) = exprs.next() {
                        let expr = self.lower_expr(expr);
                        parts.push(HirTemplatePart::Expr { expr });
                    }
                }
                TokenKind::TemplateMiddle => {
                    let text = raw.strip_prefix('}').and_then(|s| s.strip_suffix("${"));
                    if let Some(text) = text {
                        if let Ok(s) = decode_template_chunk(text) {
                            parts.push(HirTemplatePart::Text { value: s.into() });
                        }
                    }
                    if let Some(expr) = exprs.next() {
                        let expr = self.lower_expr(expr);
                        parts.push(HirTemplatePart::Expr { expr });
                    }
                }
                TokenKind::TemplateTail => {
                    let text = raw.strip_prefix('}').and_then(|s| s.strip_suffix('`'));
                    if let Some(text) = text {
                        if let Ok(s) = decode_template_chunk(text) {
                            parts.push(HirTemplatePart::Text { value: s.into() });
                        }
                    }
                }
                _ => {}
            }
        }

        let range = self.store.template_parts.alloc_from_iter(parts);
        self.alloc_expr(origin, HirExprKind::Template { parts: range })
    }

    fn lower_array_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);

        if node
            .child_nodes()
            .any(|n| n.kind() == SyntaxNodeKind::ArrayItem)
        {
            let items: Vec<_> = node
                .child_nodes()
                .filter(|n| n.kind() == SyntaxNodeKind::ArrayItem)
                .map(|item| self.lower_array_item(item))
                .collect();
            let range = self.store.array_items.alloc_from_iter(items);
            return self.alloc_expr(origin, HirExprKind::Array { items: range });
        }

        let mut dims = Vec::new();
        let mut after_bracket = false;
        let mut item_expr = None;
        for elem in node.children() {
            match elem {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::RBracket => after_bracket = true,
                    TokenKind::Int if !after_bracket => {
                        let dim = self.lower_dim_token(tok);
                        dims.push(dim);
                    }
                    TokenKind::Ident if !after_bracket => {
                        if let Some(ident) = self.intern_ident_token(tok) {
                            dims.push(HirDim::Name(ident));
                        }
                    }
                    TokenKind::Underscore if !after_bracket => dims.push(HirDim::Unknown),
                    _ => {}
                },
                SyntaxElement::Node(n) => {
                    if after_bracket && n.kind().is_expr() {
                        item_expr = Some(n);
                    }
                }
            }
        }

        let Some(item_expr) = item_expr else {
            let range = self
                .store
                .array_items
                .alloc_from_iter(Vec::<HirArrayItem>::new());
            return self.alloc_expr(origin, HirExprKind::Array { items: range });
        };

        let item = self.lower_expr(item_expr);
        let dims = self.store.dims.alloc_from_iter(dims);
        self.alloc_expr(origin, HirExprKind::ArrayTy { dims, item })
    }

    fn lower_dim_token(&mut self, tok: SyntaxToken<'tree, 'src>) -> HirDim {
        let Some(raw) = tok.text() else {
            return HirDim::Unknown;
        };
        let Some(value) = parse_u32_lit(raw) else {
            self.diags
                .push(Diag::error("array dim parse failed").with_label(
                    tok.span(),
                    self.source_id,
                    "array dim invalid",
                ));
            return HirDim::Unknown;
        };
        HirDim::Int(value)
    }

    fn lower_array_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirArrayItem {
        let spread = node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::DotDotDot);
        let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
        };
        HirArrayItem { spread, expr }
    }

    fn lower_record_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let items: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::RecordItem)
            .map(|n| self.lower_record_item(n))
            .collect();
        let range = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::Record { items: range })
    }

    fn lower_record_item(&mut self, node: SyntaxNode<'tree, 'src>) -> HirRecordItem {
        let spread = node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::DotDotDot);
        if spread {
            let value = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
                self.lower_expr(n)
            } else {
                self.alloc_expr(self.origin_node(node), HirExprKind::Error)
            };
            return HirRecordItem {
                spread: true,
                name: None,
                value,
            };
        }

        let name_tok = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent));
        let name = name_tok.and_then(|t| self.intern_ident_token(t));
        let has_bind = node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq);
        let value = if has_bind {
            if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
                self.lower_expr(n)
            } else {
                self.alloc_expr(self.origin_node(node), HirExprKind::Error)
            }
        } else if let Some(name) = name {
            self.record_use(name);
            self.alloc_expr(self.origin_node(node), HirExprKind::Name { name })
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
        };
        HirRecordItem {
            spread: false,
            name,
            value,
        }
    }

    fn lower_variant_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let Some(tag_tok) = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::OpIdent))
        else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let Some(tag) = self.intern_ident_token(tag_tok) else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .collect();
        let args = self.store.alloc_expr_list(args);
        self.alloc_expr(origin, HirExprKind::Variant { tag, args })
    }

    fn lower_pi_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let binder_ty_node = nodes.next();
        let ret_node = nodes.next();

        let binder_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let arrow = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::MinusGt | TokenKind::TildeGt));

        let Some(binder_tok) = binder_tok else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let Some(binder) = self.intern_ident_token(binder_tok) else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let binder_ty = if let Some(n) = binder_ty_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let is_effectful = arrow.is_some_and(|t| t.kind() == TokenKind::TildeGt);

        self.push_scope();
        let _binding = self.insert_binding(binder, NameBindingKind::PiBinder);
        let ret = if let Some(n) = ret_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
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

    fn lower_lambda_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let param_list = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList);
        let expr_nodes: Vec<_> = node.child_nodes().filter(|n| n.kind().is_expr()).collect();
        let has_ret = node.child_tokens().any(|t| t.kind() == TokenKind::Colon);
        let first = expr_nodes.first().copied();
        let second = expr_nodes.get(1).copied();
        let (ret_node, body_node) = if has_ret {
            (first, second)
        } else {
            (None, first)
        };

        self.push_scope();
        let params = param_list.map_or(SliceRange::EMPTY, |p| {
            self.lower_param_list(p, NameBindingKind::Param)
        });
        let ret_ty = ret_node.map(|n| self.lower_expr(n));
        let body = if let Some(n) = body_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
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

    fn lower_param_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_kind: NameBindingKind,
    ) -> SliceRange<HirParam> {
        let mut params = Vec::new();
        for param in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Param)
        {
            if let Some(hir) = self.lower_param(param, bind_kind) {
                params.push(hir);
            }
        }
        self.store.params.alloc_from_iter(params)
    }

    fn lower_param(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_kind: NameBindingKind,
    ) -> Option<HirParam> {
        let is_mut = node.child_tokens().any(|t| t.kind() == TokenKind::KwMut);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident)?;
        let name = self.intern_ident_token(name_tok)?;
        let _binding = self.insert_binding(name, bind_kind);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let mut ty = None;
        let mut default = None;
        let mut seen_colon = false;
        for tok in node.child_tokens() {
            if tok.kind() == TokenKind::Colon {
                seen_colon = true;
            }
        }
        if seen_colon {
            ty = exprs.next().map(|n| self.lower_expr(n));
        }
        if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            default = exprs.next().map(|n| self.lower_expr(n));
        }

        Some(HirParam {
            is_mut,
            name,
            ty,
            default,
        })
    }

    fn lower_import_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let arg = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        self.alloc_expr(origin, HirExprKind::Import { arg })
    }

    fn lower_export_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
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
                .map(Into::into)
        } else {
            None
        };

        let mut inner = None;
        for n in node.child_nodes() {
            if n.kind().is_expr() || n.kind() == SyntaxNodeKind::MemberList {
                inner = Some(n);
            }
        }
        let expr = if let Some(n) = inner {
            if n.kind() == SyntaxNodeKind::MemberList {
                let decls = self.lower_foreign_group(n, true);
                self.alloc_expr(
                    self.origin_node(n),
                    HirExprKind::Foreign {
                        abi: foreign_abi.clone(),
                        decls,
                    },
                )
            } else {
                self.lower_stmt_expr(n)
            }
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
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

    fn lower_foreign_block_expr(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_into_scope: bool,
    ) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let abi = node
            .child_tokens()
            .find(|t| t.kind() == TokenKind::String)
            .and_then(SyntaxToken::text)
            .and_then(|raw| decode_string_lit(raw).ok())
            .map(Into::into);

        let decls_node = node.child_nodes().find(|n| {
            matches!(
                n.kind(),
                SyntaxNodeKind::LetExpr | SyntaxNodeKind::MemberList
            )
        });
        let Some(decls_node) = decls_node else {
            return self.alloc_expr(
                origin,
                HirExprKind::Foreign {
                    abi,
                    decls: SliceRange::EMPTY,
                },
            );
        };

        let decls = match decls_node.kind() {
            SyntaxNodeKind::LetExpr => self.lower_foreign_let_wrapper(decls_node, bind_into_scope),
            SyntaxNodeKind::MemberList => self.lower_foreign_group(decls_node, bind_into_scope),
            _ => SliceRange::EMPTY,
        };

        self.alloc_expr(origin, HirExprKind::Foreign { abi, decls })
    }

    fn lower_foreign_let_wrapper(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_into_scope: bool,
    ) -> SliceRange<HirForeignDecl> {
        let Some(member) = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::Member)
        else {
            return SliceRange::EMPTY;
        };
        let decl = self.lower_foreign_decl(member, bind_into_scope);
        self.store.foreign_decls.alloc_from_iter([decl])
    }

    fn lower_foreign_group(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_into_scope: bool,
    ) -> SliceRange<HirForeignDecl> {
        let decls: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Member)
            .map(|member| self.lower_foreign_decl(member, bind_into_scope))
            .collect();
        self.store.foreign_decls.alloc_from_iter(decls)
    }

    fn lower_foreign_decl(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_into_scope: bool,
    ) -> HirForeignDecl {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));

        if bind_into_scope {
            let _ = self.insert_binding(name, NameBindingKind::Let);
        }

        self.push_scope();
        let type_params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::TypeParamList)
            .map_or(SliceRange::EMPTY, |n| self.lower_type_param_list(n));

        let params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |n| {
                self.lower_param_list(n, NameBindingKind::Param)
            });

        let constraints = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |n| self.lower_constraint_list(n));

        let sig = if node.child_tokens().any(|t| t.kind() == TokenKind::Colon) {
            node.child_nodes()
                .filter(|n| n.kind().is_expr())
                .last()
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

    fn lower_let_expr(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
        bind_kind: NameBindingKind,
        bind_into_scope: bool,
    ) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let is_mut = node.child_tokens().any(|t| t.kind() == TokenKind::KwMut);
        let is_rec = node.child_tokens().any(|t| t.kind() == TokenKind::KwRec);
        let mods = music_hir::HirLetMods { is_mut, is_rec };

        let Some(pat_node) = node.child_nodes().find(|n| n.kind().is_pat()) else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let binders = self.collect_pat_binders(pat_node);

        if is_rec && bind_into_scope {
            for ident in binders.iter().copied() {
                let _ = self.insert_binding(ident, bind_kind);
            }
        }

        self.push_scope();
        if is_rec && !bind_into_scope {
            for ident in binders.iter().copied() {
                let _ = self.insert_binding(ident, bind_kind);
            }
        }

        let type_params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::TypeParamList)
            .map_or(SliceRange::EMPTY, |n| self.lower_type_param_list(n));

        let constraints = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |n| self.lower_constraint_list(n));

        let effects = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::EffectSet)
            .map(|n| self.lower_effect_set(n));

        let expr_nodes: Vec<_> = node.child_nodes().filter(|n| n.kind().is_expr()).collect();
        let has_sig = node.child_tokens().any(|t| t.kind() == TokenKind::Colon);
        let first = expr_nodes.first().copied();
        let second = expr_nodes.get(1).copied();
        let (sig_node, value_node) = if has_sig {
            (first, second)
        } else {
            (None, first)
        };

        let params_node = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList);
        let (params, sig, value) = if let Some(params_node) = params_node {
            self.push_scope();
            let params = self.lower_param_list(params_node, NameBindingKind::Param);
            let sig = sig_node.map(|n| self.lower_expr(n));
            let value = if let Some(n) = value_node {
                self.lower_expr(n)
            } else {
                self.alloc_expr(origin, HirExprKind::Error)
            };
            self.pop_scope();
            (params, sig, value)
        } else {
            let sig = sig_node.map(|n| self.lower_expr(n));
            let value = if let Some(n) = value_node {
                self.lower_expr(n)
            } else {
                self.alloc_expr(origin, HirExprKind::Error)
            };
            (SliceRange::EMPTY, sig, value)
        };
        self.pop_scope();

        let pat = self.lower_pat(pat_node);

        if bind_into_scope && !is_rec {
            for ident in binders.iter().copied() {
                let _ = self.insert_binding(ident, bind_kind);
            }
        }

        self.alloc_expr(
            origin,
            HirExprKind::Let {
                mods,
                pat,
                type_params,
                params,
                constraints,
                effects,
                sig,
                value,
            },
        )
    }

    fn lower_type_param_list(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<Ident> {
        let mut out = Vec::new();
        for tp in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::TypeParam)
        {
            let Some(tok) = tp.child_tokens().find(|t| t.kind() == TokenKind::Ident) else {
                continue;
            };
            let Some(ident) = self.intern_ident_token(tok) else {
                continue;
            };
            let _binding = self.insert_binding(ident, NameBindingKind::TypeParam);
            out.push(ident);
        }
        self.store.idents.alloc_from_iter(out)
    }

    fn lower_constraint_list(
        &mut self,
        node: SyntaxNode<'tree, 'src>,
    ) -> SliceRange<HirConstraint> {
        let mut out = Vec::new();
        for c in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Constraint)
        {
            if let Some(hir) = self.lower_constraint(c) {
                out.push(hir);
            }
        }
        self.store.constraints.alloc_from_iter(out)
    }

    fn lower_constraint(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirConstraint> {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident)?;
        let name = self.intern_ident_token(name_tok)?;
        self.record_use(name);
        let kind = if node.child_tokens().any(|t| t.kind() == TokenKind::LtColon) {
            HirConstraintKind::Subtype
        } else {
            HirConstraintKind::Implements
        };
        let value_node = node.child_nodes().find(|n| n.kind().is_expr())?;
        let value = self.lower_expr(value_node);
        Some(HirConstraint { name, kind, value })
    }

    fn lower_effect_set(&mut self, node: SyntaxNode<'tree, 'src>) -> HirEffectSet {
        let mut items = Vec::new();
        let mut open = None;
        let mut saw_dots = false;
        for elem in node.children() {
            match elem {
                SyntaxElement::Token(tok) => match tok.kind() {
                    TokenKind::DotDotDot => saw_dots = true,
                    TokenKind::Ident if saw_dots => {
                        open = self.intern_ident_token(tok);
                        saw_dots = false;
                    }
                    _ => {}
                },
                SyntaxElement::Node(n) => {
                    if n.kind() == SyntaxNodeKind::EffectItem {
                        if let Some(item) = self.lower_effect_item(n) {
                            items.push(item);
                        }
                    }
                }
            }
        }
        let items = self.store.effect_items.alloc_from_iter(items);
        HirEffectSet { items, open }
    }

    fn lower_effect_item(&mut self, node: SyntaxNode<'tree, 'src>) -> Option<HirEffectItem> {
        let name_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident)?;
        let name = self.intern_ident_token(name_tok)?;
        self.record_use(name);
        let arg = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n));
        Some(HirEffectItem { name, arg })
    }

    fn lower_call_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let callee_node = nodes.next();
        let callee = if let Some(n) = callee_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let args: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Arg)
            .map(|n| self.lower_arg(n))
            .collect();
        let args = self.store.args.alloc_from_iter(args);
        self.alloc_expr(origin, HirExprKind::Call { callee, args })
    }

    fn lower_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirArg {
        let spread = node
            .child_tokens()
            .any(|t| t.kind() == TokenKind::DotDotDot);
        let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
        };
        HirArg { spread, expr }
    }

    fn lower_apply_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let callee_node = nodes.next();
        let callee = if let Some(n) = callee_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let args: Vec<_> = nodes
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .collect();
        let args = self.store.alloc_expr_list(args);
        self.alloc_expr(origin, HirExprKind::Apply { callee, args })
    }

    fn lower_index_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes();
        let base_node = nodes.next();
        let base = if let Some(n) = base_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let args: Vec<_> = nodes
            .filter(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n))
            .collect();
        let args = self.store.alloc_expr_list(args);
        self.alloc_expr(origin, HirExprKind::Index { base, args })
    }

    fn lower_field_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let base = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let access = node.child_tokens().find(|t| {
            matches!(
                t.kind(),
                TokenKind::Dot | TokenKind::QDot | TokenKind::BangDot
            )
        });
        let access = match access.map(SyntaxToken::kind) {
            Some(TokenKind::QDot) => HirAccessKind::Optional,
            Some(TokenKind::BangDot) => HirAccessKind::Unwrap,
            _ => HirAccessKind::Direct,
        };
        let target = node
            .child_tokens()
            .find(|t| matches!(t.kind(), TokenKind::Ident | TokenKind::Int));
        let Some(target) = target else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        let name = if target.kind() == TokenKind::Ident {
            self.intern_ident_token(target)
        } else {
            target
                .text()
                .map(|s| Ident::new(self.interner.intern(s), target.span()))
        };
        let Some(name) = name else {
            return self.alloc_expr(origin, HirExprKind::Error);
        };
        self.alloc_expr(origin, HirExprKind::Field { base, access, name })
    }

    fn lower_record_update_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let base = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let items: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::RecordItem)
            .map(|n| self.lower_record_item(n))
            .collect();
        let items = self.store.record_items.alloc_from_iter(items);
        self.alloc_expr(origin, HirExprKind::RecordUpdate { base, items })
    }

    fn lower_type_test_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let base = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let ty = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let as_name = if node.child_tokens().any(|t| t.kind() == TokenKind::KwAs) {
            node.child_tokens()
                .find(|t| t.kind() == TokenKind::Ident)
                .and_then(|t| self.intern_ident_token(t))
        } else {
            None
        };
        self.alloc_expr(origin, HirExprKind::TypeTest { base, ty, as_name })
    }

    fn lower_type_cast_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let base = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let ty = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        self.alloc_expr(origin, HirExprKind::TypeCast { base, ty })
    }

    fn lower_prefix_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let op_tok = node.child_tokens().next();
        let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::Minus) => HirPrefixOp::Neg,
            Some(TokenKind::KwMut) => HirPrefixOp::Mut,
            _ => HirPrefixOp::Not,
        };
        self.alloc_expr(origin, HirExprKind::Prefix { op, expr })
    }

    fn lower_binary_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut nodes = node.child_nodes().filter(|n| n.kind().is_expr());
        let left = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let right = if let Some(n) = nodes.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        let op_tok = node.child_tokens().next();

        let op = match op_tok.map(SyntaxToken::kind) {
            Some(TokenKind::LtMinus) => HirBinaryOp::Assign,
            Some(TokenKind::PipeGt) => HirBinaryOp::Pipe,
            Some(TokenKind::MinusGt) => HirBinaryOp::Arrow,
            Some(TokenKind::TildeGt) => HirBinaryOp::EffectArrow,
            Some(TokenKind::KwOr) => HirBinaryOp::Or,
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
            Some(TokenKind::Minus) => HirBinaryOp::Sub,
            Some(TokenKind::Star) => HirBinaryOp::Mul,
            Some(TokenKind::Slash) => HirBinaryOp::Div,
            Some(TokenKind::Percent) => HirBinaryOp::Rem,
            Some(TokenKind::SymbolicOp) => {
                let tok = op_tok.expect("present");
                let raw = tok.text().unwrap_or("");
                let ident = Ident::new(self.interner.intern(raw), tok.span());
                HirBinaryOp::UserOp(ident)
            }
            _ => HirBinaryOp::Add,
        };

        self.alloc_expr(origin, HirExprKind::Binary { op, left, right })
    }

    fn lower_case_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let scrutinee = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };

        let arms: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::CaseArm)
            .map(|arm| self.lower_case_arm(arm))
            .collect();
        let arms = self.store.case_arms.alloc_from_iter(arms);
        self.alloc_expr(origin, HirExprKind::Case { scrutinee, arms })
    }

    fn lower_case_arm(&mut self, node: SyntaxNode<'tree, 'src>) -> HirCaseArm {
        let attrs = self.lower_attrs(node);
        let Some(pat_node) = node.child_nodes().find(|n| n.kind().is_pat()) else {
            return HirCaseArm {
                attrs,
                pat: self.store.alloc_pat(HirPat {
                    origin: self.origin_node(node),
                    kind: HirPatKind::Error,
                }),
                guard: None,
                expr: self.alloc_expr(self.origin_node(node), HirExprKind::Error),
            };
        };

        self.push_scope();
        let binders = self.collect_pat_binders(pat_node);
        for ident in binders {
            let _ = self.insert_binding(ident, NameBindingKind::PatternBind);
        }
        let pat = self.lower_pat(pat_node);

        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let guard = if node.child_tokens().any(|t| t.kind() == TokenKind::KwIf) {
            exprs.next().map(|n| self.lower_expr(n))
        } else {
            None
        };
        let expr = if let Some(n) = exprs.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
        };
        self.pop_scope();
        HirCaseArm {
            attrs,
            pat,
            guard,
            expr,
        }
    }

    fn lower_perform_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        self.alloc_expr(origin, HirExprKind::Perform { expr })
    }

    fn lower_handle_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let expr = if let Some(n) = exprs.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };

        let handler_tok = node.child_tokens().find(|t| t.kind() == TokenKind::Ident);
        let handler = handler_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        self.record_use(handler);

        let clauses: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::HandlerClause)
            .map(|c| self.lower_handle_clause(c))
            .collect();
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
        let op = op_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        self.record_use(op);

        let names: Vec<_> = node
            .child_tokens()
            .filter(|t| t.kind() == TokenKind::Ident)
            .skip(1)
            .filter_map(|t| self.intern_ident_token(t))
            .collect();
        let (result, params) = names
            .split_first()
            .map_or((None, Vec::new()), |(r, rest)| (Some(*r), rest.to_vec()));
        self.push_scope();
        if let Some(result) = result {
            let _ = self.insert_binding(result, NameBindingKind::HandleClauseResult);
        }
        for p in &params {
            let _ = self.insert_binding(*p, NameBindingKind::HandleClauseParam);
        }
        let body = if let Some(n) = node.child_nodes().filter(|n| n.kind().is_expr()).last() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
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

    fn lower_resume_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let expr = node
            .child_nodes()
            .find(|n| n.kind().is_expr())
            .map(|n| self.lower_expr(n));
        self.alloc_expr(origin, HirExprKind::Resume { expr })
    }

    fn lower_quote_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let has_paren = node.child_tokens().any(|t| t.kind() == TokenKind::LParen);
        if has_paren {
            let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
                self.lower_expr(n)
            } else {
                self.alloc_expr(origin, HirExprKind::Error)
            };
            return self.alloc_expr(
                origin,
                HirExprKind::Quote {
                    kind: HirQuoteKind::Expr { expr },
                },
            );
        }
        self.push_scope();
        let mut exprs = Vec::new();
        for stmt in node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::SequenceExpr)
        {
            if let Some(inner) = stmt_wrapper_expr(stmt) {
                exprs.push(self.lower_stmt_expr(inner));
            }
        }
        self.pop_scope();
        let range = self.store.alloc_expr_list(exprs);
        self.alloc_expr(
            origin,
            HirExprKind::Quote {
                kind: HirQuoteKind::Block { exprs: range },
            },
        )
    }

    fn lower_splice_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        if node.child_tokens().any(|t| t.kind() == TokenKind::LBracket) {
            let exprs: Vec<_> = node
                .child_nodes()
                .filter(|n| n.kind().is_expr())
                .map(|n| self.lower_expr(n))
                .collect();
            let exprs = self.store.alloc_expr_list(exprs);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Exprs { exprs },
                },
            );
        }
        if node.child_tokens().any(|t| t.kind() == TokenKind::LParen) {
            let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
                self.lower_expr(n)
            } else {
                self.alloc_expr(origin, HirExprKind::Error)
            };
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Expr { expr },
                },
            );
        }
        if let Some(tok) = node.child_tokens().find(|t| t.kind() == TokenKind::Ident) {
            let Some(name) = self.intern_ident_token(tok) else {
                return self.alloc_expr(origin, HirExprKind::Error);
            };
            self.record_use(name);
            return self.alloc_expr(
                origin,
                HirExprKind::Splice {
                    kind: HirSpliceKind::Name { name },
                },
            );
        }
        self.alloc_expr(origin, HirExprKind::Error)
    }

    fn lower_attributed_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let attrs = self.lower_attrs(node);
        let expr = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
        self.alloc_expr(origin, HirExprKind::Attributed { attrs, expr })
    }

    fn lower_attrs(&mut self, node: SyntaxNode<'tree, 'src>) -> SliceRange<HirAttr> {
        let attrs: Vec<_> = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::Attr)
            .map(|a| self.lower_attr(a))
            .collect();
        self.store.attrs.alloc_from_iter(attrs)
    }

    fn lower_attr(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttr {
        let origin = self.origin_node(node);
        let mut path = Vec::new();
        for tok in node.child_tokens().filter(|t| t.kind() == TokenKind::Ident) {
            if let Some(ident) = self.intern_ident_token(tok) {
                path.push(ident);
            }
        }
        let path = self.store.idents.alloc_from_iter(path);
        let args = node
            .child_nodes()
            .filter(|n| n.kind() == SyntaxNodeKind::AttrArg)
            .map(|n| self.lower_attr_arg(n))
            .collect::<Vec<_>>();
        let args = self.store.attr_args.alloc_from_iter(args);
        HirAttr { origin, path, args }
    }

    fn lower_attr_arg(&mut self, node: SyntaxNode<'tree, 'src>) -> HirAttrArg {
        let mut tokens = node.child_tokens();
        let (name, value_node) = if node.child_tokens().any(|t| t.kind() == TokenKind::ColonEq) {
            let name = tokens
                .find(|t| t.kind() == TokenKind::Ident)
                .and_then(|t| self.intern_ident_token(t));
            let value_node = node.child_nodes().find(|n| n.kind().is_expr());
            (name, value_node)
        } else {
            (None, node.child_nodes().find(|n| n.kind().is_expr()))
        };
        let value = if let Some(n) = value_node {
            self.lower_expr(n)
        } else {
            self.alloc_expr(self.origin_node(node), HirExprKind::Error)
        };
        HirAttrArg { name, value }
    }

    fn lower_data_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let mut variants = Vec::new();
        let mut fields = Vec::new();
        let mut stack = vec![node];
        while let Some(n) = stack.pop() {
            for child in n.child_nodes() {
                match child.kind() {
                    SyntaxNodeKind::Variant => variants.push(self.lower_variant_def(child)),
                    SyntaxNodeKind::Field => fields.push(self.lower_field_def(child)),
                    _ => stack.push(child),
                }
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
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
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
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));
        let mut exprs = node.child_nodes().filter(|n| n.kind().is_expr());
        let ty = if let Some(n) = exprs.next() {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
        };
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

    fn lower_effect_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let members = self.lower_members(node);
        self.alloc_expr(origin, HirExprKind::Effect { members })
    }

    fn lower_class_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        let constraints = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |n| self.lower_constraint_list(n));
        let members = self.lower_members(node);
        self.alloc_expr(
            origin,
            HirExprKind::Class {
                constraints,
                members,
            },
        )
    }

    fn lower_instance_expr(&mut self, node: SyntaxNode<'tree, 'src>) -> music_hir::HirExprId {
        let origin = self.origin_node(node);
        self.push_scope();
        let type_params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::TypeParamList)
            .map_or(SliceRange::EMPTY, |n| self.lower_type_param_list(n));
        let constraints = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ConstraintList)
            .map_or(SliceRange::EMPTY, |n| self.lower_constraint_list(n));

        let class = if let Some(n) = node.child_nodes().find(|n| n.kind().is_expr()) {
            self.lower_expr(n)
        } else {
            self.alloc_expr(origin, HirExprKind::Error)
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
        let name = name_tok
            .and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| Ident::new(self.interner.intern("_"), node.span()));

        self.push_scope();
        let params = node
            .child_nodes()
            .find(|n| n.kind() == SyntaxNodeKind::ParamList)
            .map_or(SliceRange::EMPTY, |n| {
                self.lower_param_list(n, NameBindingKind::Param)
            });

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
}

fn stmt_wrapper_expr<'tree, 'src>(
    node: SyntaxNode<'tree, 'src>,
) -> Option<SyntaxNode<'tree, 'src>> {
    if node.kind() != SyntaxNodeKind::SequenceExpr {
        return None;
    }
    let children: Vec<_> = node.children().collect();
    if children.len() != 2 {
        return None;
    }
    match (children[0], children[1]) {
        (SyntaxElement::Node(expr), SyntaxElement::Token(tok))
            if tok.kind() == TokenKind::Semicolon =>
        {
            expr.kind().is_expr().then_some(expr)
        }
        _ => None,
    }
}

fn parse_u32_lit(raw: &str) -> Option<u32> {
    let raw = raw.replace('_', "");
    let (radix, digits) = raw
        .strip_prefix("0x")
        .or_else(|| raw.strip_prefix("0X"))
        .map_or_else(
            || {
                raw.strip_prefix("0o")
                    .or_else(|| raw.strip_prefix("0O"))
                    .map_or_else(
                        || {
                            raw.strip_prefix("0b")
                                .or_else(|| raw.strip_prefix("0B"))
                                .map_or((10, raw.as_str()), |rest| (2, rest))
                        },
                        |rest| (8, rest),
                    )
            },
            |rest| (16, rest),
        );
    u32::from_str_radix(digits, radix).ok()
}
