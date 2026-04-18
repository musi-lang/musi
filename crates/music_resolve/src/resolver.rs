use std::collections::HashMap;
use std::marker::PhantomData;

use music_arena::SliceRange;
use music_base::diag::Diag;
use music_base::{SourceId, Span};
use music_hir::{
    HirAttr, HirExpr, HirExprId, HirExprKind, HirMods, HirModule, HirOrigin, HirStore,
};
use music_module::{
    ImportEnv, ModuleExportSummary, ModuleKey, ModuleSpecifier, collect_export_summary,
};
use music_names::{
    Ident, Interner, KnownSymbols, NameBinding, NameBindingId, NameBindingKind, NameResolution,
    NameSite, Symbol,
};
use music_syntax::{SyntaxNode, SyntaxToken, SyntaxTree, TokenKind, canonical_name_text};

use crate::ResolveDiagKind;

mod expr;
mod imports;
mod names;
mod pat;
mod stmt;
mod util;

pub type ResolvedImportList = Vec<ResolvedImport>;
pub type ResolveDiagList = Vec<Diag>;

#[must_use]
pub fn resolve_diag_kind(diag: &Diag) -> Option<ResolveDiagKind> {
    ResolveDiagKind::from_diag(diag)
}

#[derive(Default)]
pub struct ResolveOptions<'env> {
    pub inject_compiler_prelude: bool,
    pub prelude: Vec<Symbol>,
    pub import_env: Option<&'env dyn ImportEnv>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedImport {
    pub span: Span,
    pub spec: ModuleSpecifier,
    pub to: ModuleKey,
}

#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub module_key: ModuleKey,
    pub module: HirModule,
    pub imports: ResolvedImportList,
    pub export_summary: ModuleExportSummary,
    pub names: NameResolution,
    pub diags: ResolveDiagList,
}

#[must_use]
pub fn resolve_module(
    source_id: SourceId,
    module_key: &ModuleKey,
    tree: &SyntaxTree,
    interner: &mut Interner,
    options: ResolveOptions<'_>,
) -> ResolvedModule {
    let mut resolver = Resolver::new(source_id, tree, interner, options);
    let imports = resolver.discover_imports(module_key);
    let root = resolver.lower_source_file();
    let module = HirModule::new(source_id, resolver.store, root);
    let export_summary = collect_export_summary(source_id, tree);
    ResolvedModule {
        module_key: module_key.clone(),
        module,
        imports,
        export_summary,
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
    tree: &'tree SyntaxTree,
    interner: &'a mut Interner,
    import_env: Option<&'env dyn ImportEnv>,
    marker: PhantomData<&'src str>,

    store: HirStore,

    names: NameResolution,
    diags: ResolveDiagList,
    scopes: Vec<Scope>,
}

impl<'a, 'env, 'tree, 'src> Resolver<'a, 'env, 'tree, 'src>
where
    'tree: 'src,
{
    fn new(
        source_id: SourceId,
        tree: &'tree SyntaxTree,
        interner: &'a mut Interner,
        options: ResolveOptions<'env>,
    ) -> Self {
        let store = HirStore::new();

        let mut names = NameResolution::new();
        let mut root = Scope::default();

        let known = KnownSymbols::new(interner);
        if options.inject_compiler_prelude {
            for sym in known.compiler_prelude() {
                let binding = names.alloc_binding(NameBinding {
                    name: sym,
                    site: NameSite::new(source_id, Span::DUMMY),
                    kind: NameBindingKind::Prelude,
                });
                let _prev = root.names.insert(sym, binding);
            }
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
            marker: PhantomData,
            store,
            names,
            diags: Vec::new(),
            scopes: vec![root],
        }
    }

    fn alloc_expr(&mut self, origin: HirOrigin, kind: HirExprKind) -> HirExprId {
        self.store.alloc_expr(HirExpr::new(origin, kind))
    }

    fn error_expr(&mut self, origin: HirOrigin) -> HirExprId {
        self.alloc_expr(origin, HirExprKind::Error)
    }

    fn merge_attrs(
        &mut self,
        first: SliceRange<HirAttr>,
        second: SliceRange<HirAttr>,
    ) -> SliceRange<HirAttr> {
        if first.is_empty() {
            return second;
        }
        if second.is_empty() {
            return first;
        }
        let mut out = Vec::with_capacity(usize::try_from(first.len() + second.len()).unwrap_or(0));
        out.extend(self.store.attrs.get(first).iter().cloned());
        out.extend(self.store.attrs.get(second).iter().cloned());
        self.store.attrs.alloc_from_iter(out)
    }

    fn apply_mods(&mut self, expr: HirExprId, mods: HirMods) {
        if mods.is_empty() {
            return;
        }
        let current = self.store.exprs.get(expr).mods.clone();
        let attrs = self.merge_attrs(current.attrs, mods.attrs);
        let export = current.export.or(mods.export);
        let foreign = current.foreign.or(mods.foreign);
        let partial = current.partial || mods.partial;
        self.store.exprs.get_mut(expr).mods = HirMods::new(attrs, export, foreign, partial);
    }

    fn lower_opt_expr(
        &mut self,
        origin: HirOrigin,
        node: Option<SyntaxNode<'tree, 'src>>,
    ) -> HirExprId {
        match node {
            Some(node) => self.lower_expr(node),
            None => self.error_expr(origin),
        }
    }

    fn origin_node(&self, node: SyntaxNode<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, node.span())
    }

    fn origin_token(&self, tok: SyntaxToken<'tree, 'src>) -> HirOrigin {
        HirOrigin::new(self.source_id, tok.span())
    }

    fn intern_ident_text(&mut self, token_kind: TokenKind, raw: &str, span: Span) -> Ident {
        let canon = canonical_name_text(token_kind, raw);
        let sym = self.interner.intern(canon);
        Ident::new(sym, span)
    }

    const fn is_ident_token_kind(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Ident | TokenKind::KwAny | TokenKind::KwSome
        )
    }

    const fn is_name_token_kind(kind: TokenKind) -> bool {
        matches!(
            kind,
            TokenKind::Ident | TokenKind::OpIdent | TokenKind::KwAny | TokenKind::KwSome
        )
    }

    fn placeholder_ident(&mut self, span: Span) -> Ident {
        Ident::new(self.interner.intern("_"), span)
    }

    fn intern_ident_token_or_placeholder(
        &mut self,
        tok: Option<SyntaxToken<'tree, 'src>>,
        span: Span,
    ) -> Ident {
        tok.and_then(|t| self.intern_ident_token(t))
            .unwrap_or_else(|| self.placeholder_ident(span))
    }
}
