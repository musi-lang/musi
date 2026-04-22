use super::*;
use crate::diag::ResolveDiagKind;
use music_base::diag::DiagContext;

impl<'tree, 'src> Resolver<'_, '_, 'tree, 'src>
where
    'tree: 'src,
{
    pub(super) fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub(super) fn pop_scope(&mut self) {
        let _ = self.scopes.pop();
        if self.scopes.is_empty() {
            self.scopes.push(Scope::default());
        }
    }

    pub(super) fn insert_binding(&mut self, ident: Ident, kind: NameBindingKind) -> NameBindingId {
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

    pub(super) fn alloc_binding_without_scope(
        &mut self,
        ident: Ident,
        kind: NameBindingKind,
    ) -> NameBindingId {
        self.names.alloc_binding(NameBinding {
            name: ident.name,
            site: NameSite::new(self.source_id, ident.span),
            kind,
        })
    }

    pub(super) fn insert_import_binding(
        &mut self,
        name: Symbol,
        span: Span,
        from: ModuleKey,
    ) -> NameBindingId {
        let binding = self.names.alloc_binding(NameBinding {
            name,
            site: NameSite::new(self.source_id, span),
            kind: NameBindingKind::Import,
        });
        if let Some(scope) = self.scopes.last_mut() {
            let _prev = scope.names.insert(name, binding);
        }
        self.import_bindings.push(ResolvedImportBinding {
            binding,
            from,
            name,
        });
        binding
    }

    pub(super) fn lookup(&self, sym: Symbol) -> Option<NameBindingId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.names.get(&sym).copied() {
                return Some(id);
            }
        }
        None
    }

    pub(super) fn record_use(&mut self, ident: Ident) {
        let site = NameSite::new(self.source_id, ident.span);
        let Some(binding) = self.lookup(ident.name) else {
            let name = self.interner.resolve(ident.name);
            self.diags.push(resolve_diag(
                self.source_id,
                ident.span,
                ResolveDiagKind::UnboundName,
                DiagContext::new().with("name", name),
            ));
            return;
        };
        self.names.record_ref(site, binding);
    }

    pub(super) fn intern_ident_token(&mut self, tok: SyntaxToken<'tree, 'src>) -> Option<Ident> {
        let raw = tok.text()?;
        Some(self.intern_ident_text(tok.kind(), raw, tok.span()))
    }
}
