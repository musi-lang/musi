use super::*;

impl<'a, 'env, 'tree, 'src> Resolver<'a, 'env, 'tree, 'src> {
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
            self.diags.push(Diag::error("unbound name").with_label(
                ident.span,
                self.source_id,
                "name not found",
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
