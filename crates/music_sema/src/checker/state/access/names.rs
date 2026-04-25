use music_names::{Ident, NameBindingId, NameSite};

use crate::checker::state::PassBase;

impl PassBase<'_, '_, '_> {
    pub fn binding_id_for_decl(&self, ident: Ident) -> Option<NameBindingId> {
        let site = NameSite::new(self.source_id(), ident.span);
        if let Some(id) = self.module.binding_ids.get(&site).copied() {
            return Some(id);
        }
        // Some decl sites in resolve are recorded on the enclosing syntactic form rather than the
        // identifier token span. Fall back to a name+containment lookup so sema can still attach
        // binding facts (notably for params) to the correct resolved binding.
        self.module
            .resolved
            .names
            .bindings
            .iter()
            .filter_map(|(id, binding)| {
                if binding.name != ident.name || binding.site.source_id != site.source_id {
                    return None;
                }
                let overlaps = binding.site.span.start <= ident.span.end
                    && binding.site.span.end >= ident.span.start;
                if overlaps {
                    Some((id, binding.site.span.len()))
                } else {
                    None
                }
            })
            .min_by_key(|(_, span_len)| *span_len)
            .map(|(id, _)| id)
    }

    pub fn binding_id_for_use(&self, ident: Ident) -> Option<NameBindingId> {
        self.module
            .resolved
            .names
            .refs
            .get(&NameSite::new(self.source_id(), ident.span))
            .copied()
    }
}
