use super::*;

impl ResolveDb {
    pub(super) fn resolve_pat(&mut self, pat_id: PatId, scope: ScopeId) {
        let kind = self.db.ast.pats.get(pat_id).kind.clone();
        match kind {
            PatKind::Bind(ident) => {
                let _ = self.define_value(&ident, scope);
            }
            PatKind::As { name, pat } => {
                let _ = self.define_value(&name, scope);
                self.resolve_pat(pat, scope);
            }
            PatKind::Tuple(pats) | PatKind::Array(pats) | PatKind::Or(pats) => {
                for p in pats {
                    self.resolve_pat(p, scope);
                }
            }
            PatKind::Variant { tag, fields } => {
                if let Some(def_id) = self.resolution.scopes.resolve(scope, tag.name) {
                    let _ = self.resolution.pat_variant_res.insert(pat_id, def_id);
                }
                for p in fields {
                    self.resolve_pat(p, scope);
                }
            }
            PatKind::Record(fields) => {
                for f in fields {
                    if let Some(p) = f.pat {
                        self.resolve_pat(p, scope);
                    } else {
                        let _ = self.define_value(&f.name, scope);
                    }
                }
            }
            PatKind::Wildcard | PatKind::Lit(_) => {}
        }
    }
}
