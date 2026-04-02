use super::*;

use music_module::{ImportSiteKind, collect_import_sites};

impl<'a, 'env, 'tree, 'src> Resolver<'a, 'env, 'tree, 'src> {
    pub(super) fn discover_imports(&mut self, module_key: &ModuleKey) -> ResolvedImportList {
        let Some(env) = self.import_env else {
            return Vec::new();
        };

        let sites = collect_import_sites(self.source_id, self.tree);
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
                                format!("{}", err),
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
}
