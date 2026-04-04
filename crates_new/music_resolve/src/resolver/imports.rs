use super::*;

use music_module::{ImportErrorKind, ImportSiteKind, collect_import_sites};

impl Resolver<'_, '_, '_, '_> {
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
                        let label = match err.kind {
                            ImportErrorKind::NotFound => "module not found",
                            ImportErrorKind::InvalidSpecifier => "invalid module specifier",
                        };
                        self.diags
                            .push(Diag::error("import resolve failed").with_label(
                                site.span,
                                self.source_id,
                                label,
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
