use super::*;

use crate::diag::ResolveDiagKind;
use music_base::diag::DiagContext;
use music_module::{ImportErrorKind, ImportSiteKind, collect_import_sites};

impl Resolver<'_, '_, '_, '_> {
    pub(super) fn discover_imports(&mut self, module_key: &ModuleKey) -> ResolvedImportList {
        let sites = collect_import_sites(self.source_id, self.tree);
        let mut imports = Vec::new();
        for site in sites {
            match site.kind {
                ImportSiteKind::Static { spec } => {
                    let Some(env) = self.import_env else {
                        continue;
                    };
                    match env.resolve(module_key, &spec) {
                        Ok(to) => {
                            let _prev = self.import_targets.insert(site.span, to.clone());
                            imports.push(ResolvedImport {
                                span: site.span,
                                spec,
                                to,
                            });
                        }
                        Err(err) => {
                            let label = match err.kind {
                                ImportErrorKind::ModuleNotFound => "module not found",
                                ImportErrorKind::SpecifierInvalid => "invalid module specifier",
                            };
                            self.diags.push(resolve_diag(
                                self.source_id,
                                site.span,
                                ResolveDiagKind::ImportResolveFailed,
                                DiagContext::new().with("spec", spec).with("reason", label),
                            ));
                        }
                    }
                }
                ImportSiteKind::InvalidStringLit | ImportSiteKind::NonLiteral => {
                    self.diags.push(resolve_diag(
                        self.source_id,
                        site.span,
                        ResolveDiagKind::InvalidImportSpec,
                        DiagContext::new(),
                    ));
                }
            }
        }
        imports
    }
}
