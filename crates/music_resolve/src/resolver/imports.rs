use super::*;

use crate::diag::ResolveDiagKind;
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
                        Ok(to) => imports.push(ResolvedImport {
                            span: site.span,
                            spec,
                            to,
                        }),
                        Err(err) => {
                            let label = match err.kind {
                                ImportErrorKind::ModuleNotFound => "module not found",
                                ImportErrorKind::SpecifierInvalid => "invalid module specifier",
                            };
                            self.diags.push(
                                Diag::error(format!("unresolved source import specifier `{spec}`"))
                                    .with_code(ResolveDiagKind::ImportResolveFailed.code())
                                    .with_label(
                                        site.span,
                                        self.source_id,
                                        format!("{label} `{spec}`"),
                                    ),
                            );
                        }
                    }
                }
                ImportSiteKind::InvalidStringLit => {
                    self.diags.push(
                        Diag::error("string literal import specifier expected")
                            .with_code(ResolveDiagKind::InvalidImportSpec.code())
                            .with_label(
                                site.span,
                                self.source_id,
                                "string literal import specifier expected here",
                            ),
                    );
                }
                ImportSiteKind::NonLiteral => {
                    self.diags.push(
                        Diag::error("string literal import specifier expected")
                            .with_code(ResolveDiagKind::InvalidImportSpec.code())
                            .with_label(
                                site.span,
                                self.source_id,
                                "import specifier must be string literal",
                            ),
                    );
                }
            }
        }
        imports
    }
}
