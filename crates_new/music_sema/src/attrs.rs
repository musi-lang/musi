use music_hir::{HirAttr, HirAttrArgKind};

use crate::SemaErrorKind;

use super::check::Checker;

impl<'a> Checker<'a> {
    pub(super) fn validate_public_attrs(&mut self) {
        let attrs: Vec<HirAttr> = self
            .ctx
            .store
            .attrs
            .iter()
            .map(|(_, attr)| attr.clone())
            .collect();

        for attr in attrs {
            // `@musi.lang` is validated by the lang-item path.
            if is_path(&attr, &["musi", "lang"], self.ctx.interner) {
                continue;
            }

            if is_path(&attr, &["link"], self.ctx.interner) {
                self.validate_named_positional(&attr, "link", &["name", "symbol"]);
                continue;
            }

            if is_path(&attr, &["repr"], self.ctx.interner) {
                self.validate_named_positional(&attr, "repr", &["kind"]);
                continue;
            }

            if is_path(&attr, &["layout"], self.ctx.interner) {
                self.validate_named_positional(&attr, "layout", &["align", "pack"]);
                continue;
            }

            if is_path(&attr, &["when"], self.ctx.interner) {
                self.validate_when_attr(&attr);
                continue;
            }

            if is_path_prefix(&attr, &["diag"], self.ctx.interner) {
                self.validate_diag_attr(&attr);
                continue;
            }
        }
    }

    fn validate_named_positional(&mut self, attr: &HirAttr, name: &str, params: &[&str]) {
        if attr.args.is_empty() {
            self.error(
                attr.origin.span,
                SemaErrorKind::AttrArgsRequired {
                    attr: attr_name(attr, self.ctx.interner),
                },
            );
            return;
        }

        let mut used = vec![false; params.len()];
        let mut positional_index = 0usize;

        for arg in attr.args.iter() {
            match arg.kind {
                HirAttrArgKind::Positional { .. } => {
                    if positional_index >= params.len() {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrArgCountInvalid {
                                attr: String::from(name),
                                expected: u32::try_from(params.len()).unwrap_or(0),
                                found: u32::try_from(positional_index + 1).unwrap_or(0),
                            },
                        );
                        positional_index += 1;
                        continue;
                    }

                    let key = params[positional_index];
                    if used[positional_index] {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrDuplicateArg {
                                attr: String::from(name),
                                name: String::from(key),
                            },
                        );
                    } else {
                        used[positional_index] = true;
                    }
                    positional_index += 1;
                }
                HirAttrArgKind::Named { name: ident, .. } => {
                    let key = self.ctx.interner.resolve(ident.name).to_string();
                    let Some(index) = params.iter().position(|p| *p == key) else {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrUnknownArg {
                                attr: String::from(name),
                                name: key,
                            },
                        );
                        continue;
                    };
                    if used[index] {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrDuplicateArg {
                                attr: String::from(name),
                                name: params[index].to_string(),
                            },
                        );
                    } else {
                        used[index] = true;
                    }
                }
            }
        }
    }

    fn validate_when_attr(&mut self, attr: &HirAttr) {
        if attr.args.is_empty() {
            self.error(
                attr.origin.span,
                SemaErrorKind::AttrArgsRequired {
                    attr: String::from("when"),
                },
            );
            return;
        }

        let allowed = ["os", "arch", "env", "abi", "vendor", "feature"];
        let mut used = [false; 5];

        for arg in attr.args.iter() {
            let HirAttrArgKind::Named { name: ident, .. } = arg.kind else {
                // Positional args map to `os, arch, env, abi, vendor, feature...`.
                continue;
            };

            let key = self.ctx.interner.resolve(ident.name).to_string();
            if !allowed.iter().copied().any(|k| k == key) {
                self.error(
                    arg.origin.span,
                    SemaErrorKind::AttrUnknownArg {
                        attr: String::from("when"),
                        name: key,
                    },
                );
                continue;
            }

            if key == "feature" {
                continue;
            }

            let index = match key.as_str() {
                "os" => 0,
                "arch" => 1,
                "env" => 2,
                "abi" => 3,
                "vendor" => 4,
                _ => continue,
            };

            if used[index] {
                self.error(
                    arg.origin.span,
                    SemaErrorKind::AttrDuplicateArg {
                        attr: String::from("when"),
                        name: key,
                    },
                );
            } else {
                used[index] = true;
            }
        }
    }

    fn validate_diag_attr(&mut self, attr: &HirAttr) {
        if attr.args.is_empty() {
            self.error(
                attr.origin.span,
                SemaErrorKind::AttrArgsRequired {
                    attr: attr_name(attr, self.ctx.interner),
                },
            );
            return;
        }

        for arg in attr.args.iter() {
            if matches!(arg.kind, HirAttrArgKind::Named { .. }) {
                self.error(
                    arg.origin.span,
                    SemaErrorKind::AttrNamedArgsNotAllowed {
                        attr: attr_name(attr, self.ctx.interner),
                    },
                );
            }
        }
    }
}

fn is_path(attr: &HirAttr, segments: &[&str], interner: &music_names::Interner) -> bool {
    if attr.path.segments.len() != segments.len() {
        return false;
    }
    attr.path
        .segments
        .iter()
        .zip(segments.iter().copied())
        .all(|(seg, expected)| interner.resolve(seg.name) == expected)
}

fn is_path_prefix(attr: &HirAttr, segments: &[&str], interner: &music_names::Interner) -> bool {
    if attr.path.segments.len() < segments.len() {
        return false;
    }
    attr.path
        .segments
        .iter()
        .take(segments.len())
        .zip(segments.iter().copied())
        .all(|(seg, expected)| interner.resolve(seg.name) == expected)
}

fn attr_name(attr: &HirAttr, interner: &music_names::Interner) -> String {
    let mut out = String::new();
    for (i, seg) in attr.path.segments.iter().enumerate() {
        if i > 0 {
            out.push('.');
        }
        out.push_str(interner.resolve(seg.name));
    }
    out
}
