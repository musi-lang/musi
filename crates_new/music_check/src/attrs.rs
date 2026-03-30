use music_hir::{HirArrayItem, HirAttr, HirAttrArgKind, HirExprKind, HirLitKind};

use crate::SemaErrorKind;

use super::checker::Checker;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum AttrValueKind {
    StringLit,
    StringOrStringArray,
    IntLit,
}

#[derive(Debug, Copy, Clone)]
struct AttrSlot {
    key: &'static str,
    value: AttrValueKind,
    repeatable: bool,
}

#[derive(Debug, Copy, Clone)]
struct AttrSpec {
    attr: &'static str,
    positional: &'static [AttrSlot],
    named: &'static [AttrSlot],
    extra_positional: Option<AttrSlot>,
    required_all: &'static [&'static str],
    required_any: &'static [&'static str],
}

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
                self.validate_link_attr(&attr);
                continue;
            }

            if is_path(&attr, &["repr"], self.ctx.interner) {
                self.validate_repr_attr(&attr);
                continue;
            }

            if is_path(&attr, &["layout"], self.ctx.interner) {
                self.validate_layout_attr(&attr);
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

    fn validate_link_attr(&mut self, attr: &HirAttr) {
        self.validate_attr_args(
            attr,
            AttrSpec {
                attr: "link",
                positional: &[
                    AttrSlot {
                        key: "name",
                        value: AttrValueKind::StringLit,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "symbol",
                        value: AttrValueKind::StringLit,
                        repeatable: false,
                    },
                ],
                named: &[
                    AttrSlot {
                        key: "name",
                        value: AttrValueKind::StringLit,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "symbol",
                        value: AttrValueKind::StringLit,
                        repeatable: false,
                    },
                ],
                extra_positional: None,
                required_all: &["name"],
                required_any: &[],
            },
        );
    }

    fn validate_repr_attr(&mut self, attr: &HirAttr) {
        self.validate_attr_args(
            attr,
            AttrSpec {
                attr: "repr",
                positional: &[AttrSlot {
                    key: "kind",
                    value: AttrValueKind::StringLit,
                    repeatable: false,
                }],
                named: &[AttrSlot {
                    key: "kind",
                    value: AttrValueKind::StringLit,
                    repeatable: false,
                }],
                extra_positional: None,
                required_all: &["kind"],
                required_any: &[],
            },
        );
    }

    fn validate_layout_attr(&mut self, attr: &HirAttr) {
        self.validate_attr_args(
            attr,
            AttrSpec {
                attr: "layout",
                positional: &[
                    AttrSlot {
                        key: "align",
                        value: AttrValueKind::IntLit,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "pack",
                        value: AttrValueKind::IntLit,
                        repeatable: false,
                    },
                ],
                named: &[
                    AttrSlot {
                        key: "align",
                        value: AttrValueKind::IntLit,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "pack",
                        value: AttrValueKind::IntLit,
                        repeatable: false,
                    },
                ],
                extra_positional: None,
                required_all: &[],
                required_any: &["align", "pack"],
            },
        );
    }

    fn validate_when_attr(&mut self, attr: &HirAttr) {
        self.validate_attr_args(
            attr,
            AttrSpec {
                attr: "when",
                positional: &[
                    AttrSlot {
                        key: "os",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "arch",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "env",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "abi",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "vendor",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                ],
                named: &[
                    AttrSlot {
                        key: "os",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "arch",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "env",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "abi",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "vendor",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: false,
                    },
                    AttrSlot {
                        key: "feature",
                        value: AttrValueKind::StringOrStringArray,
                        repeatable: true,
                    },
                ],
                extra_positional: Some(AttrSlot {
                    key: "feature",
                    value: AttrValueKind::StringOrStringArray,
                    repeatable: true,
                }),
                required_all: &[],
                required_any: &[],
            },
        );
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

    fn validate_attr_args(&mut self, attr: &HirAttr, spec: AttrSpec) {
        if attr.args.is_empty() {
            self.error(
                attr.origin.span,
                SemaErrorKind::AttrArgsRequired {
                    attr: spec.attr.to_string(),
                },
            );
            return;
        }

        let mut seen: Vec<(&'static str, u32)> = Vec::new();
        let mut positional_index = 0usize;

        for arg in attr.args.iter() {
            match arg.kind {
                HirAttrArgKind::Positional { value } => {
                    let slot = spec
                        .positional
                        .get(positional_index)
                        .copied()
                        .or(spec.extra_positional);
                    positional_index += 1;

                    let Some(slot) = slot else {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrArgCountInvalid {
                                attr: spec.attr.to_string(),
                                expected: u32::try_from(spec.positional.len()).unwrap_or(0),
                                found: u32::try_from(positional_index).unwrap_or(0),
                            },
                        );
                        continue;
                    };

                    if self.bump_seen(&mut seen, slot) {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrDuplicateArg {
                                attr: spec.attr.to_string(),
                                name: slot.key.to_string(),
                            },
                        );
                        continue;
                    }

                    if !self.is_lit_kind(value, slot.value) {
                        self.error(
                            arg.origin.span,
                            match slot.value {
                                AttrValueKind::StringLit | AttrValueKind::StringOrStringArray => {
                                    SemaErrorKind::AttrArgStringRequired {
                                        attr: spec.attr.to_string(),
                                        name: slot.key.to_string(),
                                    }
                                }
                                AttrValueKind::IntLit => SemaErrorKind::AttrArgIntRequired {
                                    attr: spec.attr.to_string(),
                                    name: slot.key.to_string(),
                                },
                            },
                        );
                    }
                }
                HirAttrArgKind::Named { name, value } => {
                    let key = self.ctx.interner.resolve(name.name);
                    let Some(slot) = spec.named.iter().copied().find(|s| s.key == key) else {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrUnknownArg {
                                attr: spec.attr.to_string(),
                                name: key.to_string(),
                            },
                        );
                        continue;
                    };

                    if self.bump_seen(&mut seen, slot) {
                        self.error(
                            arg.origin.span,
                            SemaErrorKind::AttrDuplicateArg {
                                attr: spec.attr.to_string(),
                                name: key.to_string(),
                            },
                        );
                        continue;
                    }

                    if !self.is_lit_kind(value, slot.value) {
                        self.error(
                            arg.origin.span,
                            match slot.value {
                                AttrValueKind::StringLit | AttrValueKind::StringOrStringArray => {
                                    SemaErrorKind::AttrArgStringRequired {
                                        attr: spec.attr.to_string(),
                                        name: key.to_string(),
                                    }
                                }
                                AttrValueKind::IntLit => SemaErrorKind::AttrArgIntRequired {
                                    attr: spec.attr.to_string(),
                                    name: key.to_string(),
                                },
                            },
                        );
                    }
                }
            }
        }

        for key in spec.required_all {
            if !seen.iter().any(|(k, c)| *k == *key && *c > 0) {
                self.error(
                    attr.origin.span,
                    SemaErrorKind::AttrArgRequired {
                        attr: spec.attr.to_string(),
                        name: (*key).to_string(),
                    },
                );
            }
        }

        if !spec.required_any.is_empty()
            && !spec
                .required_any
                .iter()
                .any(|key| seen.iter().any(|(k, c)| *k == *key && *c > 0))
        {
            let name = spec.required_any[0];
            self.error(
                attr.origin.span,
                SemaErrorKind::AttrArgRequired {
                    attr: spec.attr.to_string(),
                    name: name.to_string(),
                },
            );
        }
    }

    fn bump_seen(&self, seen: &mut Vec<(&'static str, u32)>, slot: AttrSlot) -> bool {
        let entry = seen.iter_mut().find(|(key, _)| *key == slot.key);
        let count = match entry {
            Some((_, c)) => c,
            None => {
                seen.push((slot.key, 0));
                &mut seen.last_mut().unwrap().1
            }
        };

        *count = count.saturating_add(1);
        !slot.repeatable && *count > 1
    }

    fn is_lit_kind(&self, expr: music_hir::HirExprId, expected: AttrValueKind) -> bool {
        match expected {
            AttrValueKind::StringLit => matches!(
                self.ctx.store.exprs.get(expr).kind,
                HirExprKind::Lit {
                    lit: music_hir::HirLit {
                        kind: HirLitKind::String(_),
                    }
                }
            ),
            AttrValueKind::StringOrStringArray => {
                if self.is_lit_kind(expr, AttrValueKind::StringLit) {
                    return true;
                }

                let HirExprKind::Array { items } = &self.ctx.store.exprs.get(expr).kind else {
                    return false;
                };

                items.iter().all(|item| match *item {
                    HirArrayItem::Expr(expr) => self.is_lit_kind(expr, AttrValueKind::StringLit),
                    HirArrayItem::Spread { .. } => false,
                })
            }
            AttrValueKind::IntLit => matches!(
                self.ctx.store.exprs.get(expr).kind,
                HirExprKind::Lit {
                    lit: music_hir::HirLit {
                        kind: HirLitKind::Int { .. },
                    }
                }
            ),
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
