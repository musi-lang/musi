use music_basic::Span;
use music_hir::{
    HirArrayItem, HirAttr, HirAttrArgKind, HirExprId, HirExprKind, HirLit, HirLitKind,
};
use music_names::{Ident, Interner};

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

impl Checker<'_> {
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

        for arg in &attr.args {
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
                    attr: spec.attr.to_owned(),
                },
            );
            return;
        }

        let mut seen: Vec<(&'static str, u32)> = vec![];
        let mut positional_index = 0usize;

        for arg in &attr.args {
            match arg.kind {
                HirAttrArgKind::Positional { value } => {
                    let slot = spec
                        .positional
                        .get(positional_index)
                        .copied()
                        .or(spec.extra_positional);
                    positional_index += 1;

                    self.validate_positional_attr_arg(
                        arg.origin.span,
                        spec,
                        &mut seen,
                        slot,
                        positional_index,
                        value,
                    );
                }
                HirAttrArgKind::Named { name, value } => {
                    self.validate_named_attr_arg(arg.origin.span, spec, &mut seen, name, value);
                }
            }
        }

        self.validate_required_attr_args(attr.origin.span, spec, &seen);
    }

    fn validate_positional_attr_arg(
        &mut self,
        span: Span,
        spec: AttrSpec,
        seen: &mut Vec<(&'static str, u32)>,
        slot: Option<AttrSlot>,
        positional_index: usize,
        value: HirExprId,
    ) {
        let Some(slot) = slot else {
            self.error(
                span,
                SemaErrorKind::AttrArgCountInvalid {
                    attr: spec.attr.to_owned(),
                    expected: u32::try_from(spec.positional.len()).unwrap_or(0),
                    found: u32::try_from(positional_index).unwrap_or(0),
                },
            );
            return;
        };

        if Self::bump_seen(seen, slot) {
            self.error(
                span,
                SemaErrorKind::AttrDuplicateArg {
                    attr: spec.attr.to_owned(),
                    name: slot.key.to_owned(),
                },
            );
            return;
        }

        if self.is_lit_kind(value, slot.value) {
            return;
        }

        self.error(
            span,
            match slot.value {
                AttrValueKind::StringLit | AttrValueKind::StringOrStringArray => {
                    SemaErrorKind::AttrArgStringRequired {
                        attr: spec.attr.to_owned(),
                        name: slot.key.to_owned(),
                    }
                }
                AttrValueKind::IntLit => SemaErrorKind::AttrArgIntRequired {
                    attr: spec.attr.to_owned(),
                    name: slot.key.to_owned(),
                },
            },
        );
    }

    fn validate_named_attr_arg(
        &mut self,
        span: Span,
        spec: AttrSpec,
        seen: &mut Vec<(&'static str, u32)>,
        name: Ident,
        value: HirExprId,
    ) {
        let key = self.ctx.interner.resolve(name.name);
        let Some(slot) = spec.named.iter().copied().find(|s| s.key == key) else {
            self.error(
                span,
                SemaErrorKind::AttrUnknownArg {
                    attr: spec.attr.to_owned(),
                    name: key.to_owned(),
                },
            );
            return;
        };

        if Self::bump_seen(seen, slot) {
            self.error(
                span,
                SemaErrorKind::AttrDuplicateArg {
                    attr: spec.attr.to_owned(),
                    name: key.to_owned(),
                },
            );
            return;
        }

        if self.is_lit_kind(value, slot.value) {
            return;
        }

        self.error(
            span,
            match slot.value {
                AttrValueKind::StringLit | AttrValueKind::StringOrStringArray => {
                    SemaErrorKind::AttrArgStringRequired {
                        attr: spec.attr.to_owned(),
                        name: key.to_owned(),
                    }
                }
                AttrValueKind::IntLit => SemaErrorKind::AttrArgIntRequired {
                    attr: spec.attr.to_owned(),
                    name: key.to_owned(),
                },
            },
        );
    }

    fn validate_required_attr_args(
        &mut self,
        span: Span,
        spec: AttrSpec,
        seen: &[(&'static str, u32)],
    ) {
        for key in spec.required_all {
            if seen.iter().any(|(k, c)| *k == *key && *c > 0) {
                continue;
            }
            self.error(
                span,
                SemaErrorKind::AttrArgRequired {
                    attr: spec.attr.to_owned(),
                    name: (*key).to_owned(),
                },
            );
        }

        if spec.required_any.is_empty() {
            return;
        }
        if spec
            .required_any
            .iter()
            .any(|key| seen.iter().any(|(k, c)| *k == *key && *c > 0))
        {
            return;
        }

        self.error(
            span,
            SemaErrorKind::AttrArgRequired {
                attr: spec.attr.to_owned(),
                name: spec.required_any[0].to_owned(),
            },
        );
    }

    fn bump_seen(seen: &mut Vec<(&'static str, u32)>, slot: AttrSlot) -> bool {
        let entry = seen.iter_mut().find(|(key, _)| *key == slot.key);
        let count = if let Some((_, c)) = entry {
            c
        } else {
            let idx = seen.len();
            seen.push((slot.key, 0));
            &mut seen[idx].1
        };

        *count = count.saturating_add(1);
        !slot.repeatable && *count > 1
    }

    fn is_lit_kind(&self, expr: HirExprId, expected: AttrValueKind) -> bool {
        match expected {
            AttrValueKind::StringLit => matches!(
                self.ctx.store.exprs.get(expr).kind,
                HirExprKind::Lit {
                    lit: HirLit {
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
                    lit: HirLit {
                        kind: HirLitKind::Int { .. },
                    }
                }
            ),
        }
    }
}

fn is_path(attr: &HirAttr, segments: &[&str], interner: &Interner) -> bool {
    if attr.path.segments.len() != segments.len() {
        return false;
    }
    attr.path
        .segments
        .iter()
        .zip(segments.iter().copied())
        .all(|(seg, expected)| interner.resolve(seg.name) == expected)
}

fn is_path_prefix(attr: &HirAttr, segments: &[&str], interner: &Interner) -> bool {
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

fn attr_name(attr: &HirAttr, interner: &Interner) -> String {
    let mut out = String::new();
    for (i, seg) in attr.path.segments.iter().enumerate() {
        if i > 0 {
            out.push('.');
        }
        out.push_str(interner.resolve(seg.name));
    }
    out
}
