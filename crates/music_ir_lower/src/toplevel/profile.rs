use super::{HirAttr, HirExprKind, Interner, ProfileAttrs, SemaModule, SliceRange};

pub(super) fn profile_attrs(
    sema: &SemaModule,
    interner: &Interner,
    attrs: SliceRange<HirAttr>,
) -> ProfileAttrs {
    let mut profile = ProfileAttrs::default();
    for attr in sema.module().store.attrs.get(attrs) {
        let parts = sema.module().store.idents.get(attr.path);
        if parts.len() != 1 || interner.resolve(parts[0].name) != "profile" {
            continue;
        }
        for arg in sema.module().store.attr_args.get(attr.args.clone()) {
            let Some(name) = arg.name else {
                continue;
            };
            if interner.resolve(name.name) != "level" {
                continue;
            }
            let HirExprKind::Variant { tag, args } = &sema.module().store.exprs.get(arg.value).kind
            else {
                continue;
            };
            if !sema.module().store.args.get(args.clone()).is_empty() {
                continue;
            }
            match interner.resolve(tag.name) {
                "hot" => profile.hot = true,
                "cold" => profile.cold = true,
                _ => {}
            }
        }
    }
    profile
}
