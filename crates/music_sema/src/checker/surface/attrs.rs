use music_hir::{HirAttr, HirExprId, HirExprKind, HirLitKind};
use music_names::Interner;

use crate::api::{Attr, AttrArg, AttrRecordField, AttrValue};
use crate::checker::ModuleState;

fn attr_is_musi(path: &[Box<str>]) -> bool {
    matches!(path, [head, tail] if head.as_ref() == "musi" && matches!(tail.as_ref(), "builtin" | "intrinsic"))
        || path.first().is_some_and(|seg| seg.as_ref() == "musi")
}

fn attr_is_reserved(path: &[Box<str>]) -> bool {
    match path {
        [head, tail]
            if head.as_ref() == "musi" && matches!(tail.as_ref(), "builtin" | "intrinsic") =>
        {
            true
        }
        [head] if head.as_ref() == "native" => true,
        [head] if head.as_ref() == "target" => true,
        [head] if head.as_ref() == "profile" => true,
        [head] if head.as_ref() == "lifecycle" => true,
        [head] if head.as_ref() == "repr" => true,
        [head] if head.as_ref() == "layout" => true,
        [head] if head.as_ref() == "frozen" => true,
        [head, ..] if head.as_ref() == "diag" => true,
        [head, ..] if head.as_ref() == "musi" => true,
        _ => false,
    }
}

fn lower_attr_value(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
) -> Option<AttrValue> {
    match module.resolved.module.store.exprs.get(expr_id).kind.clone() {
        HirExprKind::Lit { lit } => match module.resolved.module.store.lits.get(lit).kind.clone() {
            HirLitKind::String { value } => Some(AttrValue::String(value)),
            HirLitKind::Int { raw } => Some(AttrValue::Int(raw)),
            HirLitKind::Rune { value } => Some(AttrValue::Rune(value)),
            HirLitKind::Float { .. } => None,
        },
        HirExprKind::Variant { tag, args } => {
            let tag = interner.resolve(tag.name).into();
            let mut lowered = Vec::new();
            for arg in module.resolved.module.store.args.get(args) {
                lowered.push(lower_attr_value(module, interner, arg.expr)?);
            }
            Some(AttrValue::Variant {
                tag,
                args: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Array { items } => {
            let mut lowered = Vec::new();
            for item in module.resolved.module.store.array_items.get(items) {
                if item.spread {
                    return None;
                }
                lowered.push(lower_attr_value(module, interner, item.expr)?);
            }
            Some(AttrValue::Array {
                items: lowered.into_boxed_slice(),
            })
        }
        HirExprKind::Record { items } => {
            let mut fields = Vec::new();
            for item in module.resolved.module.store.record_items.get(items) {
                if item.spread {
                    return None;
                }
                let name = item.name?;
                let attr_value = lower_attr_value(module, interner, item.value)?;
                fields.push(AttrRecordField {
                    name: interner.resolve(name.name).into(),
                    value: attr_value,
                });
            }
            Some(AttrValue::Record {
                fields: fields.into_boxed_slice(),
            })
        }
        _ => None,
    }
}

fn lower_attr(module: &ModuleState, interner: &Interner, attr: &HirAttr) -> Option<Attr> {
    let path = module
        .resolved
        .module
        .store
        .idents
        .get(attr.path)
        .iter()
        .map(|ident| interner.resolve(ident.name).into())
        .collect::<Vec<Box<str>>>()
        .into_boxed_slice();
    let mut args = Vec::<AttrArg>::new();
    for arg in module
        .resolved
        .module
        .store
        .attr_args
        .get(attr.args.clone())
    {
        let name = arg.name.map(|ident| interner.resolve(ident.name).into());
        let attr_value = lower_attr_value(module, interner, arg.value)?;
        args.push(AttrArg {
            name,
            value: attr_value,
        });
    }
    Some(Attr {
        path,
        args: args.into_boxed_slice(),
    })
}

pub(super) fn split_export_attrs(
    module: &ModuleState,
    interner: &Interner,
    attrs: &[HirAttr],
) -> (Box<[Attr]>, Box<[Attr]>) {
    let mut inert = Vec::new();
    let mut musi = Vec::new();
    for attr in attrs {
        let Some(lowered) = lower_attr(module, interner, attr) else {
            continue;
        };
        if attr_is_reserved(&lowered.path) {
            continue;
        }
        if attr_is_musi(&lowered.path) {
            musi.push(lowered);
        } else {
            inert.push(lowered);
        }
    }
    (inert.into_boxed_slice(), musi.into_boxed_slice())
}
