use music_module::ModuleKey;
use music_sema::{Attr, AttrValue, SemaModule};

use crate::api::IrMetaRecord;

fn qualified_name(module: &ModuleKey, name: &str) -> Box<str> {
    format!("{}::{name}", module.as_str()).into_boxed_str()
}

fn escape_string(text: &str) -> String {
    let mut out = String::new();
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out
}

fn format_attr_value(value: &AttrValue) -> String {
    match value {
        AttrValue::String(text) => format!("\"{}\"", escape_string(text)),
        AttrValue::Int(raw) => raw.to_string(),
        AttrValue::Rune(value) => value.to_string(),
        AttrValue::Variant { tag, args } => {
            if args.is_empty() {
                format!(".{tag}")
            } else {
                let inner = args
                    .iter()
                    .map(format_attr_value)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(".{tag}({inner})")
            }
        }
        AttrValue::Array { items } => {
            let inner = items
                .iter()
                .map(format_attr_value)
                .collect::<Vec<_>>()
                .join(", ");
            format!("[{inner}]")
        }
        AttrValue::Record { fields } => {
            let inner = fields
                .iter()
                .map(|field| format!("{} := {}", field.name, format_attr_value(&field.value)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {inner} }}")
        }
    }
}

fn format_attr(attr: &Attr) -> String {
    let path = attr
        .path
        .iter()
        .map(AsRef::as_ref)
        .collect::<Vec<_>>()
        .join(".");
    if attr.args.is_empty() {
        return format!("@{path}");
    }
    let args = attr
        .args
        .iter()
        .map(|arg| {
            arg.name.as_deref().map_or_else(
                || format_attr_value(&arg.value),
                |name| format!("{name} := {}", format_attr_value(&arg.value)),
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("@{path}({args})")
}

pub(super) fn collect_meta(sema: &SemaModule) -> Box<[IrMetaRecord]> {
    let mut out = Vec::<IrMetaRecord>::new();
    let surface = sema.surface();

    for class in &surface.exported_classes {
        if !class.laws.is_empty() {
            out.push(IrMetaRecord {
                target: qualified_name(&class.key.module, class.key.name.as_ref()),
                key: "class.laws".into(),
                values: class.laws.to_vec().into_boxed_slice(),
            });
        }
        for attr in &class.inert_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&class.key.module, class.key.name.as_ref()),
                key: "inert.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
        for attr in &class.musi_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&class.key.module, class.key.name.as_ref()),
                key: "musi.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
    }

    for effect in &surface.exported_effects {
        if !effect.laws.is_empty() {
            out.push(IrMetaRecord {
                target: qualified_name(&effect.key.module, effect.key.name.as_ref()),
                key: "effect.laws".into(),
                values: effect.laws.to_vec().into_boxed_slice(),
            });
        }
        for attr in &effect.inert_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&effect.key.module, effect.key.name.as_ref()),
                key: "inert.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
        for attr in &effect.musi_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&effect.key.module, effect.key.name.as_ref()),
                key: "musi.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
    }

    for data in &surface.exported_data {
        for attr in &data.inert_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&data.key.module, data.key.name.as_ref()),
                key: "inert.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
        for attr in &data.musi_attrs {
            out.push(IrMetaRecord {
                target: qualified_name(&data.key.module, data.key.name.as_ref()),
                key: "musi.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
    }

    for export in &surface.exported_values {
        let target = qualified_name(&surface.module_key, export.name.as_ref());
        for attr in &export.inert_attrs {
            out.push(IrMetaRecord {
                target: target.clone(),
                key: "inert.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
        for attr in &export.musi_attrs {
            out.push(IrMetaRecord {
                target: target.clone(),
                key: "musi.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
    }

    for (idx, inst) in surface.exported_instances.iter().enumerate() {
        let target: Box<str> = format!("{}::instance::{idx}", surface.module_key.as_str()).into();
        for attr in &inst.inert_attrs {
            out.push(IrMetaRecord {
                target: target.clone(),
                key: "inert.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
        for attr in &inst.musi_attrs {
            out.push(IrMetaRecord {
                target: target.clone(),
                key: "musi.attr".into(),
                values: vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
            });
        }
    }

    out.into_boxed_slice()
}

