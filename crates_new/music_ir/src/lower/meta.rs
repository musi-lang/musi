use music_module::ModuleKey;
use music_sema::{
    Attr, AttrValue, ConstraintKind, ExportedValue, ModuleSurface, SemaModule, SurfaceDim,
    SurfaceEffectRow, SurfaceTyId, SurfaceTyKind,
};

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
                .map(|field| format!("{} = {}", field.name, format_attr_value(&field.value)))
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
                |name| format!("{name} = {}", format_attr_value(&arg.value)),
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("@{path}({args})")
}

fn format_surface_ty(surface: &ModuleSurface, ty: SurfaceTyId) -> String {
    match &surface.ty(ty).kind {
        SurfaceTyKind::Error => "<error>".into(),
        SurfaceTyKind::Unknown => "Unknown".into(),
        SurfaceTyKind::Type => "Type".into(),
        SurfaceTyKind::Syntax => "Syntax".into(),
        SurfaceTyKind::Any => "Any".into(),
        SurfaceTyKind::Empty => "Empty".into(),
        SurfaceTyKind::Unit => "Unit".into(),
        SurfaceTyKind::Bool => "Bool".into(),
        SurfaceTyKind::Nat => "Nat".into(),
        SurfaceTyKind::Int => "Int".into(),
        SurfaceTyKind::Float => "Float".into(),
        SurfaceTyKind::String => "String".into(),
        SurfaceTyKind::CString => "CString".into(),
        SurfaceTyKind::CPtr => "CPtr".into(),
        SurfaceTyKind::Module => "Module".into(),
        SurfaceTyKind::NatLit(value) => value.to_string(),
        SurfaceTyKind::Named { name, args } => {
            if args.is_empty() {
                name.to_string()
            } else {
                let args = args
                    .iter()
                    .copied()
                    .map(|arg| format_surface_ty(surface, arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{name}[{args}]")
            }
        }
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let binder_ty = format_surface_ty(surface, *binder_ty);
            let body = format_surface_ty(surface, *body);
            let arrow = if *is_effectful { "~>" } else { "->" };
            format!("forall ({binder}: {binder_ty}) {arrow} {body}")
        }
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => {
            let params = params
                .iter()
                .copied()
                .map(|param| format_surface_ty(surface, param))
                .collect::<Vec<_>>()
                .join(", ");
            let ret = format_surface_ty(surface, *ret);
            let arrow = if *is_effectful { "~>" } else { "->" };
            format!("({params}) {arrow} {ret}")
        }
        SurfaceTyKind::Sum { left, right } => format!(
            "{} + {}",
            format_surface_ty(surface, *left),
            format_surface_ty(surface, *right)
        ),
        SurfaceTyKind::Tuple { items } => {
            let items = items
                .iter()
                .copied()
                .map(|item| format_surface_ty(surface, item))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({items})")
        }
        SurfaceTyKind::Array { dims, item } => {
            let item = format_surface_ty(surface, *item);
            let mut parts = vec![item];
            for dim in dims {
                parts.push(match dim {
                    SurfaceDim::Unknown => "_".into(),
                    SurfaceDim::Name(name) => name.to_string(),
                    SurfaceDim::Int(value) => value.to_string(),
                });
            }
            format!("Array[{}]", parts.join(", "))
        }
        SurfaceTyKind::Mut { inner } => format!("mut {}", format_surface_ty(surface, *inner)),
        SurfaceTyKind::Record { fields } => {
            let fields = fields
                .iter()
                .map(|field| format!("{}: {}", field.name, format_surface_ty(surface, field.ty)))
                .collect::<Vec<_>>()
                .join("; ");
            format!("{{ {fields} }}")
        }
    }
}

fn format_effect_row(surface: &ModuleSurface, row: &SurfaceEffectRow) -> String {
    let mut items = row
        .items
        .iter()
        .map(|item| {
            item.arg.map_or_else(
                || item.name.to_string(),
                |arg| format!("{}[{}]", item.name, format_surface_ty(surface, arg)),
            )
        })
        .collect::<Vec<_>>();
    if let Some(open) = row.open.as_deref() {
        items.push(format!("...{open}"));
    }
    format!("with {{ {} }}", items.join(", "))
}

fn push_meta(out: &mut Vec<IrMetaRecord>, target: &str, key: &'static str, values: Box<[Box<str>]>) {
    out.push(IrMetaRecord {
        target: target.to_owned().into_boxed_str(),
        key: key.into(),
        values,
    });
}

fn push_inert_and_musi_attrs(
    out: &mut Vec<IrMetaRecord>,
    target: &str,
    inert: &[Attr],
    musi: &[Attr],
) {
    for attr in inert {
        push_meta(
            out,
            target,
            "inert.attr",
            vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
        );
    }
    for attr in musi {
        push_meta(
            out,
            target,
            "musi.attr",
            vec![format_attr(attr).into_boxed_str()].into_boxed_slice(),
        );
    }
}

fn push_export_sig_meta(
    out: &mut Vec<IrMetaRecord>,
    surface: &ModuleSurface,
    target: &str,
    export: &ExportedValue,
) {
    push_meta(
        out,
        target,
        "value.ty",
        vec![format_surface_ty(surface, export.ty).into_boxed_str()].into_boxed_slice(),
    );
    if !export.type_params.is_empty() {
        push_meta(
            out,
            target,
            "value.type_params",
            export
                .type_params
                .iter()
                .map(|param| param.to_string().into_boxed_str())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
    }
    if !export.constraints.is_empty() {
        push_meta(
            out,
            target,
            "value.constraints",
            export
                .constraints
                .iter()
                .map(|constraint| {
                    let op = match constraint.kind {
                        ConstraintKind::Subtype => "<:",
                        ConstraintKind::Implements => ":",
                    };
                    format!(
                        "{} {op} {}",
                        constraint.name,
                        format_surface_ty(surface, constraint.value)
                    )
                    .into_boxed_str()
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        );
    }
    if !export.effects.items.is_empty() || export.effects.open.is_some() {
        push_meta(
            out,
            target,
            "value.effects",
            vec![format_effect_row(surface, &export.effects).into_boxed_str()].into_boxed_slice(),
        );
    }
}

pub(super) fn collect_meta(sema: &SemaModule) -> Box<[IrMetaRecord]> {
    let mut out = Vec::<IrMetaRecord>::new();
    let surface = sema.surface();

    for class in &surface.exported_classes {
        let target = qualified_name(&class.key.module, class.key.name.as_ref());
        if !class.laws.is_empty() {
            push_meta(&mut out, target.as_ref(), "class.laws", class.laws.to_vec().into_boxed_slice());
        }
        push_inert_and_musi_attrs(&mut out, target.as_ref(), &class.inert_attrs, &class.musi_attrs);
    }

    for effect in &surface.exported_effects {
        let target = qualified_name(&effect.key.module, effect.key.name.as_ref());
        if !effect.laws.is_empty() {
            push_meta(&mut out, target.as_ref(), "effect.laws", effect.laws.to_vec().into_boxed_slice());
        }
        push_inert_and_musi_attrs(&mut out, target.as_ref(), &effect.inert_attrs, &effect.musi_attrs);
    }

    for data in &surface.exported_data {
        let target = qualified_name(&data.key.module, data.key.name.as_ref());
        push_inert_and_musi_attrs(&mut out, target.as_ref(), &data.inert_attrs, &data.musi_attrs);
    }

    for export in &surface.exported_values {
        let target = qualified_name(&surface.module_key, export.name.as_ref());
        push_inert_and_musi_attrs(&mut out, target.as_ref(), &export.inert_attrs, &export.musi_attrs);
        push_export_sig_meta(&mut out, surface, target.as_ref(), export);
    }

    for (idx, inst) in surface.exported_instances.iter().enumerate() {
        let target: Box<str> = format!("{}::instance::{idx}", surface.module_key.as_str()).into();
        push_inert_and_musi_attrs(&mut out, target.as_ref(), &inst.inert_attrs, &inst.musi_attrs);
    }

    out.into_boxed_slice()
}
