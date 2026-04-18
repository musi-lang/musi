use music_module::ModuleKey;
use music_sema::{
    Attr, AttrValue, ConstraintKind, ExportedValue, ModuleSurface, SemaModule, SurfaceDim,
    SurfaceEffectRow, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
};

use crate::api::IrMetaRecord;

type MetaRecordList = Vec<IrMetaRecord>;

struct SimpleSurfaceTyName {
    kind: SurfaceTyKind,
    display_name: &'static str,
}

const SIMPLE_SURFACE_TY_NAMES: &[SimpleSurfaceTyName] = &[
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Error,
        display_name: "<error>",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Unknown,
        display_name: "Unknown",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Type,
        display_name: "Type",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Syntax,
        display_name: "Syntax",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Any,
        display_name: "Any",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Empty,
        display_name: "Empty",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Unit,
        display_name: "Unit",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Bool,
        display_name: "Bool",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Nat,
        display_name: "Nat",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Int,
        display_name: "Int",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Int8,
        display_name: "Int8",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Int16,
        display_name: "Int16",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Int32,
        display_name: "Int32",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Int64,
        display_name: "Int64",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Nat8,
        display_name: "Nat8",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Nat16,
        display_name: "Nat16",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Nat32,
        display_name: "Nat32",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Nat64,
        display_name: "Nat64",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Float,
        display_name: "Float",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Float32,
        display_name: "Float32",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Float64,
        display_name: "Float64",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::String,
        display_name: "String",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Rune,
        display_name: "Rune",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::CString,
        display_name: "CString",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::CPtr,
        display_name: "CPtr",
    },
    SimpleSurfaceTyName {
        kind: SurfaceTyKind::Module,
        display_name: "Module",
    },
];

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

fn format_surface_ty(surface: &ModuleSurface, ty: SurfaceTyId) -> String {
    let Some(ty) = surface.try_ty(ty) else {
        return "Unknown".into();
    };
    if let Some(simple) = format_simple_surface_ty(&ty.kind) {
        return simple;
    }
    match &ty.kind {
        SurfaceTyKind::Named { name, args } => format_named_surface_ty(surface, name, args),
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => format_pi_surface_ty(surface, binder, *binder_ty, *body, *is_effectful),
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => format_arrow_surface_ty(surface, params, *ret, *is_effectful),
        SurfaceTyKind::Sum { left, right } => format!(
            "{} + {}",
            format_surface_ty(surface, *left),
            format_surface_ty(surface, *right)
        ),
        SurfaceTyKind::Tuple { items } => format_tuple_surface_ty(surface, items),
        SurfaceTyKind::Seq { item } => format!("[]{}", format_surface_ty(surface, *item)),
        SurfaceTyKind::Array { dims, item } => format_array_surface_ty(surface, dims, *item),
        SurfaceTyKind::Range { bound } => format!("Range[{}]", format_surface_ty(surface, *bound)),
        SurfaceTyKind::ClosedRange { bound } => {
            format!("ClosedRange[{}]", format_surface_ty(surface, *bound))
        }
        SurfaceTyKind::PartialRangeFrom { bound } => {
            format!("PartialRangeFrom[{}]", format_surface_ty(surface, *bound))
        }
        SurfaceTyKind::PartialRangeUpTo { bound } => {
            format!("PartialRangeUpTo[{}]", format_surface_ty(surface, *bound))
        }
        SurfaceTyKind::PartialRangeThru { bound } => {
            format!("PartialRangeThru[{}]", format_surface_ty(surface, *bound))
        }
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => format!(
            "using {} ({} -> {})",
            format_surface_ty(surface, *effect),
            format_surface_ty(surface, *input),
            format_surface_ty(surface, *output)
        ),
        SurfaceTyKind::Mut { inner } => format!("mut {}", format_surface_ty(surface, *inner)),
        SurfaceTyKind::AnyClass { class } => {
            format!("any {}", format_surface_ty(surface, *class))
        }
        SurfaceTyKind::SomeClass { class } => {
            format!("some {}", format_surface_ty(surface, *class))
        }
        SurfaceTyKind::Record { fields } => format_record_surface_ty(surface, fields),
        SurfaceTyKind::Error
        | SurfaceTyKind::Unknown
        | SurfaceTyKind::Type
        | SurfaceTyKind::Syntax
        | SurfaceTyKind::Any
        | SurfaceTyKind::Empty
        | SurfaceTyKind::Unit
        | SurfaceTyKind::Bool
        | SurfaceTyKind::Nat
        | SurfaceTyKind::Int
        | SurfaceTyKind::Int8
        | SurfaceTyKind::Int16
        | SurfaceTyKind::Int32
        | SurfaceTyKind::Int64
        | SurfaceTyKind::Nat8
        | SurfaceTyKind::Nat16
        | SurfaceTyKind::Nat32
        | SurfaceTyKind::Nat64
        | SurfaceTyKind::Float
        | SurfaceTyKind::Float32
        | SurfaceTyKind::Float64
        | SurfaceTyKind::String
        | SurfaceTyKind::Rune
        | SurfaceTyKind::CString
        | SurfaceTyKind::CPtr
        | SurfaceTyKind::Module
        | SurfaceTyKind::NatLit(_) => "<invalid-simple-surface-ty>".into(),
    }
}

fn format_simple_surface_ty(kind: &SurfaceTyKind) -> Option<String> {
    if let SurfaceTyKind::NatLit(value) = kind {
        return Some(value.to_string());
    }
    SIMPLE_SURFACE_TY_NAMES
        .iter()
        .find_map(|simple| (&simple.kind == kind).then(|| simple.display_name.to_owned()))
}

fn format_named_surface_ty(surface: &ModuleSurface, name: &str, args: &[SurfaceTyId]) -> String {
    if args.is_empty() {
        return name.to_owned();
    }
    let args = args
        .iter()
        .copied()
        .map(|arg| format_surface_ty(surface, arg))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{name}[{args}]")
}

fn format_pi_surface_ty(
    surface: &ModuleSurface,
    binder: &str,
    binder_ty: SurfaceTyId,
    body: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let binder_ty = format_surface_ty(surface, binder_ty);
    let body = format_surface_ty(surface, body);
    let arrow = if is_effectful { "~>" } else { "->" };
    format!("forall ({binder}: {binder_ty}) {arrow} {body}")
}

fn format_arrow_surface_ty(
    surface: &ModuleSurface,
    params: &[SurfaceTyId],
    ret: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let params = params
        .iter()
        .copied()
        .map(|param| format_surface_ty(surface, param))
        .collect::<Vec<_>>()
        .join(", ");
    let ret = format_surface_ty(surface, ret);
    let arrow = if is_effectful { "~>" } else { "->" };
    format!("({params}) {arrow} {ret}")
}

fn format_tuple_surface_ty(surface: &ModuleSurface, items: &[SurfaceTyId]) -> String {
    let items = items
        .iter()
        .copied()
        .map(|item| format_surface_ty(surface, item))
        .collect::<Vec<_>>()
        .join(", ");
    format!("({items})")
}

fn format_array_surface_ty(
    surface: &ModuleSurface,
    dims: &[SurfaceDim],
    item: SurfaceTyId,
) -> String {
    let item = format_surface_ty(surface, item);
    let mut out = String::new();
    for dim in dims {
        out.push('[');
        out.push_str(&format_surface_dim(dim));
        out.push(']');
    }
    out.push_str(&item);
    out
}

fn format_surface_dim(dim: &SurfaceDim) -> String {
    match dim {
        SurfaceDim::Unknown => "_".into(),
        SurfaceDim::Name(name) => name.to_string(),
        SurfaceDim::Int(value) => value.to_string(),
    }
}

fn format_record_surface_ty(surface: &ModuleSurface, fields: &[SurfaceTyField]) -> String {
    let fields = fields
        .iter()
        .map(|field| format!("{}: {}", field.name, format_surface_ty(surface, field.ty)))
        .collect::<Vec<_>>()
        .join("; ");
    format!("{{ {fields} }}")
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
    format!("using {{ {} }}", items.join(", "))
}

fn push_meta(out: &mut MetaRecordList, target: &str, key: &'static str, values: Box<[Box<str>]>) {
    out.push(IrMetaRecord::new(target, key, values));
}

fn push_inert_and_musi_attrs(
    out: &mut MetaRecordList,
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
    out: &mut MetaRecordList,
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
                        ConstraintKind::TypeEq => "~=",
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

    for class in surface.exported_classes() {
        let target = qualified_name(&class.key.module, class.key.name.as_ref());
        if !class.laws.is_empty() {
            push_meta(
                &mut out,
                target.as_ref(),
                "class.laws",
                class
                    .laws
                    .iter()
                    .map(|law| law.name.clone())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
        }
        push_inert_and_musi_attrs(
            &mut out,
            target.as_ref(),
            &class.inert_attrs,
            &class.musi_attrs,
        );
    }

    for effect in surface.exported_effects() {
        let target = qualified_name(&effect.key.module, effect.key.name.as_ref());
        if !effect.laws.is_empty() {
            push_meta(
                &mut out,
                target.as_ref(),
                "effect.laws",
                effect
                    .laws
                    .iter()
                    .map(|law| law.name.clone())
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            );
        }
        push_inert_and_musi_attrs(
            &mut out,
            target.as_ref(),
            &effect.inert_attrs,
            &effect.musi_attrs,
        );
    }

    for data in surface.exported_data_defs() {
        let target = qualified_name(&data.key.module, data.key.name.as_ref());
        push_inert_and_musi_attrs(
            &mut out,
            target.as_ref(),
            &data.inert_attrs,
            &data.musi_attrs,
        );
    }

    for export in surface.exported_values() {
        let target = qualified_name(surface.module_key(), export.name.as_ref());
        push_inert_and_musi_attrs(
            &mut out,
            target.as_ref(),
            &export.inert_attrs,
            &export.musi_attrs,
        );
        push_export_sig_meta(&mut out, surface, target.as_ref(), export);
    }

    for (idx, inst) in surface.exported_instances().iter().enumerate() {
        let target: Box<str> = format!("{}::instance::{idx}", surface.module_key().as_str()).into();
        push_inert_and_musi_attrs(
            &mut out,
            target.as_ref(),
            &inst.inert_attrs,
            &inst.musi_attrs,
        );
    }

    out.into_boxed_slice()
}
