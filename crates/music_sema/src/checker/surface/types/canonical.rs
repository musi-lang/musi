use music_module::ModuleKey;
use music_names::{Interner, Symbol};

use crate::api::{
    DefinitionKey, ModuleSurface, SurfaceDim, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
};

use super::simple::{SimpleTyKind, simple_ty_info};

pub fn surface_key(module_key: &ModuleKey, interner: &Interner, name: Symbol) -> DefinitionKey {
    DefinitionKey::new(module_key.clone(), interner.resolve(name))
}

pub fn canonical_surface_ty(surface: &ModuleSurface, ty: SurfaceTyId) -> String {
    let kind = &surface
        .try_ty(ty)
        .expect("surface type missing while formatting")
        .kind;
    canonical_surface_ty_kind(surface, kind)
}

fn canonical_surface_ty_kind(surface: &ModuleSurface, kind: &SurfaceTyKind) -> String {
    if let Some(rendered) = canonical_simple_surface_ty(kind) {
        return rendered;
    }
    match kind {
        SurfaceTyKind::Named { name, args } => canonical_surface_named(surface, name, args),
        SurfaceTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => canonical_surface_pi(surface, binder, *binder_ty, *body, *is_effectful),
        SurfaceTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => canonical_surface_arrow(surface, params, *ret, *is_effectful),
        SurfaceTyKind::Sum { left, right } => {
            format!(
                "{} + {}",
                canonical_surface_ty(surface, *left),
                canonical_surface_ty(surface, *right)
            )
        }
        SurfaceTyKind::Tuple { items } => canonical_surface_tuple(surface, items),
        SurfaceTyKind::Seq { item } => format!("[]{}", canonical_surface_ty(surface, *item)),
        SurfaceTyKind::Array { dims, item } => canonical_surface_array(surface, dims, *item),
        SurfaceTyKind::Bits { width } => format!("Bits[{width}]"),
        SurfaceTyKind::Range { bound } => canonical_surface_wrapped(surface, "Range", *bound),
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => canonical_surface_handler(surface, *effect, *input, *output),
        SurfaceTyKind::Mut { inner } => canonical_surface_prefixed(surface, "mut", *inner),
        SurfaceTyKind::AnyShape { capability } => {
            canonical_surface_prefixed(surface, "any", *capability)
        }
        SurfaceTyKind::SomeShape { capability } => {
            canonical_surface_prefixed(surface, "some", *capability)
        }
        SurfaceTyKind::Record { fields } => canonical_surface_record(surface, fields),
        _ => canonical_simple_surface_ty(kind).expect("simple surface type should render"),
    }
}

fn canonical_surface_wrapped(surface: &ModuleSurface, name: &str, inner: SurfaceTyId) -> String {
    format!("{name}[{}]", canonical_surface_ty(surface, inner))
}

fn canonical_surface_handler(
    surface: &ModuleSurface,
    effect: SurfaceTyId,
    input: SurfaceTyId,
    output: SurfaceTyId,
) -> String {
    format!(
        "answer {} ({} -> {})",
        canonical_surface_ty(surface, effect),
        canonical_surface_ty(surface, input),
        canonical_surface_ty(surface, output)
    )
}

fn canonical_surface_prefixed(surface: &ModuleSurface, prefix: &str, inner: SurfaceTyId) -> String {
    format!("{prefix} {}", canonical_surface_ty(surface, inner))
}

fn canonical_simple_surface_ty(kind: &SurfaceTyKind) -> Option<String> {
    let simple = SimpleTyKind::from_surface(kind)?;
    if let SimpleTyKind::NatLit(value) = simple {
        return Some(value.to_string());
    }
    simple_ty_info(simple).map(|ty| ty.display_name.to_owned())
}

fn canonical_surface_named(surface: &ModuleSurface, name: &str, args: &[SurfaceTyId]) -> String {
    if args.is_empty() {
        return name.to_owned();
    }
    format!(
        "{}[{}]",
        name,
        args.iter()
            .copied()
            .map(|arg| canonical_surface_ty(surface, arg))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn canonical_surface_arrow(
    surface: &ModuleSurface,
    params: &[SurfaceTyId],
    ret: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let params = params
        .iter()
        .copied()
        .map(|param| canonical_surface_ty(surface, param))
        .collect::<Vec<_>>();
    let left = if params.len() == 1 {
        params[0].clone()
    } else {
        format!("({})", params.join(", "))
    };
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!("{left}{arrow}{}", canonical_surface_ty(surface, ret))
}

fn canonical_surface_pi(
    surface: &ModuleSurface,
    binder: &str,
    binder_ty: SurfaceTyId,
    body: SurfaceTyId,
    is_effectful: bool,
) -> String {
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!(
        "forall ({binder} : {}){arrow}{}",
        canonical_surface_ty(surface, binder_ty),
        canonical_surface_ty(surface, body)
    )
}

fn canonical_surface_tuple(surface: &ModuleSurface, items: &[SurfaceTyId]) -> String {
    format!(
        "({})",
        items
            .iter()
            .copied()
            .map(|item| canonical_surface_ty(surface, item))
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn canonical_surface_array(
    surface: &ModuleSurface,
    dims: &[SurfaceDim],
    item: SurfaceTyId,
) -> String {
    let dims = dims
        .iter()
        .map(|dim| match dim {
            SurfaceDim::Unknown => "_".into(),
            SurfaceDim::Name(name) => name.to_string(),
            SurfaceDim::Int(value) => value.to_string(),
        })
        .collect::<Vec<_>>()
        .join(", ");
    format!("[{dims}]{}", canonical_surface_ty(surface, item))
}

fn canonical_surface_record(surface: &ModuleSurface, fields: &[SurfaceTyField]) -> String {
    format!(
        "{{{}}}",
        fields
            .iter()
            .map(|field| format!(
                "{} = {}",
                field.name,
                canonical_surface_ty(surface, field.ty)
            ))
            .collect::<Vec<_>>()
            .join(", ")
    )
}
