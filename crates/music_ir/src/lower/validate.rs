use music_base::diag::{Diag, DiagContext};
use music_sema::{SemaModule, SurfaceEffectRow, SurfaceTy, SurfaceTyId, SurfaceTyKind};

use crate::{IrDiagKind as DiagKind, api::IrDiagList};

pub(super) fn validate_surface(sema: &SemaModule, diags: &mut IrDiagList) {
    let surface = sema.surface();
    let types = surface.types();
    for export in surface.exported_values() {
        validate_surface_ty_id(types, export.ty, diags);
        validate_effect_row(types, &export.effects, diags);
        for constraint in &export.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
    }
    for effect in surface.exported_effects() {
        for op in &effect.ops {
            for param in &op.params {
                validate_surface_ty_id(types, *param, diags);
            }
            validate_surface_ty_id(types, op.result, diags);
        }
    }
    for shape in surface.exported_shapes() {
        for constraint in &shape.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
        for member in &shape.members {
            for param in &member.params {
                validate_surface_ty_id(types, *param, diags);
            }
            validate_surface_ty_id(types, member.result, diags);
        }
    }
    for given in surface.exported_givens() {
        for arg in &given.shape_args {
            validate_surface_ty_id(types, *arg, diags);
        }
        for constraint in &given.constraints {
            validate_surface_ty_id(types, constraint.value, diags);
        }
    }
}

fn validate_effect_row(types: &[SurfaceTy], row: &SurfaceEffectRow, diags: &mut IrDiagList) {
    for item in &row.items {
        if let Some(arg) = item.arg {
            validate_surface_ty_id(types, arg, diags);
        }
    }
}

fn validate_surface_ty_id(types: &[SurfaceTy], id: SurfaceTyId, diags: &mut IrDiagList) {
    let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
    let Some(ty) = types.get(index) else {
        let context = DiagContext::new().with("id", id.raw());
        diags.push(
            Diag::error(DiagKind::InvalidSurfaceTypeId.message_with(&context))
                .with_code(DiagKind::InvalidSurfaceTypeId.code())
                .with_note(DiagKind::InvalidSurfaceTypeId.label_with(&context)),
        );
        return;
    };
    validate_surface_ty(types, ty, diags);
}

fn validate_surface_ty(types: &[SurfaceTy], ty: &SurfaceTy, diags: &mut IrDiagList) {
    match &ty.kind {
        SurfaceTyKind::Named { args, .. } => {
            for arg in args.iter().copied() {
                validate_surface_ty_id(types, arg, diags);
            }
        }
        SurfaceTyKind::Arrow { params, ret, .. } => {
            for param in params.iter().copied() {
                validate_surface_ty_id(types, param, diags);
            }
            validate_surface_ty_id(types, *ret, diags);
        }
        SurfaceTyKind::Pi {
            binder_ty, body, ..
        } => {
            validate_surface_ty_id(types, *binder_ty, diags);
            validate_surface_ty_id(types, *body, diags);
        }
        SurfaceTyKind::Sum { left, right } => {
            validate_surface_ty_id(types, *left, diags);
            validate_surface_ty_id(types, *right, diags);
        }
        SurfaceTyKind::Tuple { items } => {
            for item in items.iter().copied() {
                validate_surface_ty_id(types, item, diags);
            }
        }
        SurfaceTyKind::Seq { item } | SurfaceTyKind::Array { item, .. } => {
            validate_surface_ty_id(types, *item, diags);
        }
        SurfaceTyKind::Range { bound } => validate_surface_ty_id(types, *bound, diags),
        SurfaceTyKind::Handler {
            effect,
            input,
            output,
        } => {
            validate_surface_ty_id(types, *effect, diags);
            validate_surface_ty_id(types, *input, diags);
            validate_surface_ty_id(types, *output, diags);
        }
        SurfaceTyKind::Mut { inner } => validate_surface_ty_id(types, *inner, diags),
        SurfaceTyKind::AnyShape { capability: shape }
        | SurfaceTyKind::SomeShape { capability: shape } => {
            validate_surface_ty_id(types, *shape, diags);
        }
        SurfaceTyKind::Record { fields } => {
            for field in fields {
                validate_surface_ty_id(types, field.ty, diags);
            }
        }
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
        | SurfaceTyKind::NatLit(_) => {}
    }
}
