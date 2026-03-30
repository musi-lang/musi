use music_il::descriptors;
use music_il::{TypeDescriptor, TypeDescriptors, TypeKind};
use music_ir::{IrScalarTy, IrTypeRef};

pub(super) const fn builtin_ty_descriptors() -> TypeDescriptors {
    // Keep builtin table empty for now; SEAM reserves ids in `music_il::descriptors::builtins`.
    Vec::new()
}

pub(super) fn ensure_tuple_ty(types: &mut TypeDescriptors, arity: usize) -> u16 {
    let id =
        descriptors::FIRST_EMITTED_TYPE_ID.saturating_add(u16::try_from(types.len()).unwrap_or(0));
    types.push(TypeDescriptor {
        id,
        key: format!("tuple({arity})"),
        kind: TypeKind::Record,
        member_count: u16::try_from(arity).unwrap_or(0),
    });
    id
}

pub(super) fn ensure_record_ty(
    types: &mut TypeDescriptors,
    key: String,
    field_count: usize,
) -> u16 {
    let id =
        descriptors::FIRST_EMITTED_TYPE_ID.saturating_add(u16::try_from(types.len()).unwrap_or(0));
    types.push(TypeDescriptor {
        id,
        key,
        kind: TypeKind::Record,
        member_count: u16::try_from(field_count).unwrap_or(0),
    });
    id
}

pub(super) fn ensure_choice_ty(
    types: &mut TypeDescriptors,
    key: String,
    variant_count: usize,
) -> u16 {
    let id =
        descriptors::FIRST_EMITTED_TYPE_ID.saturating_add(u16::try_from(types.len()).unwrap_or(0));
    types.push(TypeDescriptor {
        id,
        key,
        kind: TypeKind::Choice,
        member_count: u16::try_from(variant_count).unwrap_or(0),
    });
    id
}

pub(super) const fn builtin_ty_id_for_ref(ty: IrTypeRef) -> u16 {
    match ty {
        IrTypeRef::Scalar(s) => match s {
            IrScalarTy::Unit => descriptors::BUILTIN_TYPE_UNIT,
            IrScalarTy::Bool => descriptors::BUILTIN_TYPE_BOOL,
            IrScalarTy::Int => descriptors::BUILTIN_TYPE_INT,
            IrScalarTy::Float => descriptors::BUILTIN_TYPE_FLOAT,
            IrScalarTy::String => descriptors::BUILTIN_TYPE_STRING,
        },
        IrTypeRef::Any => descriptors::BUILTIN_TYPE_ANY,
        IrTypeRef::Error => descriptors::BUILTIN_TYPE_NEVER,
        IrTypeRef::Unknown | IrTypeRef::Named(_) => descriptors::BUILTIN_TYPE_UNKNOWN,
    }
}
