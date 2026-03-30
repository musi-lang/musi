use music_il::descriptors;
use music_il::{TypeDescriptor, TypeKind};

pub(super) fn builtin_type_descriptors() -> Vec<TypeDescriptor> {
    // Keep builtin table empty for now; SEAM reserves ids in `music_il::descriptors::builtins`.
    Vec::new()
}

pub(super) fn ensure_tuple_type(types: &mut Vec<TypeDescriptor>, arity: usize) -> u16 {
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

pub(super) fn ensure_record_type(
    types: &mut Vec<TypeDescriptor>,
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

pub(super) fn ensure_choice_type(
    types: &mut Vec<TypeDescriptor>,
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

pub(super) fn builtin_type_id_for_ref(ty: music_ir::IrTypeRef) -> u16 {
    match ty {
        music_ir::IrTypeRef::Scalar(s) => match s {
            music_ir::IrScalarTy::Unit => descriptors::BUILTIN_TYPE_UNIT,
            music_ir::IrScalarTy::Bool => descriptors::BUILTIN_TYPE_BOOL,
            music_ir::IrScalarTy::Int => descriptors::BUILTIN_TYPE_INT,
            music_ir::IrScalarTy::Float => descriptors::BUILTIN_TYPE_FLOAT,
            music_ir::IrScalarTy::String => descriptors::BUILTIN_TYPE_STRING,
        },
        music_ir::IrTypeRef::Any => descriptors::BUILTIN_TYPE_ANY,
        music_ir::IrTypeRef::Unknown => descriptors::BUILTIN_TYPE_UNKNOWN,
        music_ir::IrTypeRef::Error => descriptors::BUILTIN_TYPE_NEVER,
        music_ir::IrTypeRef::Named(_) => descriptors::BUILTIN_TYPE_UNKNOWN,
    }
}
