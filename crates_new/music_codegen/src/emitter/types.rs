use music_il::{TypeDescriptor, TypeKind};

pub(super) fn builtin_type_descriptors() -> Vec<TypeDescriptor> {
    // Keep builtin table empty for now; SEAM reserves ids in `music_il::descriptors::builtins`.
    Vec::new()
}

pub(super) fn ensure_tuple_type(types: &mut Vec<TypeDescriptor>, arity: usize) -> u16 {
    let id = u16::try_from(types.len()).unwrap_or(0);
    types.push(TypeDescriptor {
        id,
        key: format!("tuple({arity})"),
        kind: TypeKind::Record,
        member_count: u16::try_from(arity).unwrap_or(0),
    });
    id
}
