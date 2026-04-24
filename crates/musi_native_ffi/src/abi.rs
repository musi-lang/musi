use std::collections::BTreeSet;

use musi_vm::{ForeignCall, ProgramTypeAbiKind, VmResult};
use music_seam::TypeId;

use crate::ffi::{FfiTypeRef, build_ffi_type, touch_ffi_type};
use crate::native_abi_issue;

#[derive(Debug)]
pub enum NativeAbiType {
    Unit,
    Bool { ty: TypeId },
    Int { signed: bool, bits: u8 },
    Float { bits: u8 },
    CString,
    CPtr,
    Transparent { ty: TypeId, inner: Box<Self> },
    ReprCProduct { ty: TypeId, fields: Box<[Self]> },
}

pub struct NativeAbiSignature {
    pub arg_tys: Vec<NativeAbiType>,
    pub arg_ffi: Vec<FfiTypeRef>,
    pub result_ty: NativeAbiType,
    pub result_ffi: FfiTypeRef,
}

impl NativeAbiSignature {
    pub fn build(foreign: &ForeignCall) -> VmResult<Self> {
        let mut seen = BTreeSet::<u32>::new();
        let arg_tys = foreign
            .param_tys()
            .iter()
            .copied()
            .map(|ty| native_abi_type(foreign, ty, &mut seen))
            .collect::<VmResult<Vec<_>>>()?;
        let result_ty = native_abi_type(foreign, foreign.result_ty(), &mut BTreeSet::new())?;
        let mut arg_ffi = arg_tys
            .iter()
            .map(build_ffi_type)
            .collect::<VmResult<Vec<_>>>()?;
        let result_ffi = build_ffi_type(&result_ty)?;
        for ffi in &mut arg_ffi {
            touch_ffi_type(ffi);
        }
        let mut result_ffi = result_ffi;
        touch_ffi_type(&mut result_ffi);
        Ok(Self {
            arg_tys,
            arg_ffi,
            result_ty,
            result_ffi,
        })
    }
}

fn native_abi_type(
    foreign: &ForeignCall,
    ty: TypeId,
    seen: &mut BTreeSet<u32>,
) -> VmResult<NativeAbiType> {
    if !seen.insert(ty.raw()) {
        return Err(native_abi_issue(
            foreign,
            format!(
                "native ABI type `{}` is recursive by value",
                foreign.type_name(ty)
            ),
        ));
    }
    let abi_ty = match foreign.type_abi_kind(ty) {
        ProgramTypeAbiKind::Unit => NativeAbiType::Unit,
        ProgramTypeAbiKind::Bool => NativeAbiType::Bool { ty },
        ProgramTypeAbiKind::Int { signed, bits } => NativeAbiType::Int { signed, bits },
        ProgramTypeAbiKind::Float { bits } => NativeAbiType::Float { bits },
        ProgramTypeAbiKind::CString => NativeAbiType::CString,
        ProgramTypeAbiKind::CPtr => NativeAbiType::CPtr,
        ProgramTypeAbiKind::DataTransparent => transparent_abi_type(foreign, ty, seen)?,
        ProgramTypeAbiKind::DataReprCProduct => repr_c_product_abi_type(foreign, ty, seen)?,
        ProgramTypeAbiKind::Unsupported => {
            return Err(native_abi_issue(
                foreign,
                format!(
                    "type `{}` is not native-ABI compatible",
                    foreign.type_name(ty)
                ),
            ));
        }
    };
    let _ = seen.remove(&ty.raw());
    Ok(abi_ty)
}

fn transparent_abi_type(
    foreign: &ForeignCall,
    ty: TypeId,
    seen: &mut BTreeSet<u32>,
) -> VmResult<NativeAbiType> {
    let layout = foreign.type_data_layout(ty).ok_or_else(|| {
        native_abi_issue(
            foreign,
            format!(
                "transparent type `{}` is missing layout",
                foreign.type_name(ty)
            ),
        )
    })?;
    let variant = layout.single_variant().ok_or_else(|| {
        native_abi_issue(
            foreign,
            format!(
                "transparent type `{}` is not a single-variant wrapper",
                foreign.type_name(ty)
            ),
        )
    })?;
    let Some(field_ty) = variant.field_tys.first().copied() else {
        return Err(native_abi_issue(
            foreign,
            format!(
                "transparent type `{}` is missing a wrapped field",
                foreign.type_name(ty)
            ),
        ));
    };
    Ok(NativeAbiType::Transparent {
        ty,
        inner: Box::new(native_abi_type(foreign, field_ty, seen)?),
    })
}

fn repr_c_product_abi_type(
    foreign: &ForeignCall,
    ty: TypeId,
    seen: &mut BTreeSet<u32>,
) -> VmResult<NativeAbiType> {
    let layout = foreign.type_data_layout(ty).ok_or_else(|| {
        native_abi_issue(
            foreign,
            format!("repr(c) type `{}` is missing layout", foreign.type_name(ty)),
        )
    })?;
    let variant = layout.single_variant().ok_or_else(|| {
        native_abi_issue(
            foreign,
            format!(
                "repr(c) type `{}` is not a single-variant product",
                foreign.type_name(ty)
            ),
        )
    })?;
    let fields = variant
        .field_tys
        .iter()
        .copied()
        .map(|field_ty| native_abi_type(foreign, field_ty, seen))
        .collect::<VmResult<Vec<_>>>()?
        .into_boxed_slice();
    Ok(NativeAbiType::ReprCProduct { ty, fields })
}
