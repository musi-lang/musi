use std::ffi::{c_long, c_uint, c_void};
use std::ptr::null_mut;

use musi_vm::{NativeFailureStage, VmResult};

use crate::abi::NativeAbiType;
use crate::native_call_failed;

pub const FFI_OK: c_uint = 0;
const FFI_TYPE_STRUCT: u16 = 13;

#[repr(C)]
pub struct FfiType {
    size: usize,
    alignment: u16,
    type_code: u16,
    elements: *mut *mut Self,
}

#[repr(C)]
pub struct FfiCif {
    abi: c_uint,
    nargs: c_uint,
    arg_types: *mut *mut FfiType,
    rtype: *mut FfiType,
    bytes: c_uint,
    flags: c_uint,
    #[cfg(all(target_arch = "aarch64", target_vendor = "apple"))]
    aarch64_nfixedargs: c_uint,
}

pub enum FfiTypeRef {
    Borrowed(*mut FfiType),
    Owned(Box<OwnedStructType>),
}

pub struct OwnedStructType {
    raw: FfiType,
    children: Vec<FfiTypeRef>,
    elements: Box<[*mut FfiType]>,
    offsets: Box<[usize]>,
}

#[link(name = "ffi")]
unsafe extern "C" {
    static mut ffi_type_void: FfiType;
    static mut ffi_type_uint8: FfiType;
    static mut ffi_type_sint8: FfiType;
    static mut ffi_type_uint16: FfiType;
    static mut ffi_type_sint16: FfiType;
    static mut ffi_type_uint32: FfiType;
    static mut ffi_type_sint32: FfiType;
    static mut ffi_type_uint64: FfiType;
    static mut ffi_type_sint64: FfiType;
    static mut ffi_type_float: FfiType;
    static mut ffi_type_double: FfiType;
    static mut ffi_type_pointer: FfiType;

    pub fn ffi_prep_cif(
        cif: *mut FfiCif,
        abi: c_uint,
        nargs: c_uint,
        rtype: *mut FfiType,
        atypes: *mut *mut FfiType,
    ) -> c_uint;
    pub fn ffi_get_struct_offsets(
        abi: c_uint,
        struct_type: *mut FfiType,
        offsets: *mut c_long,
    ) -> c_uint;
    pub fn ffi_call(
        cif: *mut FfiCif,
        fn_: *mut c_void,
        rvalue: *mut c_void,
        avalue: *mut *mut c_void,
    );
}

impl FfiTypeRef {
    pub fn as_mut_ptr(&mut self) -> *mut FfiType {
        match self {
            Self::Borrowed(ptr) => *ptr,
            Self::Owned(owned) => &raw mut owned.raw,
        }
    }

    pub fn struct_offsets(&self) -> Option<&[usize]> {
        match self {
            Self::Borrowed(_) => None,
            Self::Owned(owned) => Some(&owned.offsets),
        }
    }

    pub fn struct_size(&self) -> Option<usize> {
        match self {
            Self::Borrowed(_) => None,
            Self::Owned(owned) => Some(owned.raw.size),
        }
    }
}

pub fn build_ffi_type(ty: &NativeAbiType) -> VmResult<FfiTypeRef> {
    match ty {
        NativeAbiType::Unit => Ok(FfiTypeRef::Borrowed(ffi_type_void_ptr())),
        NativeAbiType::Bool { .. } => Ok(FfiTypeRef::Borrowed(ffi_type_uint8_ptr())),
        NativeAbiType::Int { signed, bits } => {
            Ok(FfiTypeRef::Borrowed(ffi_type_int_ptr(*signed, *bits)))
        }
        NativeAbiType::Float { bits } => Ok(FfiTypeRef::Borrowed(ffi_type_float_ptr(*bits))),
        NativeAbiType::CString | NativeAbiType::CPtr => {
            Ok(FfiTypeRef::Borrowed(ffi_type_pointer_ptr()))
        }
        NativeAbiType::Transparent { inner, .. } => build_ffi_type(inner),
        NativeAbiType::ReprCProduct { fields, .. } => build_struct_ffi_type(fields),
    }
}

fn build_struct_ffi_type(fields: &[NativeAbiType]) -> VmResult<FfiTypeRef> {
    let mut children = fields
        .iter()
        .map(build_ffi_type)
        .collect::<VmResult<Vec<_>>>()?;
    let mut elements = children
        .iter_mut()
        .map(FfiTypeRef::as_mut_ptr)
        .collect::<Vec<_>>();
    elements.push(null_mut());
    let mut owned = Box::new(OwnedStructType {
        raw: FfiType {
            size: 0,
            alignment: 0,
            type_code: FFI_TYPE_STRUCT,
            elements: null_mut(),
        },
        children,
        elements: elements.into_boxed_slice(),
        offsets: vec![0; fields.len()].into_boxed_slice(),
    });
    owned.raw.elements = owned.elements.as_mut_ptr();
    let mut ffi_offsets = vec![c_long::default(); fields.len()];
    let status = {
        // SAFETY: `owned.raw` points to a live struct type and `ffi_offsets` has one slot per field.
        unsafe {
            ffi_get_struct_offsets(
                default_ffi_abi(),
                &raw mut owned.raw,
                ffi_offsets.as_mut_ptr(),
            )
        }
    };
    if status != FFI_OK {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::AbiUnsupported,
            None,
            None,
            format!(
                "ffi_get_struct_offsets failed with status `{status}` for abi `{}`",
                default_ffi_abi()
            )
            .into(),
        ));
    }
    owned.offsets = ffi_offsets
        .into_iter()
        .map(|offset| usize::try_from(offset).unwrap_or(usize::MAX))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Ok(FfiTypeRef::Owned(owned))
}

pub fn touch_ffi_type(ffi: &mut FfiTypeRef) {
    if let FfiTypeRef::Owned(owned) = ffi {
        for child in &mut owned.children {
            touch_ffi_type(child);
        }
    }
}

pub fn ffi_child(ffi: &FfiTypeRef, index: usize) -> Option<&FfiTypeRef> {
    match ffi {
        FfiTypeRef::Borrowed(_) => None,
        FfiTypeRef::Owned(owned) => owned.children.get(index),
    }
}

pub fn ffi_type_void_ptr() -> *mut FfiType {
    &raw mut ffi_type_void
}

pub fn ffi_type_uint8_ptr() -> *mut FfiType {
    &raw mut ffi_type_uint8
}

fn ffi_type_sint8_ptr() -> *mut FfiType {
    &raw mut ffi_type_sint8
}

fn ffi_type_uint16_ptr() -> *mut FfiType {
    &raw mut ffi_type_uint16
}

fn ffi_type_sint16_ptr() -> *mut FfiType {
    &raw mut ffi_type_sint16
}

fn ffi_type_uint32_ptr() -> *mut FfiType {
    &raw mut ffi_type_uint32
}

fn ffi_type_sint32_ptr() -> *mut FfiType {
    &raw mut ffi_type_sint32
}

fn ffi_type_uint64_ptr() -> *mut FfiType {
    &raw mut ffi_type_uint64
}

fn ffi_type_sint64_ptr() -> *mut FfiType {
    &raw mut ffi_type_sint64
}

pub fn ffi_type_float_ptr(bits: u8) -> *mut FfiType {
    match bits {
        32 => &raw mut ffi_type_float,
        _ => ffi_type_double_ptr(),
    }
}

pub fn ffi_type_int_ptr(signed: bool, bits: u8) -> *mut FfiType {
    match (signed, bits) {
        (true, 8) => ffi_type_sint8_ptr(),
        (false, 8) => ffi_type_uint8_ptr(),
        (true, 16) => ffi_type_sint16_ptr(),
        (false, 16) => ffi_type_uint16_ptr(),
        (true, 32) => ffi_type_sint32_ptr(),
        (false, 32) => ffi_type_uint32_ptr(),
        (true, _) => ffi_type_sint64_ptr(),
        (false, _) => ffi_type_uint64_ptr(),
    }
}

fn ffi_type_double_ptr() -> *mut FfiType {
    &raw mut ffi_type_double
}

pub fn ffi_type_pointer_ptr() -> *mut FfiType {
    &raw mut ffi_type_pointer
}

pub fn usize_to_mut_ptr(address: usize) -> *mut c_void {
    null_mut::<c_void>().with_addr(address)
}

pub fn write_bytes(out: &mut [u8], offset: usize, bytes: &[u8]) -> VmResult<()> {
    let end = offset.saturating_add(bytes.len());
    let Some(target) = out.get_mut(offset..end) else {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::AbiUnsupported,
            None,
            None,
            "native struct layout write overflow".into(),
        ));
    };
    target.copy_from_slice(bytes);
    Ok(())
}

pub fn read_u8(bytes: &[u8], offset: usize) -> VmResult<u8> {
    bytes.get(offset).copied().ok_or_else(|| {
        native_call_failed(
            "<struct>".into(),
            NativeFailureStage::ResultInvalid,
            None,
            None,
            "native struct result read overflow".into(),
        )
    })
}

pub fn read_array<const N: usize>(bytes: &[u8], offset: usize) -> VmResult<[u8; N]> {
    let end = offset.saturating_add(N);
    let Some(source) = bytes.get(offset..end) else {
        return Err(native_call_failed(
            "<struct>".into(),
            NativeFailureStage::ResultInvalid,
            None,
            None,
            "native struct result read overflow".into(),
        ));
    };
    let mut out = [0_u8; N];
    out.copy_from_slice(source);
    Ok(out)
}

#[cfg(all(target_arch = "x86_64", not(target_os = "windows")))]
pub const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"))]
pub const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")))]
pub const fn default_ffi_abi() -> c_uint {
    1
}

#[cfg(all(target_arch = "aarch64", not(target_os = "windows")))]
pub const fn default_ffi_abi() -> c_uint {
    1
}

#[cfg(all(target_arch = "aarch64", target_os = "windows"))]
pub const fn default_ffi_abi() -> c_uint {
    2
}

#[cfg(not(any(
    all(target_arch = "x86_64", not(target_os = "windows")),
    all(target_arch = "x86_64", target_os = "windows", target_env = "gnu"),
    all(target_arch = "x86_64", target_os = "windows", not(target_env = "gnu")),
    all(target_arch = "aarch64", not(target_os = "windows")),
    all(target_arch = "aarch64", target_os = "windows")
)))]
pub const fn default_ffi_abi() -> c_uint {
    1
}
