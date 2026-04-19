use std::env::consts::OS;

use musi_vm::{
    EffectCall, ForeignCall, ProgramTypeAbiKind, Value, VmError, VmErrorKind, VmHostCallContext,
    VmResult,
};
use music_seam::TypeId;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeAbiTypePosition {
    Param(usize),
    Result,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NativeAbiCallSupport {
    Supported,
    UnsupportedTarget,
    UnsupportedAbi {
        abi: Box<str>,
    },
    MissingLink,
    UnsupportedType {
        position: NativeAbiTypePosition,
        ty: Box<str>,
        kind: ProgramTypeAbiKind,
    },
}

impl NativeAbiCallSupport {
    #[must_use]
    pub const fn is_supported(&self) -> bool {
        matches!(self, Self::Supported)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PlatformHost {
    supported_target: bool,
}

impl Default for PlatformHost {
    fn default() -> Self {
        Self::new()
    }
}

impl PlatformHost {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            supported_target: is_supported_target(),
        }
    }

    #[cfg_attr(not(test), allow(dead_code))]
    #[must_use]
    pub fn supports_native_abi_call(self, foreign: &ForeignCall) -> bool {
        self.native_abi_support(foreign).is_supported()
    }

    #[cfg_attr(not(test), allow(dead_code))]
    #[must_use]
    pub fn native_abi_support(self, foreign: &ForeignCall) -> NativeAbiCallSupport {
        if !self.supported_target {
            return NativeAbiCallSupport::UnsupportedTarget;
        }
        if foreign.abi() != "c" {
            return NativeAbiCallSupport::UnsupportedAbi {
                abi: foreign.abi().into(),
            };
        }
        for (index, ty) in foreign.param_tys().iter().copied().enumerate() {
            let kind = native_abi_kind_for_type(foreign, ty);
            if !kind.is_supported_native_abi() {
                return NativeAbiCallSupport::UnsupportedType {
                    position: NativeAbiTypePosition::Param(index),
                    ty: foreign.type_name(ty).into(),
                    kind,
                };
            }
        }
        let result_kind = native_abi_kind_for_type(foreign, foreign.result_ty());
        if !result_kind.is_supported_native_abi() {
            return NativeAbiCallSupport::UnsupportedType {
                position: NativeAbiTypePosition::Result,
                ty: foreign.result_ty_name().into(),
                kind: result_kind,
            };
        }
        if foreign.link().is_none() {
            return NativeAbiCallSupport::MissingLink;
        }
        NativeAbiCallSupport::Supported
    }

    #[cfg_attr(not(test), allow(dead_code))]
    #[must_use]
    pub fn foreign_uses_data_layout(foreign: &ForeignCall) -> bool {
        foreign.param_tys().iter().enumerate().any(|(index, ty)| {
            foreign
                .param_abi_kind(index)
                .is_some_and(ProgramTypeAbiKind::uses_data_layout)
                || foreign.type_data_layout(*ty).is_some()
        }) || foreign.result_abi_kind().uses_data_layout()
            || foreign.result_data_layout().is_some()
    }

    pub fn call_foreign(
        self,
        ctx: VmHostCallContext<'_, '_>,
        foreign: &ForeignCall,
        args: &[Value],
    ) -> Option<VmResult<Value>> {
        if let Some(result) = musi_native_ffi::call_musi_pointer_intrinsic(ctx, foreign, args) {
            return Some(result);
        }
        match self.native_abi_support(foreign) {
            NativeAbiCallSupport::Supported => {
                Some(musi_native_ffi::call_foreign(ctx, foreign, args))
            }
            NativeAbiCallSupport::UnsupportedTarget
            | NativeAbiCallSupport::UnsupportedAbi { .. }
            | NativeAbiCallSupport::MissingLink
            | NativeAbiCallSupport::UnsupportedType { .. } => None,
        }
    }

    #[must_use]
    pub fn handle_effect(self, effect: &EffectCall, _args: &[Value]) -> Option<VmResult<Value>> {
        if self.supported_target {
            return None;
        }
        Some(Err(VmError::new(VmErrorKind::EffectRejected {
            effect: effect.effect_name().into(),
            op: Some(effect.op_name().into()),
            reason: unsupported_target_reason(),
        })))
    }
}

fn native_abi_kind_for_type(foreign: &ForeignCall, ty: TypeId) -> ProgramTypeAbiKind {
    match foreign.type_abi_kind(ty) {
        ProgramTypeAbiKind::DataTransparent => foreign
            .type_data_layout(ty)
            .and_then(|layout| layout.single_variant())
            .and_then(|variant| variant.field_tys.first().copied())
            .map_or(ProgramTypeAbiKind::Unsupported, |field_ty| {
                native_abi_kind_for_type(foreign, field_ty)
            }),
        ProgramTypeAbiKind::DataReprCProduct => {
            foreign
                .type_data_layout(ty)
                .map_or(ProgramTypeAbiKind::Unsupported, |layout| {
                    layout
                        .single_variant()
                        .filter(|variant| {
                            variant.field_tys.iter().copied().all(|field_ty| {
                                native_abi_kind_for_type(foreign, field_ty)
                                    .is_supported_native_abi()
                            })
                        })
                        .map_or(ProgramTypeAbiKind::Unsupported, |_| {
                            ProgramTypeAbiKind::DataReprCProduct
                        })
                })
        }
        kind => kind,
    }
}

#[must_use]
const fn is_supported_target() -> bool {
    cfg!(any(
        target_os = "macos",
        target_os = "linux",
        target_os = "windows"
    ))
}

fn unsupported_target_reason() -> Box<str> {
    format!("unsupported native host target `{OS}`").into_boxed_str()
}
