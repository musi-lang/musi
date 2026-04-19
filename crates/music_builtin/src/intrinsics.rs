#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BuiltinIntrinsicId {
    DataTag,
    CompareFloatTotal,
    FfiPtrNull,
    FfiPtrIsNull,
    FfiPtrOffset,
    FfiPtrRead,
    FfiPtrWrite,
    SysTargetOs,
    SysTargetArch,
    SysTargetArchFamily,
    SysTargetFamily,
    SysTargetPointerWidth,
    SysTargetEndian,
    SysJitSupported,
    SysJitBackend,
    SysJitIsa,
    SysMatchesOs,
    SysMatchesArch,
    SysMatchesFamily,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinIntrinsicKind {
    Data,
    Numeric,
    Pointer,
    Target,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltinSafety {
    Safe,
    Unsafe,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JitLowering {
    CraneliftOpcode(&'static str),
    CraneliftTrap(&'static str),
    RuntimeCall(&'static str),
    VmOnly,
    UnsupportedForJit(&'static str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinIntrinsicDef {
    pub id: BuiltinIntrinsicId,
    pub name: &'static str,
    pub symbol: &'static str,
    pub kind: BuiltinIntrinsicKind,
    pub safety: BuiltinSafety,
    pub jit: JitLowering,
}

impl BuiltinIntrinsicDef {
    const fn new(
        id: BuiltinIntrinsicId,
        name: &'static str,
        symbol: &'static str,
        kind: BuiltinIntrinsicKind,
        safety: BuiltinSafety,
        jit: JitLowering,
    ) -> Self {
        Self {
            id,
            name,
            symbol,
            kind,
            safety,
            jit,
        }
    }
}

pub const BUILTIN_INTRINSICS: &[BuiltinIntrinsicDef] = &[
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::DataTag,
        "dataTag",
        "data.tag",
        BuiltinIntrinsicKind::Data,
        BuiltinSafety::Safe,
        JitLowering::RuntimeCall("data.tag"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::CompareFloatTotal,
        "compareFloatTotal",
        "compare.float.total",
        BuiltinIntrinsicKind::Numeric,
        BuiltinSafety::Safe,
        JitLowering::RuntimeCall("compare.float.total"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::FfiPtrNull,
        "ffiPtrNull",
        "ffi.ptr.null",
        BuiltinIntrinsicKind::Pointer,
        BuiltinSafety::Unsafe,
        JitLowering::CraneliftOpcode("iconst_0"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::FfiPtrIsNull,
        "ffiPtrIsNull",
        "ffi.ptr.is_null",
        BuiltinIntrinsicKind::Pointer,
        BuiltinSafety::Unsafe,
        JitLowering::CraneliftOpcode("icmp_imm.eqz"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::FfiPtrOffset,
        "ffiPtrOffset",
        "ffi.ptr.offset",
        BuiltinIntrinsicKind::Pointer,
        BuiltinSafety::Unsafe,
        JitLowering::CraneliftTrap("pointer offset overflow"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::FfiPtrRead,
        "ffiPtrRead",
        "ffi.ptr.read",
        BuiltinIntrinsicKind::Pointer,
        BuiltinSafety::Unsafe,
        JitLowering::CraneliftTrap("pointer read fault"),
    ),
    BuiltinIntrinsicDef::new(
        BuiltinIntrinsicId::FfiPtrWrite,
        "ffiPtrWrite",
        "ffi.ptr.write",
        BuiltinIntrinsicKind::Pointer,
        BuiltinSafety::Unsafe,
        JitLowering::CraneliftTrap("pointer write fault"),
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetOs,
        "sysTargetOs",
        "sys.target.os",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetArch,
        "sysTargetArch",
        "sys.target.arch",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetArchFamily,
        "sysTargetArchFamily",
        "sys.target.arch_family",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetFamily,
        "sysTargetFamily",
        "sys.target.family",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetPointerWidth,
        "sysTargetPointerWidth",
        "sys.target.pointer_width",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysTargetEndian,
        "sysTargetEndian",
        "sys.target.endian",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysJitSupported,
        "sysJitSupported",
        "sys.jit.supported",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysJitBackend,
        "sysJitBackend",
        "sys.jit.backend",
    ),
    sys_intrinsic(BuiltinIntrinsicId::SysJitIsa, "sysJitIsa", "sys.jit.isa"),
    sys_intrinsic(
        BuiltinIntrinsicId::SysMatchesOs,
        "sysMatchesOs",
        "sys.matches.os",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysMatchesArch,
        "sysMatchesArch",
        "sys.matches.arch",
    ),
    sys_intrinsic(
        BuiltinIntrinsicId::SysMatchesFamily,
        "sysMatchesFamily",
        "sys.matches.family",
    ),
];

const fn sys_intrinsic(
    id: BuiltinIntrinsicId,
    name: &'static str,
    symbol: &'static str,
) -> BuiltinIntrinsicDef {
    BuiltinIntrinsicDef::new(
        id,
        name,
        symbol,
        BuiltinIntrinsicKind::Target,
        BuiltinSafety::Safe,
        JitLowering::RuntimeCall(symbol),
    )
}

#[must_use]
pub const fn all_builtin_intrinsics() -> &'static [BuiltinIntrinsicDef] {
    BUILTIN_INTRINSICS
}

#[must_use]
pub fn builtin_intrinsic_by_name(name: &str) -> Option<&'static BuiltinIntrinsicDef> {
    BUILTIN_INTRINSICS.iter().find(|def| def.name == name)
}

#[must_use]
pub fn builtin_intrinsic_by_symbol(symbol: &str) -> Option<&'static BuiltinIntrinsicDef> {
    let base_symbol = pointer_storage_base_symbol(symbol);
    BUILTIN_INTRINSICS
        .iter()
        .find(|def| def.symbol == base_symbol)
}

#[must_use]
pub fn is_builtin_intrinsic_name(name: &str) -> bool {
    builtin_intrinsic_by_name(name).is_some()
}

#[must_use]
pub fn is_builtin_intrinsic_symbol(symbol: &str) -> bool {
    builtin_intrinsic_by_symbol(symbol).is_some()
}

fn pointer_storage_base_symbol(symbol: &str) -> &str {
    if symbol.starts_with("ffi.ptr.offset.") {
        "ffi.ptr.offset"
    } else if symbol.starts_with("ffi.ptr.read.") {
        "ffi.ptr.read"
    } else if symbol.starts_with("ffi.ptr.write.") {
        "ffi.ptr.write"
    } else {
        symbol
    }
}
