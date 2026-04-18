mod intrinsics;
mod modules;
mod types;

pub use intrinsics::{
    BuiltinIntrinsicDef, BuiltinIntrinsicId, BuiltinIntrinsicKind, BuiltinSafety, JitLowering,
    all_builtin_intrinsics, builtin_intrinsic_by_name, builtin_intrinsic_by_symbol,
    is_builtin_intrinsic_name, is_builtin_intrinsic_symbol,
};
pub use modules::{
    BuiltinModuleDef, BuiltinPackageFile, all_foundation_modules, all_std_package_files,
    foundation_module_by_spec, is_hidden_builtin_module,
};
pub use types::{
    BuiltinTypeDef, BuiltinTypeId, all_builtin_types, builtin_type_by_name, is_builtin_type_name,
};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
