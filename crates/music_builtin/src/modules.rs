#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinModuleDef {
    pub spec: &'static str,
    pub hidden: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct BuiltinPackageFile {
    pub path: &'static str,
}

pub const FOUNDATION_MODULES: &[BuiltinModuleDef] = &[
    BuiltinModuleDef::new("musi:core", false),
    BuiltinModuleDef::new("musi:intrinsics", true),
    BuiltinModuleDef::new("musi:env", false),
    BuiltinModuleDef::new("musi:ffi", false),
    BuiltinModuleDef::new("musi:process", false),
    BuiltinModuleDef::new("musi:io", false),
    BuiltinModuleDef::new("musi:fs", false),
    BuiltinModuleDef::new("musi:time", false),
    BuiltinModuleDef::new("musi:random", false),
    BuiltinModuleDef::new("musi:text", false),
    BuiltinModuleDef::new("musi:path", false),
    BuiltinModuleDef::new("musi:json", false),
    BuiltinModuleDef::new("musi:encoding", false),
    BuiltinModuleDef::new("musi:fmt", false),
    BuiltinModuleDef::new("musi:crypto", false),
    BuiltinModuleDef::new("musi:uuid", false),
    BuiltinModuleDef::new("musi:log", false),
    BuiltinModuleDef::new("musi:test", false),
    BuiltinModuleDef::new("musi:syntax", false),
];
pub const STD_PACKAGE_FILES: &[BuiltinPackageFile] = &[
    BuiltinPackageFile::new("assert/index.ms"),
    BuiltinPackageFile::new("bytes/core.ms"),
    BuiltinPackageFile::new("bytes/index.ms"),
    BuiltinPackageFile::new("cli/index.ms"),
    BuiltinPackageFile::new("cli/prompt.ms"),
    BuiltinPackageFile::new("cmp/core.ms"),
    BuiltinPackageFile::new("cmp/index.ms"),
    BuiltinPackageFile::new("collections/array/core.ms"),
    BuiltinPackageFile::new("collections/array/index.ms"),
    BuiltinPackageFile::new("collections/index.ms"),
    BuiltinPackageFile::new("collections/iter/core.ms"),
    BuiltinPackageFile::new("collections/iter/index.ms"),
    BuiltinPackageFile::new("collections/list/core.ms"),
    BuiltinPackageFile::new("collections/list/index.ms"),
    BuiltinPackageFile::new("collections/slice/core.ms"),
    BuiltinPackageFile::new("collections/slice/index.ms"),
    BuiltinPackageFile::new("crypto/index.ms"),
    BuiltinPackageFile::new("datetime/core.ms"),
    BuiltinPackageFile::new("datetime/index.ms"),
    BuiltinPackageFile::new("encoding/base64/index.ms"),
    BuiltinPackageFile::new("encoding/hex/index.ms"),
    BuiltinPackageFile::new("encoding/index.ms"),
    BuiltinPackageFile::new("encoding/utf8/index.ms"),
    BuiltinPackageFile::new("env/index.ms"),
    BuiltinPackageFile::new("errors/index.ms"),
    BuiltinPackageFile::new("ffi/index.ms"),
    BuiltinPackageFile::new("fmt/index.ms"),
    BuiltinPackageFile::new("fs/index.ms"),
    BuiltinPackageFile::new("index.ms"),
    BuiltinPackageFile::new("io/index.ms"),
    BuiltinPackageFile::new("json/index.ms"),
    BuiltinPackageFile::new("log/index.ms"),
    BuiltinPackageFile::new("math/_float.ms"),
    BuiltinPackageFile::new("math/_int.ms"),
    BuiltinPackageFile::new("math/index.ms"),
    BuiltinPackageFile::new("option/core.ms"),
    BuiltinPackageFile::new("option/index.ms"),
    BuiltinPackageFile::new("os/core.ms"),
    BuiltinPackageFile::new("os/index.ms"),
    BuiltinPackageFile::new("path/core.ms"),
    BuiltinPackageFile::new("path/index.ms"),
    BuiltinPackageFile::new("prelude/index.ms"),
    BuiltinPackageFile::new("process/index.ms"),
    BuiltinPackageFile::new("random/index.ms"),
    BuiltinPackageFile::new("result/core.ms"),
    BuiltinPackageFile::new("result/index.ms"),
    BuiltinPackageFile::new("semver/index.ms"),
    BuiltinPackageFile::new("sys/index.ms"),
    BuiltinPackageFile::new("testing/index.ms"),
    BuiltinPackageFile::new("text/index.ms"),
    BuiltinPackageFile::new("text/string.ms"),
    BuiltinPackageFile::new("uuid/index.ms"),
];

impl BuiltinModuleDef {
    const fn new(spec: &'static str, hidden: bool) -> Self {
        Self { spec, hidden }
    }
}

impl BuiltinPackageFile {
    const fn new(path: &'static str) -> Self {
        Self { path }
    }
}

#[must_use]
pub const fn all_foundation_modules() -> &'static [BuiltinModuleDef] {
    FOUNDATION_MODULES
}

#[must_use]
pub const fn all_std_package_files() -> &'static [BuiltinPackageFile] {
    STD_PACKAGE_FILES
}

#[must_use]
pub fn foundation_module_by_spec(spec: &str) -> Option<&'static BuiltinModuleDef> {
    FOUNDATION_MODULES.iter().find(|def| def.spec == spec)
}

#[must_use]
pub fn is_hidden_builtin_module(spec: &str) -> bool {
    foundation_module_by_spec(spec).is_some_and(|def| def.hidden)
}
