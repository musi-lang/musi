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
    BuiltinModuleDef::new("musi:runtime", false),
    BuiltinModuleDef::new("musi:test", false),
    BuiltinModuleDef::new("musi:syntax", false),
];

pub const STD_PACKAGE_FILES: &[BuiltinPackageFile] = &[
    BuiltinPackageFile::new("array/_core.ms"),
    BuiltinPackageFile::new("array/index.ms"),
    BuiltinPackageFile::new("assert/index.ms"),
    BuiltinPackageFile::new("bytes/_core.ms"),
    BuiltinPackageFile::new("bytes/index.ms"),
    BuiltinPackageFile::new("cmp/_core.ms"),
    BuiltinPackageFile::new("cmp/index.ms"),
    BuiltinPackageFile::new("encoding/base64/index.ms"),
    BuiltinPackageFile::new("encoding/hex/index.ms"),
    BuiltinPackageFile::new("encoding/index.ms"),
    BuiltinPackageFile::new("encoding/utf8/index.ms"),
    BuiltinPackageFile::new("env/index.ms"),
    BuiltinPackageFile::new("ffi/index.ms"),
    BuiltinPackageFile::new("fs/index.ms"),
    BuiltinPackageFile::new("index.ms"),
    BuiltinPackageFile::new("io/index.ms"),
    BuiltinPackageFile::new("io/prompt.ms"),
    BuiltinPackageFile::new("iter/_core.ms"),
    BuiltinPackageFile::new("iter/index.ms"),
    BuiltinPackageFile::new("json/index.ms"),
    BuiltinPackageFile::new("list/_core.ms"),
    BuiltinPackageFile::new("list/index.ms"),
    BuiltinPackageFile::new("log/index.ms"),
    BuiltinPackageFile::new("math/_float.ms"),
    BuiltinPackageFile::new("math/_int.ms"),
    BuiltinPackageFile::new("math/index.ms"),
    BuiltinPackageFile::new("option/_core.ms"),
    BuiltinPackageFile::new("option/index.ms"),
    BuiltinPackageFile::new("os/_core.ms"),
    BuiltinPackageFile::new("os/index.ms"),
    BuiltinPackageFile::new("path/_core.ms"),
    BuiltinPackageFile::new("path/index.ms"),
    BuiltinPackageFile::new("prelude/index.ms"),
    BuiltinPackageFile::new("process/index.ms"),
    BuiltinPackageFile::new("random/index.ms"),
    BuiltinPackageFile::new("result/_core.ms"),
    BuiltinPackageFile::new("result/index.ms"),
    BuiltinPackageFile::new("slice/_core.ms"),
    BuiltinPackageFile::new("slice/index.ms"),
    BuiltinPackageFile::new("sys/index.ms"),
    BuiltinPackageFile::new("testing/index.ms"),
    BuiltinPackageFile::new("text/_string.ms"),
    BuiltinPackageFile::new("text/index.ms"),
    BuiltinPackageFile::new("time/_core.ms"),
    BuiltinPackageFile::new("time/index.ms"),
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
