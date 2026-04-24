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
    BuiltinPackageFile::new("assert.ms"),
    BuiltinPackageFile::new("bits.ms"),
    BuiltinPackageFile::new("bytes.ms"),
    BuiltinPackageFile::new("cli.ms"),
    BuiltinPackageFile::new("cli/prompt.ms"),
    BuiltinPackageFile::new("cmp.ms"),
    BuiltinPackageFile::new("collections.ms"),
    BuiltinPackageFile::new("collections/array.ms"),
    BuiltinPackageFile::new("collections/iter.ms"),
    BuiltinPackageFile::new("collections/list.ms"),
    BuiltinPackageFile::new("collections/slice.ms"),
    BuiltinPackageFile::new("crypto.ms"),
    BuiltinPackageFile::new("datetime.ms"),
    BuiltinPackageFile::new("encoding.ms"),
    BuiltinPackageFile::new("encoding/base64.ms"),
    BuiltinPackageFile::new("encoding/hex.ms"),
    BuiltinPackageFile::new("encoding/utf8.ms"),
    BuiltinPackageFile::new("env.ms"),
    BuiltinPackageFile::new("errors.ms"),
    BuiltinPackageFile::new("ffi.ms"),
    BuiltinPackageFile::new("fmt.ms"),
    BuiltinPackageFile::new("fs.ms"),
    BuiltinPackageFile::new("io.ms"),
    BuiltinPackageFile::new("json.ms"),
    BuiltinPackageFile::new("log.ms"),
    BuiltinPackageFile::new("math.ms"),
    BuiltinPackageFile::new("math/float.ms"),
    BuiltinPackageFile::new("math/integer.ms"),
    BuiltinPackageFile::new("option.ms"),
    BuiltinPackageFile::new("os.ms"),
    BuiltinPackageFile::new("path.ms"),
    BuiltinPackageFile::new("prelude.ms"),
    BuiltinPackageFile::new("process.ms"),
    BuiltinPackageFile::new("random.ms"),
    BuiltinPackageFile::new("result.ms"),
    BuiltinPackageFile::new("semver.ms"),
    BuiltinPackageFile::new("std.ms"),
    BuiltinPackageFile::new("sys.ms"),
    BuiltinPackageFile::new("testing.ms"),
    BuiltinPackageFile::new("text.ms"),
    BuiltinPackageFile::new("uuid.ms"),
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
