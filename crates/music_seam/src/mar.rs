use std::collections::BTreeSet;
use std::error::Error;
use std::fmt::{self, Display, Formatter};

pub const MAR_MAGIC: [u8; 4] = *b"MARS";
const MAR_BINARY_MAJOR_VERSION_U32: u32 = 1;
const MAR_BINARY_MINOR_VERSION_U32: u32 = 0;
pub const MAR_BINARY_MAJOR_VERSION: u16 = 1;
pub const MAR_BINARY_MINOR_VERSION: u16 = 0;
pub const MAR_BINARY_VERSION: u32 =
    (MAR_BINARY_MAJOR_VERSION_U32 << 16) | MAR_BINARY_MINOR_VERSION_U32;
pub const MAR_MAX_MODULES: u32 = 65_535;

pub type MarResult<T = ()> = Result<T, MarError>;
pub type MarModuleEntryList = Vec<MarModuleEntry>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarProfile {
    Debug,
    Release,
}

impl MarProfile {
    const fn to_wire_code(self) -> u8 {
        match self {
            Self::Debug => 0,
            Self::Release => 1,
        }
    }

    const fn from_wire_code(code: u8) -> MarResult<Self> {
        match code {
            0 => Ok(Self::Debug),
            1 => Ok(Self::Release),
            _ => Err(MarError::InvalidProfile(code)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MarPackageKind {
    Thin,
    Fat,
}

impl MarPackageKind {
    const fn to_wire_code(self) -> u8 {
        match self {
            Self::Thin => 0,
            Self::Fat => 1,
        }
    }

    const fn from_wire_code(code: u8) -> MarResult<Self> {
        match code {
            0 => Ok(Self::Thin),
            1 => Ok(Self::Fat),
            _ => Err(MarError::InvalidPackageKind(code)),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct MarOptimizationPolicy {
    pub scope: MarOptimizationScope,
    pub payload: MarOptimizationPayload,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct MarOptimizationScope {
    pub flatten_private_modules: bool,
    pub shrink_private_items: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct MarOptimizationPayload {
    pub mangle_private_names: bool,
    pub strip_debug_payloads: bool,
}

impl MarOptimizationPolicy {
    #[must_use]
    pub const fn release_fat() -> Self {
        Self {
            scope: MarOptimizationScope {
                flatten_private_modules: true,
                shrink_private_items: true,
            },
            payload: MarOptimizationPayload {
                mangle_private_names: true,
                strip_debug_payloads: true,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarManifest {
    pub package_name: Box<str>,
    pub package_version: Box<str>,
    pub entry_module: Option<Box<str>>,
    pub entry_export: Option<Box<str>>,
    pub profile: MarProfile,
    pub package_kind: MarPackageKind,
    pub optimization_policy: MarOptimizationPolicy,
}

impl MarManifest {
    #[must_use]
    pub fn new(
        package_name: impl Into<Box<str>>,
        package_version: impl Into<Box<str>>,
        profile: MarProfile,
        package_kind: MarPackageKind,
    ) -> Self {
        Self {
            package_name: package_name.into(),
            package_version: package_version.into(),
            entry_module: None,
            entry_export: None,
            profile,
            package_kind,
            optimization_policy: MarOptimizationPolicy::default(),
        }
    }

    #[must_use]
    pub fn with_entry_module(mut self, entry_module: impl Into<Box<str>>) -> Self {
        self.entry_module = Some(entry_module.into());
        self
    }

    #[must_use]
    pub fn with_entry_export(mut self, entry_export: impl Into<Box<str>>) -> Self {
        self.entry_export = Some(entry_export.into());
        self
    }

    #[must_use]
    pub const fn with_optimization_policy(
        mut self,
        optimization_policy: MarOptimizationPolicy,
    ) -> Self {
        self.optimization_policy = optimization_policy;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarModuleEntry {
    pub key: Box<str>,
    pub blob: Box<[u8]>,
}

impl MarModuleEntry {
    #[must_use]
    pub fn new(key: impl Into<Box<str>>, blob: impl Into<Box<[u8]>>) -> Self {
        Self {
            key: key.into(),
            blob: blob.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MarArchive {
    pub manifest: MarManifest,
    pub modules: MarModuleEntryList,
}

impl MarArchive {
    #[must_use]
    pub const fn new(manifest: MarManifest, modules: MarModuleEntryList) -> Self {
        Self { manifest, modules }
    }

    /// # Errors
    ///
    /// Returns [`MarError`] when module blobs, module keys, or entry-module references are invalid.
    pub fn validate(&self) -> MarResult {
        let module_count =
            u32::try_from(self.modules.len()).map_err(|_| MarError::LengthOverflow {
                field: "module table length",
            })?;
        if module_count > MAR_MAX_MODULES {
            return Err(MarError::TooManyModules {
                count: module_count,
            });
        }
        let mut seen = BTreeSet::new();
        for module in &self.modules {
            if module.blob.is_empty() {
                return Err(MarError::EmptyModuleBlob {
                    key: String::from(module.key.as_ref()),
                });
            }
            crate::validate_binary(&module.blob)
                .map_err(|source| invalid_module_blob_error(module.key.as_ref(), source))?;
            if !seen.insert(module.key.as_ref()) {
                return Err(MarError::DuplicateModuleKey {
                    key: String::from(module.key.as_ref()),
                });
            }
        }
        if let Some(entry_module) = self.manifest.entry_module.as_deref()
            && !seen.contains(entry_module)
        {
            return Err(MarError::MissingEntryModule {
                key: entry_module.to_owned(),
            });
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MarError {
    InvalidHeader,
    UnsupportedVersion(u32),
    PayloadTruncated,
    TrailingPayload,
    InvalidProfile(u8),
    InvalidPackageKind(u8),
    InvalidOptionalMarker { field: &'static str, marker: u8 },
    InvalidUtf8 { field: &'static str },
    LengthOverflow { field: &'static str },
    TooManyModules { count: u32 },
    DuplicateModuleKey { key: String },
    EmptyModuleBlob { key: String },
    InvalidModuleBlob { key: String, message: String },
    MissingEntryModule { key: String },
}

impl Display for MarError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidHeader => f.write_str("invalid mar archive header"),
            Self::UnsupportedVersion(version) => {
                write!(f, "unsupported mar archive version {version}")
            }
            Self::PayloadTruncated => f.write_str("truncated mar archive payload"),
            Self::TrailingPayload => f.write_str("trailing mar archive payload"),
            Self::InvalidProfile(profile) => write!(f, "invalid mar profile code {profile}"),
            Self::InvalidPackageKind(kind) => write!(f, "invalid mar package kind code {kind}"),
            Self::InvalidOptionalMarker { field, marker } => {
                write!(f, "invalid marker {marker} for optional field {field}")
            }
            Self::InvalidUtf8 { field } => write!(f, "invalid utf-8 text in field {field}"),
            Self::LengthOverflow { field } => write!(f, "length overflow for field {field}"),
            Self::TooManyModules { count } => write!(f, "mar module count `{count}` too large"),
            Self::DuplicateModuleKey { key } => write!(f, "duplicate module key `{key}`"),
            Self::EmptyModuleBlob { key } => write!(f, "empty module blob for key `{key}`"),
            Self::InvalidModuleBlob { key, message } => {
                write!(f, "invalid seam module blob for key `{key}`: {message}")
            }
            Self::MissingEntryModule { key } => {
                write!(f, "mar entry module `{key}` missing from module table")
            }
        }
    }
}

impl Error for MarError {}

fn invalid_module_blob_error(key: &str, source: impl Display) -> MarError {
    MarError::InvalidModuleBlob {
        key: key.to_owned(),
        message: format!("{source}"),
    }
}

/// Encodes a validated `.mar` archive image into bytes.
///
/// # Errors
///
/// Returns [`MarError`] if archive validation fails or an encoded field overflows.
pub fn encode_mar_archive(archive: &MarArchive) -> MarResult<Vec<u8>> {
    archive.validate()?;

    let mut out = Vec::new();
    out.extend_from_slice(&MAR_MAGIC);
    push_u16(&mut out, MAR_BINARY_MAJOR_VERSION);
    push_u16(&mut out, MAR_BINARY_MINOR_VERSION);
    encode_manifest(&mut out, &archive.manifest)?;
    push_len(&mut out, archive.modules.len(), "module table length")?;
    for module in &archive.modules {
        push_text(&mut out, module.key.as_ref(), "module key")?;
        push_bytes(&mut out, &module.blob, "module blob")?;
    }
    Ok(out)
}

/// Decodes a `.mar` archive image and validates archive invariants.
///
/// # Errors
///
/// Returns [`MarError`] if header/version, payload, utf-8 text, or archive invariants are invalid.
pub fn decode_mar_archive(bytes: &[u8]) -> MarResult<MarArchive> {
    let mut cursor = MarCursor::new(bytes);
    if cursor.read_array::<4>()? != MAR_MAGIC {
        return Err(MarError::InvalidHeader);
    }

    let major = cursor.read_u16()?;
    let minor = cursor.read_u16()?;
    if major != MAR_BINARY_MAJOR_VERSION || minor != MAR_BINARY_MINOR_VERSION {
        let version = (u32::from(major) << 16) | u32::from(minor);
        return Err(MarError::UnsupportedVersion(version));
    }

    let manifest = decode_manifest(&mut cursor)?;
    let module_count = cursor.read_u32()?;
    if module_count > MAR_MAX_MODULES {
        return Err(MarError::TooManyModules {
            count: module_count,
        });
    }
    let mut modules = Vec::new();
    for _ in 0..module_count {
        let key = cursor.read_text("module key")?;
        let blob = cursor.read_bytes("module blob")?;
        modules.push(MarModuleEntry::new(key, blob));
    }
    if !cursor.is_eof() {
        return Err(MarError::TrailingPayload);
    }

    let archive = MarArchive::new(manifest, modules);
    archive.validate()?;
    Ok(archive)
}

/// Validates a `.mar` archive binary blob by decoding and checking archive invariants.
///
/// # Errors
///
/// Returns [`MarError`] if decoding or validation fails.
pub fn validate_mar_archive(bytes: &[u8]) -> MarResult {
    let _ = decode_mar_archive(bytes)?;
    Ok(())
}

fn encode_manifest(out: &mut Vec<u8>, manifest: &MarManifest) -> MarResult {
    push_text(out, manifest.package_name.as_ref(), "package name")?;
    push_text(out, manifest.package_version.as_ref(), "package version")?;
    out.push(manifest.profile.to_wire_code());
    out.push(manifest.package_kind.to_wire_code());
    push_optional_text(out, manifest.entry_module.as_deref(), "entry module")?;
    push_optional_text(out, manifest.entry_export.as_deref(), "entry export")?;
    out.push(u8::from(
        manifest.optimization_policy.scope.flatten_private_modules,
    ));
    out.push(u8::from(
        manifest.optimization_policy.scope.shrink_private_items,
    ));
    out.push(u8::from(
        manifest.optimization_policy.payload.mangle_private_names,
    ));
    out.push(u8::from(
        manifest.optimization_policy.payload.strip_debug_payloads,
    ));
    Ok(())
}

fn decode_manifest(cursor: &mut MarCursor<'_>) -> MarResult<MarManifest> {
    let package_name = cursor.read_text("package name")?;
    let package_version = cursor.read_text("package version")?;
    let profile = MarProfile::from_wire_code(cursor.read_u8()?)?;
    let package_kind = MarPackageKind::from_wire_code(cursor.read_u8()?)?;
    let entry_module = cursor.read_optional_text("entry module")?;
    let entry_export = cursor.read_optional_text("entry export")?;
    let optimization_policy = MarOptimizationPolicy {
        scope: MarOptimizationScope {
            flatten_private_modules: cursor.read_u8()? != 0,
            shrink_private_items: cursor.read_u8()? != 0,
        },
        payload: MarOptimizationPayload {
            mangle_private_names: cursor.read_u8()? != 0,
            strip_debug_payloads: cursor.read_u8()? != 0,
        },
    };

    let mut manifest = MarManifest::new(package_name, package_version, profile, package_kind)
        .with_optimization_policy(optimization_policy);
    if let Some(entry_module) = entry_module {
        manifest = manifest.with_entry_module(entry_module);
    }
    if let Some(entry_export) = entry_export {
        manifest = manifest.with_entry_export(entry_export);
    }
    Ok(manifest)
}

fn push_u16(out: &mut Vec<u8>, value: u16) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_u32(out: &mut Vec<u8>, value: u32) {
    out.extend_from_slice(&value.to_le_bytes());
}

fn push_len(out: &mut Vec<u8>, len: usize, field: &'static str) -> MarResult {
    let len = u32::try_from(len).map_err(|_| MarError::LengthOverflow { field })?;
    push_u32(out, len);
    Ok(())
}

fn push_bytes(out: &mut Vec<u8>, bytes: &[u8], field: &'static str) -> MarResult {
    push_len(out, bytes.len(), field)?;
    out.extend_from_slice(bytes);
    Ok(())
}

fn push_text(out: &mut Vec<u8>, text: &str, field: &'static str) -> MarResult {
    push_bytes(out, text.as_bytes(), field)
}

fn push_optional_text(out: &mut Vec<u8>, text: Option<&str>, field: &'static str) -> MarResult {
    match text {
        Some(text) => {
            out.push(1);
            push_text(out, text, field)?;
        }
        None => out.push(0),
    }
    Ok(())
}

struct MarCursor<'bytes> {
    bytes: &'bytes [u8],
    offset: usize,
}

impl<'bytes> MarCursor<'bytes> {
    const fn new(bytes: &'bytes [u8]) -> Self {
        Self { bytes, offset: 0 }
    }

    const fn is_eof(&self) -> bool {
        self.offset >= self.bytes.len()
    }

    fn read_u8(&mut self) -> MarResult<u8> {
        let byte = self
            .bytes
            .get(self.offset)
            .copied()
            .ok_or(MarError::PayloadTruncated)?;
        self.offset = self.offset.saturating_add(1);
        Ok(byte)
    }

    fn read_u16(&mut self) -> MarResult<u16> {
        Ok(u16::from_le_bytes(self.read_array::<2>()?))
    }

    fn read_len(&mut self, field: &'static str) -> MarResult<usize> {
        usize::try_from(self.read_u32()?).map_err(|_| MarError::LengthOverflow { field })
    }

    fn read_u32(&mut self) -> MarResult<u32> {
        Ok(u32::from_le_bytes(self.read_array::<4>()?))
    }

    fn read_text(&mut self, field: &'static str) -> MarResult<String> {
        let bytes = self.read_bytes(field)?;
        String::from_utf8(bytes).map_err(|_| MarError::InvalidUtf8 { field })
    }

    fn read_optional_text(&mut self, field: &'static str) -> MarResult<Option<String>> {
        let marker = self.read_u8()?;
        match marker {
            0 => Ok(None),
            1 => Ok(Some(self.read_text(field)?)),
            _ => Err(MarError::InvalidOptionalMarker { field, marker }),
        }
    }

    fn read_bytes(&mut self, field: &'static str) -> MarResult<Vec<u8>> {
        let len = self.read_len(field)?;
        let end = self.offset.saturating_add(len);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(MarError::PayloadTruncated)?;
        self.offset = end;
        Ok(slice.to_vec())
    }

    fn read_array<const N: usize>(&mut self) -> MarResult<[u8; N]> {
        let end = self.offset.saturating_add(N);
        let slice = self
            .bytes
            .get(self.offset..end)
            .ok_or(MarError::PayloadTruncated)?;
        self.offset = end;
        let mut out = [0_u8; N];
        out.copy_from_slice(slice);
        Ok(out)
    }
}

#[cfg(test)]
mod tests;
