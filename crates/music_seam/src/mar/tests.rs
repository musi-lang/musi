#![allow(clippy::panic, clippy::unwrap_used)]

use super::{
    MAR_BINARY_MAJOR_VERSION, MAR_BINARY_MINOR_VERSION, MAR_MAGIC, MAR_MAX_MODULES, MarArchive,
    MarError, MarManifest, MarModuleEntry, MarOptimizationPolicy, MarPackageKind, MarProfile,
    decode_mar_archive, encode_mar_archive, validate_mar_archive,
};
use crate::{Artifact, encode_binary};

fn seam_blob() -> Vec<u8> {
    encode_binary(&Artifact::default()).expect("empty seam artifact encodes")
}

fn sample_archive() -> MarArchive {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Debug,
        MarPackageKind::Fat,
    )
    .with_entry_module("main")
    .with_entry_export("main::start");
    let modules = vec![
        MarModuleEntry::new("main", seam_blob()),
        MarModuleEntry::new("dep/math", seam_blob()),
    ];
    MarArchive::new(manifest, modules)
}

#[test]
fn mar_archive_roundtrip_binary() {
    let archive = sample_archive();

    let bytes = encode_mar_archive(&archive).expect("encode mar archive");
    let decoded = decode_mar_archive(&bytes).expect("decode mar archive");

    assert_eq!(decoded, archive);
}

#[test]
fn mar_manifest_roundtrips_release_fat_optimization_policy() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Release,
        MarPackageKind::Fat,
    )
    .with_optimization_policy(MarOptimizationPolicy::release_fat());
    let archive = MarArchive::new(manifest, vec![MarModuleEntry::new("main", seam_blob())]);

    let bytes = encode_mar_archive(&archive).expect("encode mar archive");
    let decoded = decode_mar_archive(&bytes).expect("decode mar archive");

    assert_eq!(
        decoded.manifest.optimization_policy,
        MarOptimizationPolicy::release_fat()
    );
}

#[test]
fn mar_archive_rejects_duplicate_module_keys() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Debug,
        MarPackageKind::Thin,
    );
    let archive = MarArchive::new(
        manifest,
        vec![
            MarModuleEntry::new("main", seam_blob()),
            MarModuleEntry::new("main", seam_blob()),
        ],
    );

    let error = encode_mar_archive(&archive).expect_err("duplicate key should fail");
    assert!(matches!(error, MarError::DuplicateModuleKey { .. }));
}

#[test]
fn mar_archive_rejects_empty_module_blob() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Release,
        MarPackageKind::Thin,
    );
    let archive = MarArchive::new(manifest, vec![MarModuleEntry::new("main", Vec::new())]);

    let error = encode_mar_archive(&archive).expect_err("empty module blob should fail");
    assert!(matches!(error, MarError::EmptyModuleBlob { .. }));
}

#[test]
fn mar_archive_rejects_invalid_seam_module_blob() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Debug,
        MarPackageKind::Thin,
    );
    let archive = MarArchive::new(manifest, vec![MarModuleEntry::new("main", vec![1])]);

    let error = encode_mar_archive(&archive).expect_err("invalid seam blob should fail");
    assert!(matches!(error, MarError::InvalidModuleBlob { .. }));
}

#[test]
fn mar_archive_rejects_missing_entry_module() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Debug,
        MarPackageKind::Fat,
    )
    .with_entry_module("missing");
    let archive = MarArchive::new(manifest, vec![MarModuleEntry::new("main", seam_blob())]);

    let error = encode_mar_archive(&archive).expect_err("missing entry should fail");
    assert!(matches!(error, MarError::MissingEntryModule { .. }));
}

#[test]
fn mar_archive_rejects_header_mismatch() {
    let archive = sample_archive();
    let mut bytes = encode_mar_archive(&archive).expect("encode mar archive");
    bytes[0] = b'X';

    let error = decode_mar_archive(&bytes).expect_err("invalid header should fail");
    assert_eq!(error, MarError::InvalidHeader);
}

#[test]
fn mar_archive_rejects_unsupported_version() {
    let archive = sample_archive();
    let mut bytes = encode_mar_archive(&archive).expect("encode mar archive");
    bytes[4..6].copy_from_slice(&MAR_BINARY_MAJOR_VERSION.saturating_add(1).to_le_bytes());
    bytes[6..8].copy_from_slice(&MAR_BINARY_MINOR_VERSION.to_le_bytes());

    let error = decode_mar_archive(&bytes).expect_err("unsupported version should fail");
    assert!(matches!(error, MarError::UnsupportedVersion(_)));
}

#[test]
fn validate_mar_archive_matches_decode_path() {
    let archive = sample_archive();
    let bytes = encode_mar_archive(&archive).expect("encode mar archive");

    assert!(validate_mar_archive(&bytes).is_ok());
    assert_eq!(&bytes[0..4], &MAR_MAGIC);
}

#[test]
fn mar_archive_rejects_excessive_module_count_before_allocating() {
    let manifest = MarManifest::new(
        "pkg://musi/app",
        "0.1.0",
        MarProfile::Debug,
        MarPackageKind::Fat,
    );
    let archive = MarArchive::new(manifest, vec![MarModuleEntry::new("main", seam_blob())]);
    let mut bytes = encode_mar_archive(&archive).expect("encode mar archive");
    let count_offset = MAR_MAGIC.len()
        + 2
        + 2
        + 4
        + "pkg://musi/app".len()
        + 4
        + "0.1.0".len()
        + 1
        + 1
        + 1
        + 1
        + 4;
    bytes[count_offset..count_offset + 4]
        .copy_from_slice(&MAR_MAX_MODULES.saturating_add(1).to_le_bytes());

    let error = decode_mar_archive(&bytes).expect_err("large module count should fail");
    assert!(matches!(error, MarError::TooManyModules { .. }));
}
