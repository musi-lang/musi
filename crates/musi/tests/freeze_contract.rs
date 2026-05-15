#[cfg(test)]
mod freeze_contract {
    use std::collections::BTreeSet;
    use std::fs;
    use std::path::{Path, PathBuf};

    use sha2::{Digest, Sha256};

    type FreezeFileFingerprintList = Vec<FreezeFileFingerprint>;

    const REQUIRED_CANONICAL_PATHS: [&str; 20] = [
        "grammar/MusiParser.g4",
        "grammar/MusiLexer.g4",
        "grammar/Musi.abnf",
        "specs/language/first-class-everything.md",
        "specs/language/items-and-attributes.md",
        "specs/language/syntax.md",
        "specs/language/type-core.md",
        "specs/language/contextual-capabilities.md",
        "specs/language/module-boundaries.md",
        "specs/language/yield-and-capabilities.md",
        "specs/seam/bytecode.md",
        "specs/seam/lowering.md",
        "specs/seam/domains.md",
        "specs/seam/format.md",
        "docs/__smallcore__/musi-small-core-frozen-system.md",
        "docs/__smallcore__/seam-00-index-and-principles.md",
        "docs/__smallcore__/seam-01-bytecode-and-stack-effects.md",
        "docs/__smallcore__/seam-02-calls-objects-and-layouts.md",
        "docs/__smallcore__/seam-03-runtime-gc-pinning-yield-defer.md",
        "docs/__smallcore__/seam-04-external-artifacts-decomp-mar.md",
    ];

    #[derive(Debug)]
    struct FreezeManifest {
        manifest_version: u32,
        files: FreezeFileFingerprintList,
    }

    #[derive(Debug)]
    struct FreezeFileFingerprint {
        path: String,
        sha256: String,
    }

    #[derive(Default)]
    struct PendingFileFingerprint {
        path: Option<String>,
        sha256: Option<String>,
    }

    impl PendingFileFingerprint {
        fn finish(self, line_number: usize) -> Result<FreezeFileFingerprint, String> {
            let path = self
                .path
                .ok_or_else(|| format!("line {line_number}: missing `path` in [[files]] entry"))?;
            let sha256 = self.sha256.ok_or_else(|| {
                format!("line {line_number}: missing `sha256` in [[files]] entry")
            })?;
            Ok(FreezeFileFingerprint { path, sha256 })
        }
    }

    #[test]
    fn frozen_syntax_and_bytecode_surfaces_match_manifest_fingerprints() {
        let workspace_root = workspace_root();
        let manifest_path = workspace_root.join("docs/__smallcore__/freeze-manifest.toml");
        let manifest_result = load_manifest(&manifest_path);
        assert!(
            manifest_result.is_ok(),
            "failed to load freeze manifest `{}`: {}",
            display_path(&manifest_path),
            manifest_result
                .as_ref()
                .err()
                .map_or("unknown error", String::as_str)
        );
        let Ok(manifest) = manifest_result else {
            return;
        };

        assert_eq!(
            manifest.manifest_version, 1,
            "unsupported freeze manifest version `{}`",
            manifest.manifest_version
        );
        assert_required_canonical_path_set(&manifest);

        let mut mismatch_list = Vec::new();
        for file in &manifest.files {
            let canonical_path = workspace_root.join(&file.path);
            let display = display_relative_path(&workspace_root, &canonical_path);
            let bytes = match fs::read(&canonical_path) {
                Ok(bytes) => bytes,
                Err(error) => {
                    mismatch_list.push(format!("{display}: read failed ({error})"));
                    continue;
                }
            };
            let actual_sha256 = sha256_hex(&bytes);
            if actual_sha256 != file.sha256 {
                mismatch_list.push(format!(
                    "{display}: expected {}, actual {}",
                    file.sha256, actual_sha256
                ));
            }
        }

        assert!(
            mismatch_list.is_empty(),
            "freeze fingerprint mismatch detected:\n{}\n\n\
        Remediation:\n\
        1. Confirm whether canonical-file changes are intentional and approved.\n\
        2. Recompute hashes and update docs/__smallcore__/freeze-manifest.toml.\n\
        3. Add command evidence path to docs/__smallcore__/checkpoint-log.md.\n\
        4. Re-run `rtk cargo test -p musi --test freeze_contract`.\n\
        See docs/__smallcore__/freeze-policy.md for the full workflow.",
            mismatch_list.join("\n")
        );
    }

    fn load_manifest(path: &Path) -> Result<FreezeManifest, String> {
        let raw = fs::read_to_string(path)
            .map_err(|error| format!("unable to read {} ({error})", display_path(path)))?;
        parse_manifest(&raw)
    }

    fn parse_manifest(raw: &str) -> Result<FreezeManifest, String> {
        let mut manifest_version = None;
        let mut file_list = FreezeFileFingerprintList::new();
        let mut pending: Option<PendingFileFingerprint> = None;
        let mut seen_paths = BTreeSet::new();

        for (index, raw_line) in raw.lines().enumerate() {
            let line_number = index + 1;
            let line = raw_line.trim();
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            if line == "[[files]]" {
                if let Some(entry) = pending.take() {
                    let file = entry.finish(line_number)?;
                    register_file(&mut file_list, &mut seen_paths, file, line_number)?;
                }
                pending = Some(PendingFileFingerprint::default());
                continue;
            }

            let (key, value) = line
                .split_once('=')
                .ok_or_else(|| format!("line {line_number}: expected `key = value`"))?;
            let key = key.trim();
            let value = value.trim();

            if key == "manifest_version" {
                if pending.is_some() {
                    return Err(format!(
                        "line {line_number}: `manifest_version` must appear before [[files]] entries"
                    ));
                }
                if manifest_version.is_some() {
                    return Err(format!("line {line_number}: duplicate `manifest_version`"));
                }
                manifest_version = Some(parse_manifest_version(value, line_number)?);
                continue;
            }

            let Some(entry) = pending.as_mut() else {
                return Err(format!(
                    "line {line_number}: key `{key}` must appear inside a [[files]] entry"
                ));
            };

            match key {
                "path" => {
                    if entry.path.is_some() {
                        return Err(format!(
                            "line {line_number}: duplicate `path` in [[files]] entry"
                        ));
                    }
                    entry.path = Some(parse_string_literal(value, line_number, "path")?);
                }
                "sha256" => {
                    if entry.sha256.is_some() {
                        return Err(format!(
                            "line {line_number}: duplicate `sha256` in [[files]] entry"
                        ));
                    }
                    let digest = parse_string_literal(value, line_number, "sha256")?;
                    validate_sha256(&digest, line_number)?;
                    entry.sha256 = Some(digest);
                }
                _ => {
                    return Err(format!(
                        "line {line_number}: unsupported key `{key}` in [[files]] entry"
                    ));
                }
            }
        }

        if let Some(entry) = pending.take() {
            let file = entry.finish(raw.lines().count() + 1)?;
            register_file(
                &mut file_list,
                &mut seen_paths,
                file,
                raw.lines().count() + 1,
            )?;
        }

        let manifest_version = manifest_version
            .ok_or_else(|| "missing `manifest_version` in freeze manifest".to_owned())?;
        if file_list.is_empty() {
            return Err("freeze manifest requires at least one [[files]] entry".to_owned());
        }

        Ok(FreezeManifest {
            manifest_version,
            files: file_list,
        })
    }

    fn assert_required_canonical_path_set(manifest: &FreezeManifest) {
        let required_paths = REQUIRED_CANONICAL_PATHS
            .into_iter()
            .map(str::to_owned)
            .collect::<BTreeSet<String>>();
        let manifest_paths = manifest
            .files
            .iter()
            .map(|file| file.path.clone())
            .collect::<BTreeSet<String>>();

        let missing_paths = required_paths
            .difference(&manifest_paths)
            .cloned()
            .collect::<Vec<String>>();
        let unexpected_paths = manifest_paths
            .difference(&required_paths)
            .cloned()
            .collect::<Vec<String>>();
        assert!(
            missing_paths.is_empty() && unexpected_paths.is_empty(),
            "freeze manifest canonical set mismatch:\n\
        missing paths: {}\n\
        unexpected paths: {}\n\n\
        Remediation:\n\
        1. Align docs/__smallcore__/freeze-policy.md canonical list.\n\
        2. Update docs/__smallcore__/freeze-manifest.toml [[files]] entries to exactly match the policy set.\n\
        3. Re-run `rtk cargo test -p musi --test freeze_contract`.",
            format_path_list(&missing_paths),
            format_path_list(&unexpected_paths),
        );
    }

    fn register_file(
        file_list: &mut FreezeFileFingerprintList,
        seen_paths: &mut BTreeSet<String>,
        file: FreezeFileFingerprint,
        line_number: usize,
    ) -> Result<(), String> {
        if !seen_paths.insert(file.path.clone()) {
            return Err(format!(
                "line {line_number}: duplicate path `{}` in freeze manifest",
                file.path
            ));
        }
        file_list.push(file);
        Ok(())
    }

    fn parse_manifest_version(raw: &str, line_number: usize) -> Result<u32, String> {
        raw.parse::<u32>()
            .map_err(|error| format!("line {line_number}: invalid `manifest_version` ({error})"))
    }

    fn parse_string_literal(raw: &str, line_number: usize, key: &str) -> Result<String, String> {
        let Some(without_prefix) = raw.strip_prefix('"') else {
            return Err(format!(
                "line {line_number}: `{key}` must be a double-quoted string"
            ));
        };
        let Some(inner) = without_prefix.strip_suffix('"') else {
            return Err(format!(
                "line {line_number}: `{key}` must be a double-quoted string"
            ));
        };
        Ok(inner.to_owned())
    }

    fn validate_sha256(digest: &str, line_number: usize) -> Result<(), String> {
        if digest.len() != 64 || !digest.chars().all(|ch| ch.is_ascii_hexdigit()) {
            return Err(format!(
                "line {line_number}: `sha256` must be a 64-character hexadecimal string"
            ));
        }
        Ok(())
    }

    fn workspace_root() -> PathBuf {
        let crate_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        crate_dir
            .parent()
            .and_then(Path::parent)
            .expect("workspace root should be two directories above crate manifest")
            .to_path_buf()
    }

    fn sha256_hex(bytes: &[u8]) -> String {
        let digest = Sha256::digest(bytes);
        let mut output = String::with_capacity(digest.len() * 2);
        for byte in digest {
            use std::fmt::Write as _;
            write!(&mut output, "{byte:02x}").expect("writing to String should not fail");
        }
        output
    }

    fn display_relative_path(workspace_root: &Path, path: &Path) -> String {
        path.strip_prefix(workspace_root)
            .unwrap_or(path)
            .display()
            .to_string()
    }

    fn display_path(path: &Path) -> String {
        path.display().to_string()
    }

    fn format_path_list(path_list: &[String]) -> String {
        if path_list.is_empty() {
            "(none)".to_owned()
        } else {
            path_list.join(", ")
        }
    }
}
