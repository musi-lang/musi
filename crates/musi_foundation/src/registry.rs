use music_builtin::foundation_module_by_spec;
use music_module::{ImportMap, ModuleKey};
use music_session::{Session, SessionError};

struct FoundationModuleDef {
    spec: &'static str,
    source: &'static str,
}

const FOUNDATION_MODULES: &[FoundationModuleDef] = &[
    FoundationModuleDef {
        spec: "musi:core",
        source: include_str!("../modules/core.ms"),
    },
    FoundationModuleDef {
        spec: "musi:intrinsics",
        source: include_str!("../modules/_intrinsics.ms"),
    },
    FoundationModuleDef {
        spec: "musi:env",
        source: include_str!("../modules/_env.ms"),
    },
    FoundationModuleDef {
        spec: "musi:ffi",
        source: include_str!("../modules/ffi.ms"),
    },
    FoundationModuleDef {
        spec: "musi:process",
        source: include_str!("../modules/_process.ms"),
    },
    FoundationModuleDef {
        spec: "musi:io",
        source: include_str!("../modules/_io.ms"),
    },
    FoundationModuleDef {
        spec: "musi:fs",
        source: include_str!("../modules/_fs.ms"),
    },
    FoundationModuleDef {
        spec: "musi:time",
        source: include_str!("../modules/_time.ms"),
    },
    FoundationModuleDef {
        spec: "musi:random",
        source: include_str!("../modules/_random.ms"),
    },
    FoundationModuleDef {
        spec: "musi:text",
        source: include_str!("../modules/_text.ms"),
    },
    FoundationModuleDef {
        spec: "musi:json",
        source: include_str!("../modules/_json.ms"),
    },
    FoundationModuleDef {
        spec: "musi:encoding",
        source: include_str!("../modules/_encoding.ms"),
    },
    FoundationModuleDef {
        spec: "musi:fmt",
        source: include_str!("../modules/_fmt.ms"),
    },
    FoundationModuleDef {
        spec: "musi:crypto",
        source: include_str!("../modules/_crypto.ms"),
    },
    FoundationModuleDef {
        spec: "musi:uuid",
        source: include_str!("../modules/_uuid.ms"),
    },
    FoundationModuleDef {
        spec: "musi:test",
        source: include_str!("../modules/test.ms"),
    },
    FoundationModuleDef {
        spec: "musi:syntax",
        source: include_str!("../modules/syntax.ms"),
    },
];

#[must_use]
#[cfg(test)]
pub fn registered_specs() -> Vec<&'static str> {
    FOUNDATION_MODULES
        .iter()
        .map(|module| module.spec)
        .collect()
}

pub fn extend_import_map(import_map: &mut ImportMap) {
    for module in FOUNDATION_MODULES {
        if foundation_module_by_spec(module.spec).is_some_and(|module| module.hidden) {
            continue;
        }
        let _ = import_map
            .imports
            .insert(module.spec.into(), module.spec.into());
    }
}

#[must_use]
pub fn resolve_spec(spec: &str) -> Option<ModuleKey> {
    if foundation_module_by_spec(spec).is_some_and(|module| module.hidden) {
        return None;
    }
    module_source(spec).map(|_| ModuleKey::new(spec))
}

#[must_use]
pub fn resolve_public_spec(spec: &str) -> Option<ModuleKey> {
    if !is_public_import_spec(spec) {
        return None;
    }
    resolve_spec(spec)
}

#[must_use]
pub fn is_public_import_spec(spec: &str) -> bool {
    matches!(spec, "musi:core" | "musi:ffi" | "musi:test" | "musi:syntax")
}

#[must_use]
pub fn module_source(spec: &str) -> Option<&'static str> {
    FOUNDATION_MODULES
        .iter()
        .find_map(|module| (spec == module.spec).then_some(module.source))
}

/// # Errors
///
/// Returns [`SessionError`] if any foundation module cannot be interned into the session source map.
pub fn register_modules(session: &mut Session) -> Result<(), SessionError> {
    for module in FOUNDATION_MODULES {
        session.set_module_text(&ModuleKey::new(module.spec), module.source.to_owned())?;
    }
    Ok(())
}
