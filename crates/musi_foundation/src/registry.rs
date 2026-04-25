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
        source: include_str!("../modules/intrinsics.ms"),
    },
    FoundationModuleDef {
        spec: "musi:env",
        source: include_str!("../modules/env.ms"),
    },
    FoundationModuleDef {
        spec: "musi:ffi",
        source: include_str!("../modules/ffi.ms"),
    },
    FoundationModuleDef {
        spec: "musi:process",
        source: include_str!("../modules/process.ms"),
    },
    FoundationModuleDef {
        spec: "musi:io",
        source: include_str!("../modules/io.ms"),
    },
    FoundationModuleDef {
        spec: "musi:fs",
        source: include_str!("../modules/fs.ms"),
    },
    FoundationModuleDef {
        spec: "musi:time",
        source: include_str!("../modules/time.ms"),
    },
    FoundationModuleDef {
        spec: "musi:random",
        source: include_str!("../modules/random.ms"),
    },
    FoundationModuleDef {
        spec: "musi:text",
        source: include_str!("../modules/text.ms"),
    },
    FoundationModuleDef {
        spec: "musi:json",
        source: include_str!("../modules/json_host.ms"),
    },
    FoundationModuleDef {
        spec: "musi:encoding",
        source: include_str!("../modules/encoding_host.ms"),
    },
    FoundationModuleDef {
        spec: "musi:fmt",
        source: include_str!("../modules/fmt.ms"),
    },
    FoundationModuleDef {
        spec: "musi:crypto",
        source: include_str!("../modules/crypto_host.ms"),
    },
    FoundationModuleDef {
        spec: "musi:uuid",
        source: include_str!("../modules/uuid_host.ms"),
    },
    FoundationModuleDef {
        spec: "musi:log",
        source: include_str!("../modules/log.ms"),
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
