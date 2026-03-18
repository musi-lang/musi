use std::fs;
use std::process;

use msc_resolve::{ImportScheme, parse_specifier};
use serde_json::Value;

pub fn run(specifier: &str, name_override: Option<&str>, dev: bool) -> ! {
    let manifest_path = "musi.json";

    if !fs::exists(manifest_path).unwrap_or(false) {
        eprintln!("error: no musi.json found in current directory");
        eprintln!("  run `msc init` to create one");
        process::exit(1);
    }

    let parsed = match parse_specifier(specifier) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("error: invalid specifier `{specifier}`: {e}");
            process::exit(1);
        }
    };

    let content = match fs::read_to_string(manifest_path) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("error: failed to read musi.json: {e}");
            process::exit(1);
        }
    };

    let mut doc: Value = match serde_json::from_str(&content) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error: failed to parse musi.json: {e}");
            process::exit(1);
        }
    };

    let dep_name = name_override.map_or_else(
        || derive_name_from_specifier(&parsed.module_path, parsed.scheme),
        ToOwned::to_owned,
    );

    let table_key = if dev {
        "devDependencies"
    } else {
        "dependencies"
    };

    let obj = doc
        .as_object_mut()
        .expect("musi.json root must be a JSON object");

    let deps = obj
        .entry(table_key)
        .or_insert_with(|| Value::Object(serde_json::Map::new()));

    deps[&dep_name] = Value::String(specifier.to_owned());

    let serialized = match serde_json::to_string_pretty(&doc) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: failed to serialize musi.json: {e}");
            process::exit(1);
        }
    };

    if let Err(e) = fs::write(manifest_path, format!("{serialized}\n")) {
        eprintln!("error: failed to write musi.json: {e}");
        process::exit(1);
    }

    let section = if dev {
        "devDependencies"
    } else {
        "dependencies"
    };
    eprintln!(r#"added `{dep_name}` = "{specifier}" to {section}"#);
    process::exit(0)
}

fn derive_name_from_specifier(module_path: &str, scheme: ImportScheme) -> String {
    let name = match scheme {
        ImportScheme::Git => {
            let without_tag = module_path.split('@').next().unwrap_or(module_path);
            without_tag.rsplit('/').next().unwrap_or(without_tag)
        }
        ImportScheme::Relative => {
            let segment = module_path.rsplit('/').next().unwrap_or(module_path);
            segment.strip_suffix(".ms").unwrap_or(segment)
        }
        ImportScheme::Msr | ImportScheme::Musi | ImportScheme::AtStd | ImportScheme::Bare => {
            module_path
        }
    };

    String::from(name)
}
