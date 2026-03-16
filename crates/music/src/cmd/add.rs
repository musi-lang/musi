use std::fs;
use std::process;

use music_resolve::{ImportScheme, parse_specifier};
use toml_edit::DocumentMut;

pub fn run(specifier: &str, name_override: Option<&str>, dev: bool) -> ! {
    let manifest_path = "mspackage.toml";

    if !fs::exists(manifest_path).unwrap_or(false) {
        eprintln!("error: no mspackage.toml found in current directory");
        eprintln!("  run `music init` to create one");
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
            eprintln!("error: failed to read mspackage.toml: {e}");
            process::exit(1);
        }
    };

    let mut doc: DocumentMut = match content.parse() {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error: failed to parse mspackage.toml: {e}");
            process::exit(1);
        }
    };

    let dep_name = name_override.map_or_else(
        || derive_name_from_specifier(&parsed.module_path, parsed.scheme),
        ToOwned::to_owned,
    );

    let table_key = if dev {
        "dev-dependencies"
    } else {
        "dependencies"
    };

    if !doc.contains_table(table_key) {
        doc[table_key] = toml_edit::Item::Table(toml_edit::Table::new());
    }

    doc[table_key][&dep_name] = toml_edit::value(specifier);

    if let Err(e) = fs::write(manifest_path, doc.to_string()) {
        eprintln!("error: failed to write mspackage.toml: {e}");
        process::exit(1);
    }

    let section = if dev {
        "dev-dependencies"
    } else {
        "dependencies"
    };
    eprintln!(r#"added `{dep_name}` = "{specifier}" to [{section}]"#);
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
        ImportScheme::Msr | ImportScheme::Musi | ImportScheme::Bare => module_path,
    };

    String::from(name)
}
