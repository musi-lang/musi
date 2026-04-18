use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

use crate::error::{DiaggenError, DiaggenResult, io_error};
use crate::model::Catalog;
use crate::parser::parse_catalog;
use crate::render::render_catalog;
use crate::validate::validate_catalogs;

const CATALOG_DIR: &str = "diagnostics";

pub fn run_from_env() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> DiaggenResult {
    let mut args = env::args().skip(1);
    let command = args.next().unwrap_or_else(|| "check".to_owned());
    let catalogs = load_catalogs(Path::new(CATALOG_DIR))?;
    validate_catalogs(&catalogs)?;
    match command.as_str() {
        "check" => check_generated(&catalogs),
        "write" => write_generated(&catalogs),
        _ => Err(DiaggenError(format!(
            "diagnostics command `{command}` unsupported"
        ))),
    }
}

fn load_catalogs(dir: &Path) -> DiaggenResult<Vec<Catalog>> {
    let mut paths = Vec::new();
    for entry in fs::read_dir(dir).map_err(io_error("read diagnostics catalog directory"))? {
        let path = entry
            .map_err(io_error("read diagnostics catalog entry"))?
            .path();
        if path.extension().and_then(|value| value.to_str()) == Some("def") {
            paths.push(path);
        }
    }
    paths.sort();
    let mut catalogs = Vec::new();
    for path in paths {
        let text = fs::read_to_string(&path).map_err(io_error("read diagnostics catalog"))?;
        catalogs.push(parse_catalog(&path, &text)?);
    }
    Ok(catalogs)
}

fn write_generated(catalogs: &[Catalog]) -> DiaggenResult {
    for catalog in catalogs.iter().filter(|catalog| catalog.output.is_some()) {
        let Some(output) = &catalog.output else {
            continue;
        };
        let generated = render_catalog(catalog)?;
        fs::write(output, generated).map_err(io_error("write generated diagnostics catalog"))?;
    }
    Ok(())
}

fn check_generated(catalogs: &[Catalog]) -> DiaggenResult {
    for catalog in catalogs.iter().filter(|catalog| catalog.output.is_some()) {
        let Some(output) = &catalog.output else {
            continue;
        };
        let generated = render_catalog(catalog)?;
        let current =
            fs::read_to_string(output).map_err(io_error("read generated diagnostics catalog"))?;
        if current != generated {
            return Err(DiaggenError(format!(
                "{} is out of date; run `cargo run -p musi_diaggen -- write`",
                output.display()
            )));
        }
    }
    Ok(())
}
