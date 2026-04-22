use std::path::{Path, PathBuf};

use crate::error::{DiaggenError, DiaggenResult};
use crate::model::{Catalog, Entry, KindMap, MapCase, MapMode, MapTarget};

pub fn parse_catalog(path: &Path, text: &str) -> DiaggenResult<Catalog> {
    let mut catalog = None;
    let mut entries = Vec::new();
    let mut current = None;
    let mut current_map = None;
    for (line_index, raw_line) in text.lines().enumerate() {
        let line_no = line_index + 1;
        let line = raw_line.trim();
        if line.is_empty() || line.starts_with('#') {
            continue;
        }
        let words = line.split_whitespace().collect::<Vec<_>>();
        match words.first().copied() {
            Some("catalog") => {
                finish_map(&mut current_map, &mut catalog, path, line_no)?;
                finish_entry(&mut current, &mut entries, path, line_no)?;
                catalog = Some(parse_header(&words, path, line_no)?);
            }
            Some("diag") => {
                finish_map(&mut current_map, &mut catalog, path, line_no)?;
                finish_entry(&mut current, &mut entries, path, line_no)?;
                current = Some(parse_entry_header(&words, path, line_no)?);
            }
            Some("map") => {
                finish_entry(&mut current, &mut entries, path, line_no)?;
                finish_map(&mut current_map, &mut catalog, path, line_no)?;
                current_map = Some(parse_map_header(&words, path, line_no)?);
            }
            Some("case") => push_map_case(&mut current_map, line, path, line_no)?,
            Some("end") => finish_map(&mut current_map, &mut catalog, path, line_no)?,
            Some("message") => set_text_field(&mut current, "message", line, path, line_no)?,
            Some("primary") => set_text_field(&mut current, "primary", line, path, line_no)?,
            Some("secondary") => set_text_field(&mut current, "secondary", line, path, line_no)?,
            Some("help") => set_text_field(&mut current, "help", line, path, line_no)?,
            Some(other) => {
                return Err(DiaggenError(format!(
                    "{}:{line_no}: diagnostics directive `{other}` unsupported",
                    path.display()
                )));
            }
            None => {}
        }
    }
    finish_map(&mut current_map, &mut catalog, path, text.lines().count())?;
    finish_entry(&mut current, &mut entries, path, text.lines().count())?;
    let mut catalog = catalog.ok_or_else(|| {
        DiaggenError(format!(
            "{}: diagnostics catalog header missing",
            path.display()
        ))
    })?;
    catalog.entries = entries;
    Ok(catalog)
}

fn parse_header(words: &[&str], path: &Path, line_no: usize) -> DiaggenResult<Catalog> {
    match words {
        [_, owner] => Ok(Catalog {
            owner: (*owner).to_owned(),
            crate_name: None,
            enum_name: None,
            output: None,
            entries: Vec::new(),
            maps: Vec::new(),
        }),
        [_, owner, crate_name, enum_name, output] => Ok(Catalog {
            owner: (*owner).to_owned(),
            crate_name: Some((*crate_name).to_owned()),
            enum_name: Some((*enum_name).to_owned()),
            output: Some(PathBuf::from(output)),
            entries: Vec::new(),
            maps: Vec::new(),
        }),
        _ => Err(DiaggenError(format!(
            "{}:{line_no}: expected `catalog owner [crate enum output]`",
            path.display()
        ))),
    }
}

fn parse_map_header(words: &[&str], path: &Path, line_no: usize) -> DiaggenResult<KindMap> {
    let [_, name, source_type, mode] = words else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: expected `map name source_type option|required`",
            path.display()
        )));
    };
    let mode = match *mode {
        "required" => MapMode::Required,
        "option" => MapMode::Option,
        other => {
            return Err(DiaggenError(format!(
                "{}:{line_no}: diagnostic map mode `{other}` unsupported",
                path.display()
            )));
        }
    };
    let (source_type, source_by_value) = source_type.strip_prefix("copy:").map_or_else(
        || ((*source_type).to_owned(), false),
        |source_type| (source_type.to_owned(), true),
    );
    Ok(KindMap {
        name: (*name).to_owned(),
        source_type,
        source_by_value,
        mode,
        cases: Vec::new(),
    })
}

fn push_map_case(
    current_map: &mut Option<KindMap>,
    line: &str,
    path: &Path,
    line_no: usize,
) -> DiaggenResult {
    let map = current_map.as_mut().ok_or_else(|| {
        DiaggenError(format!(
            "{}:{line_no}: `case` must follow diagnostic map",
            path.display()
        ))
    })?;
    let case = parse_map_case(line, path, line_no)?;
    map.cases.push(case);
    Ok(())
}

fn parse_map_case(line: &str, path: &Path, line_no: usize) -> DiaggenResult<MapCase> {
    let Some(rest) = line.strip_prefix("case \"") else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: expected `case \"Pattern\" Kind|none`",
            path.display()
        )));
    };
    let Some((pattern, target)) = rest.split_once('"') else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: map case pattern missing closing quote",
            path.display()
        )));
    };
    let target = target.trim();
    if target.is_empty() {
        return Err(DiaggenError(format!(
            "{}:{line_no}: map case target missing",
            path.display()
        )));
    }
    let target = if target == "none" {
        MapTarget::None
    } else {
        MapTarget::Kind(target.to_owned())
    };
    Ok(MapCase {
        pattern: pattern.to_owned(),
        target,
    })
}

fn finish_map(
    current_map: &mut Option<KindMap>,
    catalog: &mut Option<Catalog>,
    path: &Path,
    line_no: usize,
) -> DiaggenResult {
    let Some(map) = current_map.take() else {
        return Ok(());
    };
    if map.cases.is_empty() {
        return Err(DiaggenError(format!(
            "{}:{line_no}: diagnostic map `{}` has no cases",
            path.display(),
            map.name
        )));
    }
    let catalog = catalog.as_mut().ok_or_else(|| {
        DiaggenError(format!(
            "{}:{line_no}: diagnostic map `{}` must follow catalog header",
            path.display(),
            map.name
        ))
    })?;
    catalog.maps.push(map);
    Ok(())
}

fn parse_entry_header(words: &[&str], path: &Path, line_no: usize) -> DiaggenResult<Entry> {
    let [_, kind, code] = words else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: expected `diag Kind Code`",
            path.display()
        )));
    };
    let code = code.parse::<u16>().map_err(|error| {
        DiaggenError(format!(
            "{}:{line_no}: diagnostic code `{code}` invalid (`{error}`)",
            path.display()
        ))
    })?;
    Ok(Entry {
        kind: (*kind).to_owned(),
        code,
        message: String::new(),
        primary: String::new(),
        secondary: None,
        help: None,
    })
}

fn set_text_field(
    current: &mut Option<Entry>,
    field: &str,
    line: &str,
    path: &Path,
    line_no: usize,
) -> DiaggenResult {
    let entry = current.as_mut().ok_or_else(|| {
        DiaggenError(format!(
            "{}:{line_no}: `{field}` must follow diagnostic entry",
            path.display()
        ))
    })?;
    let value = parse_quoted(line, field, path, line_no)?;
    match field {
        "message" => entry.message = value,
        "primary" => entry.primary = value,
        "secondary" => entry.secondary = Some(value),
        "help" => entry.help = Some(value),
        other => {
            return Err(DiaggenError(format!("text field `{other}` unsupported")));
        }
    }
    Ok(())
}

fn parse_quoted(line: &str, field: &str, path: &Path, line_no: usize) -> DiaggenResult<String> {
    let prefix = format!("{field} \"");
    let Some(rest) = line.strip_prefix(&prefix) else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: expected `{field} \"...\"`",
            path.display()
        )));
    };
    let Some(raw) = rest.strip_suffix('"') else {
        return Err(DiaggenError(format!(
            "{}:{line_no}: `{field}` string missing closing quote",
            path.display()
        )));
    };
    unescape(raw, path, line_no)
}

fn unescape(raw: &str, path: &Path, line_no: usize) -> DiaggenResult<String> {
    let mut out = String::new();
    let mut chars = raw.chars();
    while let Some(ch) = chars.next() {
        if ch == '\\' {
            let Some(next) = chars.next() else {
                return Err(DiaggenError(format!(
                    "{}:{line_no}: dangling string escape",
                    path.display()
                )));
            };
            match next {
                '\\' => out.push('\\'),
                '"' => out.push('"'),
                'n' => out.push('\n'),
                other => {
                    return Err(DiaggenError(format!(
                        "{}:{line_no}: string escape `\\{other}` unsupported",
                        path.display()
                    )));
                }
            }
        } else {
            out.push(ch);
        }
    }
    Ok(out)
}

fn finish_entry(
    current: &mut Option<Entry>,
    entries: &mut Vec<Entry>,
    path: &Path,
    line_no: usize,
) -> DiaggenResult {
    if let Some(mut entry) = current.take() {
        if entry.message.is_empty() {
            return Err(DiaggenError(format!(
                "{}:{line_no}: diagnostic `{}` missing message",
                path.display(),
                entry.kind
            )));
        }
        if entry.primary.is_empty() {
            entry.primary.clone_from(&entry.message);
        }
        entries.push(entry);
    }
    Ok(())
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
