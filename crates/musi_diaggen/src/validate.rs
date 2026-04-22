use std::collections::{BTreeMap, BTreeSet};

use crate::error::{DiaggenError, DiaggenResult};
use crate::model::{Catalog, Entry, MapMode, MapTarget};

pub fn validate_catalogs(catalogs: &[Catalog]) -> DiaggenResult {
    let mut owners = BTreeSet::new();
    let mut codes = BTreeMap::new();
    let mut messages = BTreeMap::new();
    for catalog in catalogs {
        if !owners.insert(catalog.owner.as_str()) {
            return Err(DiaggenError(format!(
                "diagnostics catalog owner `{}` appears more than once",
                catalog.owner
            )));
        }
        validate_catalog_shape(catalog)?;
        for entry in &catalog.entries {
            validate_entry(catalog, entry)?;
            if let Some(previous) =
                codes.insert(entry.code, format!("{}.{}", catalog.owner, entry.kind))
            {
                return Err(DiaggenError(format!(
                    "diagnostic code `{}` used by `{previous}` and `{}.{}`",
                    entry.code, catalog.owner, entry.kind
                )));
            }
            if let Some(previous) = messages.insert(
                entry.message.as_str(),
                format!("{}.{}", catalog.owner, entry.kind),
            ) {
                return Err(DiaggenError(format!(
                    "diagnostic message `{}` used by `{previous}` and `{}.{}`",
                    entry.message, catalog.owner, entry.kind
                )));
            }
        }
    }
    Ok(())
}

fn validate_catalog_shape(catalog: &Catalog) -> DiaggenResult {
    let generated =
        catalog.crate_name.is_some() || catalog.enum_name.is_some() || catalog.output.is_some();
    if generated
        && (catalog.crate_name.is_none() || catalog.enum_name.is_none() || catalog.output.is_none())
    {
        return Err(DiaggenError(format!(
            "catalog `{}` generated output needs crate, enum, and output path",
            catalog.owner
        )));
    }
    let mut kinds = BTreeSet::new();
    for entry in &catalog.entries {
        if !kinds.insert(entry.kind.as_str()) {
            return Err(DiaggenError(format!(
                "catalog `{}` contains duplicate kind `{}`",
                catalog.owner, entry.kind
            )));
        }
    }
    validate_maps(catalog, &kinds)?;
    Ok(())
}

fn validate_maps(catalog: &Catalog, kinds: &BTreeSet<&str>) -> DiaggenResult {
    let mut map_names = BTreeSet::new();
    for map in &catalog.maps {
        if !map_names.insert(map.name.as_str()) {
            return Err(DiaggenError(format!(
                "catalog `{}` contains duplicate map `{}`",
                catalog.owner, map.name
            )));
        }
        let mut patterns = BTreeSet::new();
        for case in &map.cases {
            if !patterns.insert(case.pattern.as_str()) {
                return Err(DiaggenError(format!(
                    "catalog `{}` map `{}` contains duplicate case `{}`",
                    catalog.owner, map.name, case.pattern
                )));
            }
            match (&map.mode, &case.target) {
                (MapMode::Required, MapTarget::None) => {
                    return Err(DiaggenError(format!(
                        "catalog `{}` map `{}` required case `{}` cannot target none",
                        catalog.owner, map.name, case.pattern
                    )));
                }
                (_, MapTarget::Kind(kind)) if !kinds.contains(kind.as_str()) => {
                    return Err(DiaggenError(format!(
                        "catalog `{}` map `{}` references unknown diagnostic kind `{kind}`",
                        catalog.owner, map.name
                    )));
                }
                _ => {}
            }
        }
    }
    Ok(())
}

fn validate_entry(catalog: &Catalog, entry: &Entry) -> DiaggenResult {
    validate_kind_message_category(catalog, entry)?;
    validate_text(catalog, entry, "message", &entry.message)?;
    validate_text(catalog, entry, "primary", &entry.primary)?;
    if let Some(secondary) = &entry.secondary {
        validate_text(catalog, entry, "secondary", secondary)?;
    }
    if let Some(help) = &entry.help {
        validate_text(catalog, entry, "help", help)?;
    }
    Ok(())
}

fn validate_kind_message_category(catalog: &Catalog, entry: &Entry) -> DiaggenResult {
    let Some(category) = expected_message_category(&entry.kind) else {
        return Ok(());
    };
    let first = first_message_word(&entry.message);
    if first == category {
        return Ok(());
    }
    Err(DiaggenError(format!(
        "{}.{} message must start with `{category}` to match diagnostic kind",
        catalog.owner, entry.kind
    )))
}

fn expected_message_category(kind: &str) -> Option<&'static str> {
    [
        ("Missing", "missing"),
        ("Unknown", "unknown"),
        ("Invalid", "invalid"),
        ("Unsupported", "unsupported"),
        ("Duplicate", "duplicate"),
        ("Unexpected", "unexpected"),
        ("Unnecessary", "unnecessary"),
    ]
    .into_iter()
    .find_map(|(prefix, category)| kind.starts_with(prefix).then_some(category))
}

fn first_message_word(message: &str) -> &str {
    message
        .split_whitespace()
        .next()
        .unwrap_or_default()
        .trim_matches(|ch: char| !ch.is_ascii_alphabetic())
}

fn validate_text(catalog: &Catalog, entry: &Entry, field: &str, text: &str) -> DiaggenResult {
    if text.contains(": ") {
        return Err(DiaggenError(format!(
            "{}.{} {field} contains `: `",
            catalog.owner, entry.kind
        )));
    }
    if text == "type mismatch" {
        return Err(DiaggenError(format!(
            "{}.{} {field} lacks expected and found types",
            catalog.owner, entry.kind
        )));
    }
    if has_suffix_category(text) {
        return Err(DiaggenError(format!(
            "{}.{} {field} puts diagnostic category after subject",
            catalog.owner, entry.kind
        )));
    }
    if is_bare_vague_text(text) {
        return Err(DiaggenError(format!(
            "{}.{} {field} lacks concrete subject",
            catalog.owner, entry.kind
        )));
    }
    for word in text.split(|ch: char| !ch.is_ascii_alphabetic()) {
        if matches!(word, "arg" | "attr" | "expr" | "fn") {
            return Err(DiaggenError(format!(
                "{}.{} {field} contains abbreviation `{word}`",
                catalog.owner, entry.kind
            )));
        }
    }
    Ok(())
}

fn has_suffix_category(text: &str) -> bool {
    category_suffix(text, "unknown")
        || category_suffix(text, "invalid")
        || category_suffix(text, "unsupported")
        || category_suffix(text, "missing")
        || category_suffix(text, "duplicate")
}

fn category_suffix(text: &str, category: &str) -> bool {
    let mut saw_subject = false;
    let mut previous_word = "";
    for word in text.split_whitespace() {
        let normalized = word.trim_matches(|ch: char| !ch.is_ascii_alphabetic());
        if normalized == category {
            return saw_subject && previous_word != "not";
        }
        if !normalized.is_empty() {
            saw_subject = true;
            previous_word = normalized;
        }
    }
    false
}

fn is_bare_vague_text(text: &str) -> bool {
    matches!(
        text,
        "unknown field"
            | "unknown export"
            | "unknown effect"
            | "unknown effect operation"
            | "invalid target"
            | "invalid ask target"
            | "arity mismatch"
            | "call arity mismatch"
            | "type mismatch"
    )
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
