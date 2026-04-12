use super::*;

/// Parses SEAM text into a validated artifact model.
///
/// # Errors
///
/// Returns [`AssemblyError`] if directives, operands, labels, references, or the final artifact
/// structure are invalid.
pub fn parse_text(text: &str) -> AssemblyResult<Artifact> {
    let mut builder = TextBuilder::new();
    let lines = text.lines().map(str::trim).collect::<Vec<_>>();
    let mut index = 0usize;

    while let Some(line) = lines.get(index).copied() {
        index = index.saturating_add(1);
        if line.is_empty() {
            continue;
        }
        if line == ".end" {
            return Err(AssemblyError::TextParseFailed("unexpected `.end`".into()));
        }
        if line.starts_with(".method ") {
            let method_lines = collect_method_lines(&lines, &mut index)?;
            builder.parse_method(line, &method_lines)?;
            continue;
        }
        builder.parse_directive(line)?;
    }

    let artifact = builder.finish();
    artifact.validate()?;
    Ok(artifact)
}

/// Validates SEAM text by parsing it and checking the resulting artifact.
///
/// # Errors
///
/// Returns [`AssemblyError`] if parsing or artifact validation fails.
pub fn validate_text(text: &str) -> AssemblyResult {
    let _ = parse_text(text)?;
    Ok(())
}

fn collect_method_lines<'text>(
    lines: &'text [&'text str],
    index: &mut usize,
) -> AssemblyResult<Vec<&'text str>> {
    let start = *index;
    while let Some(line) = lines.get(*index).copied() {
        *index = index.saturating_add(1);
        if line == ".end" {
            return Ok(lines[start..index.saturating_sub(1)].to_vec());
        }
    }
    Err(AssemblyError::TextParseFailed(
        "unterminated `.method` block".into(),
    ))
}
