use super::*;

pub(super) fn source_span_text(source: &Source, span: Span) -> Option<&str> {
    let start = usize::try_from(span.start).ok()?;
    let end = usize::try_from(span.end).ok()?;
    source.text().get(start..end)
}

fn starts_with_item_doc_block(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'-', ..])
}

fn starts_with_module_doc_block(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'!', ..])
}

pub(super) fn leading_doc_text(source: &Source, span: Span) -> Option<String> {
    let (line, _) = source.line_col(span.start);
    let previous_line = line.checked_sub(1)?;
    let previous_text = source.line_text(previous_line)?.trim_start();
    if previous_text.starts_with("---") {
        return leading_line_doc_text(source, previous_line);
    }
    if previous_text.starts_with("--!") {
        return None;
    }
    if previous_text.ends_with("-/") {
        return leading_block_doc_text(source, previous_line);
    }
    None
}

fn leading_line_doc_text(source: &Source, mut line: usize) -> Option<String> {
    let mut docs = Vec::new();
    loop {
        let text = source.line_text(line)?.trim_start();
        let Some(doc_text) = text.strip_prefix("---") else {
            break;
        };
        docs.push(doc_text.trim_start().to_owned());
        if line == 1 {
            break;
        }
        line -= 1;
    }
    docs.reverse();
    (!docs.is_empty()).then(|| docs.join("\n"))
}

fn leading_block_doc_text(source: &Source, mut line: usize) -> Option<String> {
    let mut lines = Vec::new();
    loop {
        let text = source.line_text(line)?.trim_start();
        lines.push(text.to_owned());
        if starts_with_item_doc_block(text) {
            lines.reverse();
            return Some(clean_block_doc_text(&lines.join("\n"), 3));
        }
        if starts_with_module_doc_block(text) {
            return None;
        }
        if line == 1 {
            return None;
        }
        line -= 1;
    }
}

pub(super) fn module_doc_text(source: &Source) -> Option<String> {
    module_doc_range_and_text(source).map(|(_, text)| text)
}

pub(super) fn module_doc_hover(source: &Source, offset: u32) -> Option<(Span, String)> {
    let (span, text) = module_doc_range_and_text(source)?;
    span.contains(offset).then_some((span, text))
}

fn module_doc_range_and_text(source: &Source) -> Option<(Span, String)> {
    let mut line = 1;
    let mut docs = Vec::new();
    let mut start = None::<u32>;
    let mut end = None::<u32>;
    while let Some(text) = source.line_text(line) {
        let trimmed = text.trim_start();
        if trimmed.is_empty() {
            line += 1;
            continue;
        }
        if let Some(doc_text) = trimmed.strip_prefix("--!") {
            let line_span = line_span(source, line)?;
            let _ = start.get_or_insert(line_span.start);
            end = Some(line_span.end);
            docs.push(doc_text.trim_start().to_owned());
            line += 1;
            continue;
        }
        if starts_with_module_doc_block(trimmed) {
            let block_start = line_span(source, line)?.start;
            let (doc, next_line) = module_block_doc_text(source, line)?;
            let _ = start.get_or_insert(block_start);
            end = Some(line_span(source, next_line.saturating_sub(1))?.end);
            docs.push(doc);
            line = next_line;
            continue;
        }
        break;
    }
    Some((Span::new(start?, end?), docs.join("\n"))).filter(|(_, text)| !text.is_empty())
}

fn module_block_doc_text(source: &Source, start_line: usize) -> Option<(String, usize)> {
    let mut line = start_line;
    let mut lines = Vec::new();
    while let Some(text) = source.line_text(line) {
        lines.push(text.trim_start().to_owned());
        if text.contains("-/") {
            return Some((clean_block_doc_text(&lines.join("\n"), 3), line + 1));
        }
        line += 1;
    }
    None
}

fn clean_block_doc_text(text: &str, opener_len: usize) -> String {
    let without_opener = text.get(opener_len..).unwrap_or(text);
    without_opener
        .strip_suffix("-/")
        .unwrap_or(without_opener)
        .trim()
        .to_owned()
}

fn line_span(source: &Source, line: usize) -> Option<Span> {
    let text = source.line_text(line)?;
    let start = source.offset(line, 1)?;
    let end = source.offset(line, text.chars().count().saturating_add(1))?;
    Some(Span::new(start, end))
}
