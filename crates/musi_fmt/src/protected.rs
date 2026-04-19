use std::ops::Range;

use music_syntax::SyntaxTree;

pub fn protected_line_ranges(source: &str, tree: &SyntaxTree) -> Vec<Range<usize>> {
    let lines = source_line_ranges(source);
    let mut protected = Vec::new();
    let mut in_range = false;
    for (index, line_range) in lines.iter().enumerate() {
        let line = source.get(line_range.clone()).unwrap_or_default();
        if line.contains("musi-fmt-ignore-start") {
            in_range = true;
            protected.push(line_range.clone());
            continue;
        }
        if in_range {
            protected.push(line_range.clone());
            if line.contains("musi-fmt-ignore-end") {
                in_range = false;
            }
            continue;
        }
        if line.contains("musi-fmt-ignore")
            && !line.contains("musi-fmt-ignore-file")
            && !line.contains("musi-fmt-ignore-end")
        {
            if let Some(next_range) = next_syntax_item_lines(&lines, tree, line_range.end)
                .or_else(|| next_source_line(&lines, source, index.saturating_add(1)))
            {
                protected.push(next_range);
            }
        }
    }
    merge_ranges(protected)
}

fn next_syntax_item_lines(
    lines: &[Range<usize>],
    tree: &SyntaxTree,
    after: usize,
) -> Option<Range<usize>> {
    let after = u32::try_from(after).ok()?;
    let item = tree
        .root()
        .children()
        .find(|item| item.span().start >= after)?;
    let start = usize::try_from(item.span().start).ok()?;
    let end = usize::try_from(item.span().end).ok()?;
    line_range_covering_span(lines, start, end)
}

fn line_range_covering_span(
    lines: &[Range<usize>],
    start: usize,
    end: usize,
) -> Option<Range<usize>> {
    let mut selected_start = None;
    let mut selected_end = None;
    for line in lines {
        if line.end <= start {
            continue;
        }
        if line.start > end {
            break;
        }
        if selected_start.is_none() {
            selected_start = Some(line.start);
        }
        selected_end = Some(line.end);
    }
    Some(selected_start?..selected_end?)
}

fn source_line_ranges(source: &str) -> Vec<Range<usize>> {
    let mut ranges = Vec::new();
    let mut start = 0;
    for (index, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            let end = index.saturating_add(1);
            ranges.push(start..end);
            start = end;
        }
    }
    if start < source.len() {
        ranges.push(start..source.len());
    }
    ranges
}

fn next_source_line(lines: &[Range<usize>], source: &str, start: usize) -> Option<Range<usize>> {
    lines.iter().skip(start).find_map(|range| {
        let line = source.get(range.clone())?;
        let trimmed = line.trim();
        (!trimmed.is_empty() && !trimmed.starts_with("--")).then(|| range.clone())
    })
}

fn merge_ranges(mut ranges: Vec<Range<usize>>) -> Vec<Range<usize>> {
    ranges.sort_by_key(|range| range.start);
    let mut merged: Vec<Range<usize>> = Vec::new();
    for range in ranges {
        if let Some(last) = merged.last_mut()
            && range.start <= last.end
        {
            last.end = last.end.max(range.end);
            continue;
        }
        merged.push(range);
    }
    merged
}
