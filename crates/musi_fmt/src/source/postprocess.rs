use crate::{FormatOptions, GroupLayout, MatchArmArrowAlignment};

use super::is_let_line;

pub(super) fn enforce_bind_line_width(text: String, options: &FormatOptions) -> String {
    if options.line_width == 0 {
        return text;
    }
    let mut out = String::with_capacity(text.len());
    for line in text.lines() {
        if line.chars().count() > options.line_width
            && is_let_line(line)
            && let Some(broken) = break_long_let_signature_line(line, options)
        {
            out.push_str(&broken);
            continue;
        }
        if line.chars().count() > options.line_width
            && is_let_line(line)
            && let Some(index) = line.find(" := ")
        {
            let lhs_end = index.saturating_add(" :=".len());
            let rhs_start = index.saturating_add(" := ".len());
            let Some(lhs) = line.get(..lhs_end) else {
                out.push_str(line);
                out.push('\n');
                continue;
            };
            let Some(rhs) = line.get(rhs_start..) else {
                out.push_str(line);
                out.push('\n');
                continue;
            };
            let indent = line
                .chars()
                .take_while(|ch| ch.is_whitespace())
                .collect::<String>();
            out.push_str(lhs.trim_end());
            out.push('\n');
            out.push_str(&indent);
            out.push_str(&options.indent_unit());
            out.push_str(rhs.trim_start());
            out.push('\n');
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

fn break_long_let_signature_line(line: &str, options: &FormatOptions) -> Option<String> {
    let open = line.rfind(" (")?.saturating_add(1);
    let close = matching_close_paren(line, open)?;
    let inner = line.get(open + 1..close)?;
    if inner.trim().is_empty() || !inner.contains(',') {
        return None;
    }
    let indent = line
        .chars()
        .take_while(|ch| ch.is_whitespace())
        .collect::<String>();
    let item_indent = format!("{indent}{}", options.indent_unit());
    let mut out = String::new();
    out.push_str(line.get(..=open)?.trim_end());
    out.push('\n');
    for item in split_top_level_commas(inner) {
        out.push_str(&item_indent);
        out.push_str(item.trim());
        out.push_str(",\n");
    }
    if out.ends_with(",\n") {
        out.truncate(out.len().saturating_sub(2));
        out.push('\n');
    }
    out.push_str(&indent);
    out.push(')');
    out.push_str(line.get(close + 1..)?.trim_end());
    out.push('\n');
    Some(out)
}

fn matching_close_paren(line: &str, open: usize) -> Option<usize> {
    let mut depth = 0usize;
    for (index, ch) in line.char_indices().skip_while(|(index, _)| *index < open) {
        match ch {
            '(' => depth = depth.saturating_add(1),
            ')' => {
                depth = depth.saturating_sub(1);
                if depth == 0 {
                    return Some(index);
                }
            }
            _ => {}
        }
    }
    None
}

fn split_top_level_commas(text: &str) -> Vec<&str> {
    let mut items = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    for (index, ch) in text.char_indices() {
        match ch {
            '(' => paren_depth = paren_depth.saturating_add(1),
            ')' => paren_depth = paren_depth.saturating_sub(1),
            '[' => bracket_depth = bracket_depth.saturating_add(1),
            ']' => bracket_depth = bracket_depth.saturating_sub(1),
            ',' if paren_depth == 0 && bracket_depth == 0 => {
                if let Some(item) = text.get(start..index) {
                    items.push(item);
                }
                start = index.saturating_add(1);
            }
            _ => {}
        }
    }
    if let Some(item) = text.get(start..) {
        items.push(item);
    }
    items
}

pub(super) fn compact_record_fields(text: String, options: &FormatOptions) -> String {
    if options.record_field_layout == GroupLayout::Block {
        return text;
    }
    let mut out = Vec::new();
    let lines: Vec<&str> = text.lines().collect();
    let mut index = 0usize;
    while index < lines.len() {
        let line = lines[index];
        if !line.trim_end().ends_with('{') {
            out.push(line.to_owned());
            index = index.saturating_add(1);
            continue;
        }
        let Some((candidate, next_index)) = compact_field_block(&lines, index, options) else {
            out.push(line.to_owned());
            index = index.saturating_add(1);
            continue;
        };
        out.push(candidate);
        index = next_index;
    }
    let mut formatted = out.join("\n");
    if text.ends_with('\n') {
        formatted.push('\n');
    }
    formatted
}

fn compact_field_block(
    lines: &[&str],
    start: usize,
    options: &FormatOptions,
) -> Option<(String, usize)> {
    let line = *lines.get(start)?;
    let mut cursor = start.saturating_add(1);
    let mut fields = Vec::new();
    while let Some(field_line) = lines.get(cursor).copied() {
        let trimmed = field_line.trim();
        if trimmed == "};" {
            break;
        }
        fields.push(trimmed.to_owned());
        cursor = cursor.saturating_add(1);
    }
    if fields.is_empty() || lines.get(cursor).copied().map(str::trim) != Some("};") {
        return None;
    }
    let prefix = line.trim_end().trim_end_matches('{').trim_end();
    let candidate = compact_comma_fields(prefix, &fields)
        .or_else(|| compact_semicolon_fields(prefix, &fields))?;
    (options.line_width == 0 || candidate.chars().count() <= options.line_width)
        .then_some((candidate, cursor.saturating_add(1)))
}

fn compact_comma_fields(prefix: &str, fields: &[String]) -> Option<String> {
    let mut compacted = Vec::with_capacity(fields.len());
    for field in fields {
        if !field.ends_with(',') || field.contains('{') || field.contains('}') {
            return None;
        }
        compacted.push(field.trim_end_matches(',').to_owned());
    }
    Some(format!("{prefix} {{ {} }};", compacted.join(", ")))
}

fn compact_semicolon_fields(prefix: &str, fields: &[String]) -> Option<String> {
    let mut compacted = Vec::with_capacity(fields.len());
    for field in fields {
        if !is_simple_semicolon_field(field) {
            return None;
        }
        compacted.push(field.trim_end_matches(';').to_owned());
    }
    Some(format!("{prefix} {{ {} }};", compacted.join("; ")))
}

fn is_simple_semicolon_field(field: &str) -> bool {
    field.contains(':')
        && !field.contains(":=")
        && !field.contains('{')
        && !field.contains('}')
        && !field.starts_with('|')
        && !field.starts_with("let ")
        && !field.starts_with("law ")
        && !field.starts_with("export ")
        && !field.starts_with("native ")
        && !field.starts_with("--")
        && !field.starts_with(['/', '-'])
}

pub(super) fn align_match_arrows(text: String, options: &FormatOptions) -> String {
    match options.match_arm_arrow_alignment {
        MatchArmArrowAlignment::None => text,
        MatchArmArrowAlignment::Consecutive => {
            align_match_arrow_runs(&text, options, MatchArmArrowAlignment::Consecutive)
        }
        MatchArmArrowAlignment::Block => {
            align_match_arrow_runs(&text, options, MatchArmArrowAlignment::Block)
        }
    }
}

fn align_match_arrow_runs(
    text: &str,
    options: &FormatOptions,
    alignment: MatchArmArrowAlignment,
) -> String {
    let mut lines: Vec<String> = text.lines().map(str::to_owned).collect();
    let mut run = Vec::new();
    let mut in_match = false;
    for index in 0..lines.len() {
        let trimmed = lines[index].trim_start();
        if trimmed.starts_with("match ") && trimmed.ends_with('(') {
            in_match = true;
            run.clear();
            continue;
        }
        if in_match && trimmed == ");" {
            align_match_arrow_run(&mut lines, &run, options);
            run.clear();
            in_match = false;
            continue;
        }
        if !in_match {
            continue;
        }
        if is_match_arm_line(trimmed) {
            run.push(index);
        } else if alignment == MatchArmArrowAlignment::Consecutive {
            align_match_arrow_run(&mut lines, &run, options);
            run.clear();
        }
    }
    align_match_arrow_run(&mut lines, &run, options);
    let mut out = lines.join("\n");
    if text.ends_with('\n') {
        out.push('\n');
    }
    out
}

fn align_match_arrow_run(lines: &mut [String], run: &[usize], options: &FormatOptions) {
    if run.len() < 2 {
        return;
    }
    let Some(target) = run
        .iter()
        .filter_map(|index| match_arrow_index(lines.get(*index)?))
        .max()
    else {
        return;
    };
    let mut aligned = Vec::with_capacity(run.len());
    for index in run {
        let Some(line) = lines.get(*index) else {
            return;
        };
        let Some(arrow) = match_arrow_index(line) else {
            return;
        };
        let padding = target.saturating_sub(arrow);
        let mut next = String::with_capacity(line.len().saturating_add(padding));
        next.push_str(line.get(..arrow).unwrap_or_default().trim_end());
        for _ in 0..=padding {
            next.push(' ');
        }
        next.push_str(line.get(arrow..).unwrap_or_default());
        if options.line_width > 0 && next.chars().count() > options.line_width {
            return;
        }
        aligned.push((*index, next));
    }
    for (index, line) in aligned {
        if let Some(target_line) = lines.get_mut(index) {
            *target_line = line;
        }
    }
}

fn is_match_arm_line(trimmed: &str) -> bool {
    trimmed.starts_with("| ") && trimmed.contains("=>")
}

fn match_arrow_index(line: &str) -> Option<usize> {
    line.find("=>")
}
