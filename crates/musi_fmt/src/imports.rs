use std::ops::Range;

use music_syntax::{Lexer, TokenKind, parse};

use crate::protected::protected_line_ranges;

#[derive(Debug, Clone)]
struct ImportStatement {
    range: Range<usize>,
    sort_key: ImportSortKey,
    original: String,
    text: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
struct ImportSortKey {
    spec: String,
    local: String,
    original: String,
}

#[derive(Debug, Clone, Copy)]
struct TokenView<'a> {
    kind: TokenKind,
    start: usize,
    end: usize,
    text: &'a str,
}

#[derive(Debug, Clone)]
struct Replacement {
    range: Range<usize>,
    text: String,
}

#[must_use]
pub fn organize_imports(source: &str) -> Option<String> {
    let lexed = Lexer::new(source).lex();
    if !lexed.errors().is_empty() {
        return None;
    }
    let parsed = parse(lexed);
    if !parsed.errors().is_empty() {
        return None;
    }
    let protected_ranges = protected_line_ranges(source, parsed.tree());
    organize_imports_protecting(source, &protected_ranges)
}

#[must_use]
pub fn organize_imports_protecting(
    source: &str,
    protected_ranges: &[Range<usize>],
) -> Option<String> {
    let tokens = token_views(source)?;
    if tokens.is_empty() {
        return None;
    }
    let statements = collect_top_level_statements(source, &tokens);
    let replacements = import_block_replacements(source, &statements, protected_ranges);
    apply_replacements(source, &replacements)
}

fn token_views(source: &str) -> Option<Vec<TokenView<'_>>> {
    let lexed = Lexer::new(source).lex();
    if !lexed.errors().is_empty() {
        return None;
    }
    Some(
        lexed
            .tokens()
            .iter()
            .filter(|token| token.kind != TokenKind::Eof)
            .filter_map(|token| {
                let start = usize::try_from(token.span.start).ok()?;
                let end = usize::try_from(token.span.end).ok()?;
                let text = source.get(start..end)?;
                Some(TokenView {
                    kind: token.kind,
                    start,
                    end,
                    text,
                })
            })
            .collect(),
    )
}

fn collect_top_level_statements(source: &str, tokens: &[TokenView<'_>]) -> Vec<ImportStatement> {
    let mut statements = Vec::new();
    let mut start = 0usize;
    let mut paren_depth = 0usize;
    let mut bracket_depth = 0usize;
    let mut brace_depth = 0usize;

    for (index, token) in tokens.iter().enumerate() {
        match token.kind {
            TokenKind::LParen => paren_depth = paren_depth.saturating_add(1),
            TokenKind::RParen => paren_depth = paren_depth.saturating_sub(1),
            TokenKind::LBracket => bracket_depth = bracket_depth.saturating_add(1),
            TokenKind::RBracket => bracket_depth = bracket_depth.saturating_sub(1),
            TokenKind::LBrace => brace_depth = brace_depth.saturating_add(1),
            TokenKind::RBrace => brace_depth = brace_depth.saturating_sub(1),
            TokenKind::Semicolon if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0 => {
                if let Some(statement) = classify_import_statement(source, tokens, start..index + 1)
                {
                    statements.push(statement);
                }
                start = index.saturating_add(1);
            }
            _ => {}
        }
    }
    statements
}

fn classify_import_statement(
    source: &str,
    tokens: &[TokenView<'_>],
    token_range: Range<usize>,
) -> Option<ImportStatement> {
    let statement_tokens = tokens.get(token_range)?;
    if statement_tokens
        .iter()
        .any(|token| token.kind == TokenKind::KwExport)
    {
        return None;
    }
    let import_index = statement_tokens
        .iter()
        .position(|token| token.kind == TokenKind::KwImport)?;
    let spec_token = statement_tokens.get(import_index.saturating_add(1))?;
    if !matches!(
        spec_token.kind,
        TokenKind::String | TokenKind::TemplateNoSubst
    ) {
        return None;
    }

    let first = statement_tokens.first()?;
    let last = statement_tokens.last()?;
    let start = attached_statement_start(source, first.start);
    let end = last.end;
    let original = source.get(start..end)?.to_owned();
    let local = source
        .get(first.start..statement_tokens[import_index].start)
        .unwrap_or_default()
        .trim()
        .to_owned();
    let text = sort_import_destructure_fields(&original);
    Some(ImportStatement {
        range: start..end,
        sort_key: ImportSortKey {
            spec: spec_token.text.to_ascii_lowercase(),
            local: local.to_ascii_lowercase(),
            original: original.to_ascii_lowercase(),
        },
        original,
        text,
    })
}

fn attached_statement_start(source: &str, token_start: usize) -> usize {
    let Some(prefix) = source.get(..token_start) else {
        return token_start;
    };
    let line_start = prefix
        .rfind('\n')
        .map_or(0, |index| index.saturating_add(1));

    let mut attach_start = line_start;
    while let Some(previous_start) = previous_line_start(source, attach_start) {
        let previous_line = source
            .get(previous_start..attach_start)
            .unwrap_or_default()
            .trim_end_matches(['\r', '\n']);
        if is_attached_import_line_comment(previous_line) {
            attach_start = previous_start;
            continue;
        }
        if let Some(block_start) = attached_block_comment_start(source, attach_start, previous_line)
        {
            attach_start = block_start;
            continue;
        }
        if !is_attached_import_block_comment_start(previous_line) {
            break;
        }
        attach_start = previous_start;
    }
    if attach_start != line_start {
        return attach_start;
    }
    token_start
}

fn is_attached_import_line_comment(line: &str) -> bool {
    let trimmed = line.trim_start();
    trimmed.starts_with("--") && !trimmed.starts_with("--!")
}

fn is_attached_import_block_comment_start(line: &str) -> bool {
    let trimmed = line.trim_start();
    starts_with_block_comment(trimmed) && !starts_with_module_block_comment(trimmed)
}

fn attached_block_comment_start(
    source: &str,
    attach_start: usize,
    previous_line: &str,
) -> Option<usize> {
    if !previous_line.trim_end().ends_with("-/") {
        return None;
    }
    let block_start = source
        .get(..attach_start)?
        .as_bytes()
        .windows(2)
        .rposition(|window| matches!(window, [b'/', b'-']))?;
    let block_opener = source.get(block_start..block_start.saturating_add(3))?;
    (!starts_with_module_block_comment(block_opener)).then_some(block_start)
}

fn starts_with_block_comment(text: &str) -> bool {
    text.as_bytes().starts_with(b"/-")
}

fn starts_with_module_block_comment(text: &str) -> bool {
    text.as_bytes().starts_with(b"/-!")
}

fn previous_line_start(source: &str, line_start: usize) -> Option<usize> {
    if line_start == 0 {
        return None;
    }
    let before = source.get(..line_start)?.trim_end_matches(['\r', '\n']);
    Some(
        before
            .rfind('\n')
            .map_or(0, |index| index.saturating_add(1)),
    )
}

fn import_block_replacements(
    source: &str,
    statements: &[ImportStatement],
    protected_ranges: &[Range<usize>],
) -> Vec<Replacement> {
    let mut replacements = Vec::new();
    let mut block = Vec::<ImportStatement>::new();

    for statement in statements {
        if protected_ranges
            .iter()
            .any(|range| ranges_overlap(&statement.range, range))
        {
            push_sorted_block(&mut replacements, &block);
            block.clear();
            continue;
        }
        if let Some(previous) = block.last() {
            let between = source
                .get(previous.range.end..statement.range.start)
                .unwrap_or_default();
            if !between.trim().is_empty() || newline_count(between) >= 2 {
                push_sorted_block(&mut replacements, &block);
                block.clear();
            }
        }
        block.push(statement.clone());
    }
    push_sorted_block(&mut replacements, &block);
    replacements
}

const fn ranges_overlap(left: &Range<usize>, right: &Range<usize>) -> bool {
    left.start < right.end && right.start < left.end
}

fn newline_count(text: &str) -> usize {
    text.bytes().filter(|byte| *byte == b'\n').count()
}

fn push_sorted_block(replacements: &mut Vec<Replacement>, block: &[ImportStatement]) {
    if block.is_empty() {
        return;
    }
    let mut sorted = block.to_vec();
    sorted.sort_by(|left, right| left.sort_key.cmp(&right.sort_key));
    let text = sorted
        .iter()
        .map(|statement| statement.text.trim())
        .collect::<Vec<_>>()
        .join("\n");
    let range = block[0].range.start..block[block.len().saturating_sub(1)].range.end;
    let original = block
        .iter()
        .map(|statement| statement.original.trim())
        .collect::<Vec<_>>()
        .join("\n");
    if text != original {
        replacements.push(Replacement { range, text });
    }
}

fn sort_import_destructure_fields(statement: &str) -> String {
    let Some(tokens) = token_views(statement) else {
        return statement.to_owned();
    };
    let Some(colon_eq_index) = tokens
        .iter()
        .position(|token| token.kind == TokenKind::ColonEq)
    else {
        return statement.to_owned();
    };
    let Some(open_index) = tokens
        .iter()
        .take(colon_eq_index)
        .position(|token| token.kind == TokenKind::LBrace)
    else {
        return statement.to_owned();
    };
    let Some(close_index) = matching_close_brace(&tokens, open_index, colon_eq_index) else {
        return statement.to_owned();
    };
    let sorted = sort_brace_fields(statement, &tokens, open_index, close_index);
    let range = tokens[open_index].start..tokens[close_index].end;
    replace_one(statement, range, &sorted)
}

fn matching_close_brace(
    tokens: &[TokenView<'_>],
    open_index: usize,
    end_index: usize,
) -> Option<usize> {
    let mut depth = 0usize;
    for (index, token) in tokens.iter().enumerate().take(end_index).skip(open_index) {
        match token.kind {
            TokenKind::LBrace => depth = depth.saturating_add(1),
            TokenKind::RBrace => {
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

fn sort_brace_fields(
    source: &str,
    tokens: &[TokenView<'_>],
    open_index: usize,
    close_index: usize,
) -> String {
    let open = tokens[open_index];
    let close = tokens[close_index];
    let fields = collect_field_ranges(tokens, open_index, close_index);
    if fields.len() < 2 {
        return source
            .get(open.start..close.end)
            .unwrap_or_default()
            .to_owned();
    }
    if fields.iter().any(|range| {
        source
            .get(range.clone())
            .is_some_and(field_contains_comment)
    }) {
        return source
            .get(open.start..close.end)
            .unwrap_or_default()
            .to_owned();
    }

    let mut sortable = fields
        .into_iter()
        .map(|range| {
            let field_source = source.get(range).unwrap_or_default().trim();
            let field_text = sort_nested_record_fields(field_source);
            let key = field_sort_key(&field_text);
            (key, field_text)
        })
        .collect::<Vec<_>>();
    sortable.sort_by(|left, right| left.0.cmp(&right.0));
    let body = sortable
        .into_iter()
        .map(|(_, field)| field)
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{ {body} }}")
}

fn collect_field_ranges(
    tokens: &[TokenView<'_>],
    open_index: usize,
    close_index: usize,
) -> Vec<Range<usize>> {
    let mut fields = Vec::new();
    let mut start = tokens[open_index].end;
    let mut depth = 0usize;
    for token in tokens.iter().take(close_index).skip(open_index + 1) {
        match token.kind {
            TokenKind::LBrace | TokenKind::LParen | TokenKind::LBracket => {
                depth = depth.saturating_add(1);
            }
            TokenKind::RBrace | TokenKind::RParen | TokenKind::RBracket => {
                depth = depth.saturating_sub(1);
            }
            TokenKind::Comma if depth == 0 => {
                fields.push(start..token.start);
                start = token.end;
            }
            _ => {}
        }
    }
    fields.push(start..tokens[close_index].start);
    fields
        .into_iter()
        .filter(|range| range.start < range.end)
        .collect()
}

fn field_contains_comment(field: &str) -> bool {
    field
        .as_bytes()
        .windows(2)
        .any(|window| matches!(window, [b'-' | b'/', b'-']))
}

fn sort_nested_record_fields(field: &str) -> String {
    let Some(tokens) = token_views(field) else {
        return field.to_owned();
    };
    let Some(open_index) = tokens
        .iter()
        .position(|token| token.kind == TokenKind::LBrace)
    else {
        return field.to_owned();
    };
    let Some(close_index) = matching_close_brace(&tokens, open_index, tokens.len()) else {
        return field.to_owned();
    };
    let sorted = sort_brace_fields(field, &tokens, open_index, close_index);
    replace_one(
        field,
        tokens[open_index].start..tokens[close_index].end,
        &sorted,
    )
}

fn field_sort_key(field: &str) -> String {
    token_views(field)
        .unwrap_or_default()
        .into_iter()
        .find(|token| token.kind == TokenKind::Ident)
        .map_or_else(
            || field.to_ascii_lowercase(),
            |token| token.text.to_ascii_lowercase(),
        )
}

fn apply_replacements(source: &str, replacements: &[Replacement]) -> Option<String> {
    if replacements.is_empty() {
        return None;
    }
    let mut out = source.to_owned();
    for replacement in replacements.iter().rev() {
        out.replace_range(replacement.range.clone(), &replacement.text);
    }
    (out != source).then_some(out)
}

fn replace_one(source: &str, range: Range<usize>, replacement: &str) -> String {
    let mut out = source.to_owned();
    out.replace_range(range, replacement);
    out
}
