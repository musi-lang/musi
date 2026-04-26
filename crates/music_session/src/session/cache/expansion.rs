use std::collections::BTreeMap;

use music_module::{ModuleKey, ModuleSpecifier};

use crate::session::Session;

pub(super) struct ModuleExpansionContext {
    pub(super) local_syntax: BTreeMap<String, String>,
    pub(super) imported_syntax: BTreeMap<String, BTreeMap<String, String>>,
    pub(super) local_factories: BTreeMap<String, SyntaxFactory>,
    pub(super) imported_factories: BTreeMap<String, BTreeMap<String, SyntaxFactory>>,
}

#[derive(Clone)]
pub(super) struct SyntaxFactory {
    params: Box<[String]>,
    body: String,
}

pub(super) fn expand_comptime_module_quote_pass(
    source: &str,
    context: &ModuleExpansionContext,
) -> (String, bool) {
    let mut out = String::with_capacity(source.len());
    let mut cursor = 0;
    let mut changed = false;
    while let Some(relative_start) = source_tail(source, cursor).find("known") {
        let start = cursor + relative_start;
        if !is_token_start_boundary(source, start)
            || !is_token_end_boundary(source, start.saturating_add("known".len()))
            || !is_module_statement_start(source, start)
        {
            out.push_str(source_range(source, cursor, start + "known".len()));
            cursor = start + "known".len();
            continue;
        }
        let Some(expansion) = parse_comptime_module_expansion(source, start, context) else {
            out.push_str(source_range(source, cursor, start + "known".len()));
            cursor = start + "known".len();
            continue;
        };
        out.push_str(source_range(source, cursor, start));
        out.push_str(expansion.body.trim());
        out.push('\n');
        cursor = expansion.end;
        changed = true;
    }
    out.push_str(source_tail(source, cursor));
    (out, changed)
}

struct ModuleExpansion {
    body: String,
    end: usize,
}

fn parse_comptime_module_expansion(
    source: &str,
    start: usize,
    context: &ModuleExpansionContext,
) -> Option<ModuleExpansion> {
    if let Some(expansion) = parse_comptime_quote_block(source, start) {
        return Some(expansion);
    }
    parse_comptime_syntax_name(source, start, context)
}

fn parse_comptime_quote_block(source: &str, start: usize) -> Option<ModuleExpansion> {
    let mut index = skip_ws(source, start + "known".len());
    if !source_tail(source, index).starts_with("quote") {
        return None;
    }
    index = skip_ws(source, index + "quote".len());
    let open = source_tail(source, index).chars().next()?;
    if open != '{' {
        return None;
    }
    let body_start = index + open.len_utf8();
    let close = find_matching_brace(source, index)?;
    let mut end = skip_ws(source, close + 1);
    if source_tail(source, end).starts_with(';') {
        end += 1;
    }
    Some(ModuleExpansion {
        body: source_range(source, body_start, close).to_owned(),
        end,
    })
}

fn parse_comptime_syntax_name(
    source: &str,
    start: usize,
    context: &ModuleExpansionContext,
) -> Option<ModuleExpansion> {
    let expr_start = skip_ws(source, start + "known".len());
    let semicolon = find_statement_semicolon(source, expr_start)?;
    let expr = source_range(source, expr_start, semicolon).trim();
    let body = context
        .local_syntax
        .get(expr)
        .cloned()
        .or_else(|| imported_syntax_body(context, expr))
        .or_else(|| syntax_factory_body(context, expr))?;
    Some(ModuleExpansion {
        body,
        end: semicolon + 1,
    })
}

fn imported_syntax_body(context: &ModuleExpansionContext, expr: &str) -> Option<String> {
    let (alias, name) = expr.split_once('.')?;
    if !is_ident(alias) || !is_ident(name) {
        return None;
    }
    context.imported_syntax.get(alias)?.get(name).cloned()
}

fn syntax_factory_body(context: &ModuleExpansionContext, expr: &str) -> Option<String> {
    let call = parse_call_expr_text(expr)?;
    let factory = call.import.as_ref().map_or_else(
        || context.local_factories.get(&call.name),
        |alias| context.imported_factories.get(alias)?.get(&call.name),
    )?;
    render_syntax_factory(factory, &call.args)
}

struct ModuleCallExpr {
    import: Option<String>,
    name: String,
    args: Box<[String]>,
}

fn parse_call_expr_text(expr: &str) -> Option<ModuleCallExpr> {
    let open = expr.find('(')?;
    let close = expr.rfind(')')?;
    if close + 1 != expr.len() {
        return None;
    }
    let callee = source_range(expr, 0, open).trim();
    let (import, name) = callee
        .split_once('.')
        .map_or((None, callee), |(alias, name)| (Some(alias), name));
    if !is_ident(name) || import.is_some_and(|alias| !is_ident(alias)) {
        return None;
    }
    let args = split_arg_text(source_range(expr, open + 1, close));
    Some(ModuleCallExpr {
        import: import.map(ToOwned::to_owned),
        name: name.to_owned(),
        args: args.into_boxed_slice(),
    })
}

fn render_syntax_factory(factory: &SyntaxFactory, args: &[String]) -> Option<String> {
    if factory.params.len() != args.len() {
        return None;
    }
    let mut body = factory.body.clone();
    for (param, arg) in factory.params.iter().zip(args) {
        body = body.replace(&format!("#({param})"), arg);
    }
    Some(body)
}

pub(super) fn collect_syntax_bindings(
    source: &str,
    exported_only: bool,
) -> BTreeMap<String, String> {
    let mut bindings = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        if exported_only && !has_export_prefix(source, start) {
            continue;
        }
        let Some((name, value_start)) = parse_let_value_start(source, cursor) else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(expansion) = parse_comptime_quote_block(source, value_start) else {
            continue;
        };
        let _ = bindings.insert(name, expansion.body);
    }
    bindings
}

pub(super) fn collect_syntax_factories(
    source: &str,
    exported_only: bool,
) -> BTreeMap<String, SyntaxFactory> {
    let mut factories = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        if exported_only && !has_export_prefix(source, start) {
            continue;
        }
        let Some((name, params, value_start)) = parse_let_callable_value_start(source, cursor)
        else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(expansion) = parse_comptime_quote_block(source, value_start) else {
            continue;
        };
        let _ = factories.insert(
            name,
            SyntaxFactory {
                params: params.into_boxed_slice(),
                body: expansion.body,
            },
        );
    }
    factories
}

pub(super) fn collect_import_aliases(
    session: &Session,
    key: &ModuleKey,
    source: &str,
) -> BTreeMap<String, ModuleKey> {
    let mut aliases = BTreeMap::new();
    let mut cursor = 0;
    while let Some(relative_start) = source_tail(source, cursor).find("let") {
        let start = cursor + relative_start;
        cursor = start + "let".len();
        if !is_token_start_boundary(source, start) || !is_token_end_boundary(source, cursor) {
            continue;
        }
        let Some((name, value_start)) = parse_let_value_start(source, cursor) else {
            continue;
        };
        let value_start = skip_ws(source, value_start);
        let Some(spec) = parse_import_spec(source, value_start) else {
            continue;
        };
        let target = session
            .options
            .import_map
            .resolve(key, &ModuleSpecifier::new(spec.as_str()))
            .map_or_else(
                || ModuleKey::new(spec.as_str()),
                |resolved| ModuleKey::new(resolved.as_str()),
            );
        let _ = aliases.insert(name, target);
    }
    aliases
}

fn parse_let_value_start(source: &str, mut index: usize) -> Option<(String, usize)> {
    index = skip_ws(source, index);
    let (name, next) = parse_ident_at(source, index)?;
    let value_marker = source_tail(source, next).find(":=")?;
    Some((name, next + value_marker + ":=".len()))
}

fn parse_let_callable_value_start(
    source: &str,
    mut index: usize,
) -> Option<(String, Vec<String>, usize)> {
    index = skip_ws(source, index);
    let (name, next) = parse_ident_at(source, index)?;
    let open = skip_ws(source, next);
    if !source_tail(source, open).starts_with('(') {
        return None;
    }
    let close = find_matching_paren(source, open)?;
    let params = parse_param_names(source_range(source, open + 1, close));
    let value_marker = source_tail(source, close + 1).find(":=")?;
    Some((name, params, close + 1 + value_marker + ":=".len()))
}

fn parse_param_names(text: &str) -> Vec<String> {
    split_arg_text(text)
        .into_iter()
        .filter_map(|param| {
            let trimmed = param.trim();
            let after_known = trimmed.strip_prefix("known").map_or(trimmed, str::trim);
            let name = after_known.split(':').next()?.trim();
            is_ident(name).then(|| name.to_owned())
        })
        .collect()
}

fn split_arg_text(text: &str) -> Vec<String> {
    let mut args = Vec::new();
    let mut start = 0;
    let mut depth = 0_u32;
    let mut index = 0;
    while let Some(ch) = source_tail(text, index).chars().next() {
        match ch {
            '(' | '[' | '{' => depth = depth.saturating_add(1),
            ')' | ']' | '}' => depth = depth.saturating_sub(1),
            '"' => {
                if let Some(end) = skip_string(text, index) {
                    index = end;
                }
            }
            ',' if depth == 0 => {
                args.push(source_range(text, start, index).trim().to_owned());
                start = index + ch.len_utf8();
            }
            _ => {}
        }
        index += ch.len_utf8();
    }
    let tail = source_tail(text, start).trim();
    if !tail.is_empty() {
        args.push(tail.to_owned());
    }
    args
}

fn parse_import_spec(source: &str, start: usize) -> Option<String> {
    let mut index = start;
    if !source_tail(source, index).starts_with("import") {
        return None;
    }
    index = skip_ws(source, index + "import".len());
    let text = source_tail(source, index);
    let inner = text.strip_prefix('"')?;
    let mut spec = String::new();
    let mut escaped = false;
    for ch in inner.chars() {
        if escaped {
            spec.push(ch);
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == '"' {
            return Some(spec);
        } else {
            spec.push(ch);
        }
    }
    None
}

fn find_matching_paren(source: &str, open: usize) -> Option<usize> {
    find_matching_delimiter(source, open, '(', ')')
}

fn parse_ident_at(source: &str, start: usize) -> Option<(String, usize)> {
    let mut chars = source_tail(source, start).char_indices();
    let (_, first) = chars.next()?;
    if !is_ident_start(first) {
        return None;
    }
    let mut end = start + first.len_utf8();
    for (offset, ch) in chars {
        if !is_ident_char(ch) {
            return Some((source_range(source, start, end).to_owned(), start + offset));
        }
        end = start + offset + ch.len_utf8();
    }
    Some((source_tail(source, start).to_owned(), source.len()))
}

fn find_statement_semicolon(source: &str, start: usize) -> Option<usize> {
    let mut index = start;
    while let Some(ch) = source_tail(source, index).chars().next() {
        match ch {
            ';' => return Some(index),
            '"' => index = skip_string(source, index)?,
            _ => {}
        }
        index += ch.len_utf8();
    }
    None
}

fn skip_ws(source: &str, mut index: usize) -> usize {
    while let Some(ch) = source_tail(source, index).chars().next() {
        if !ch.is_whitespace() {
            break;
        }
        index += ch.len_utf8();
    }
    index
}

fn find_matching_brace(source: &str, open: usize) -> Option<usize> {
    find_matching_delimiter(source, open, '{', '}')
}

fn find_matching_delimiter(source: &str, open: usize, left: char, right: char) -> Option<usize> {
    let mut depth = 0_u32;
    let mut index = open;
    while let Some(ch) = source_tail(source, index).chars().next() {
        if ch == left {
            depth = depth.saturating_add(1);
        } else if ch == right {
            depth = depth.saturating_sub(1);
            if depth == 0 {
                return Some(index);
            }
        } else if ch == '"' {
            index = skip_string(source, index)?;
        }
        index += ch.len_utf8();
    }
    None
}

fn skip_string(source: &str, start: usize) -> Option<usize> {
    let mut escaped = false;
    let mut index = start + 1;
    while let Some(ch) = source_tail(source, index).chars().next() {
        if escaped {
            escaped = false;
        } else if ch == '\\' {
            escaped = true;
        } else if ch == '"' {
            return Some(index);
        }
        index += ch.len_utf8();
    }
    None
}

fn is_token_start_boundary(source: &str, index: usize) -> bool {
    source_head(source, index)
        .chars()
        .next_back()
        .is_none_or(|ch| !is_ident_char(ch))
}

fn is_module_statement_start(source: &str, index: usize) -> bool {
    source_head(source, index)
        .chars()
        .rev()
        .find(|ch| !ch.is_whitespace())
        .is_none_or(|ch| matches!(ch, ';' | '{' | '}'))
}

fn has_export_prefix(source: &str, index: usize) -> bool {
    let before = source_head(source, index).trim_end();
    let Some(export_start) = before.rfind("export") else {
        return false;
    };
    source_tail(before, export_start).trim() == "export"
        && is_token_start_boundary(before, export_start)
        && is_token_end_boundary(before, export_start + "export".len())
}

fn source_head(source: &str, end: usize) -> &str {
    source.get(..end).unwrap_or_default()
}

fn source_tail(source: &str, start: usize) -> &str {
    source.get(start..).unwrap_or_default()
}

fn source_range(source: &str, start: usize, end: usize) -> &str {
    source.get(start..end).unwrap_or_default()
}

fn is_token_end_boundary(source: &str, index: usize) -> bool {
    source
        .get(index..)
        .and_then(|text| text.chars().next())
        .is_none_or(|ch| !is_ident_char(ch))
}

const fn is_ident_char(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | 'A'..='Z' | '0'..='9')
}

const fn is_ident_start(ch: char) -> bool {
    matches!(ch, '_' | 'a'..='z' | 'A'..='Z')
}

fn is_ident(text: &str) -> bool {
    let mut chars = text.chars();
    chars.next().is_some_and(is_ident_start) && chars.all(is_ident_char)
}
