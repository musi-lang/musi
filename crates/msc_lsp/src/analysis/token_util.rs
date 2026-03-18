use msc_ast::{Expr, ExprIdx, ParsedModule};
use msc_lex::{Token, TokenKind};
use msc_sema::DefInfo;
use msc_shared::{Span, Symbol};

/// Returns the span of any expression node, or `None` if the index is out of bounds.
pub fn expr_span(idx: ExprIdx, module: &ParsedModule) -> Option<Span> {
    if idx.raw() as usize >= module.arenas.exprs.len() {
        return None;
    }
    let span = match &module.arenas.exprs[idx] {
        Expr::Lit { span, .. }
        | Expr::Name { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Let { span, .. }
        | Expr::Fn { span, .. }
        | Expr::Call { span, .. }
        | Expr::Field { span, .. }
        | Expr::Index { span, .. }
        | Expr::Update { span, .. }
        | Expr::Record { span, .. }
        | Expr::Array { span, .. }
        | Expr::Variant { span, .. }
        | Expr::Choice { span, .. }
        | Expr::RecordDef { span, .. }
        | Expr::BinOp { span, .. }
        | Expr::UnaryOp { span, .. }
        | Expr::Piecewise { span, .. }
        | Expr::Match { span, .. }
        | Expr::Return { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Annotated { span, .. }
        | Expr::Binding { span, .. }
        | Expr::Class { span, .. }
        | Expr::Instance { span, .. }
        | Expr::Effect { span, .. }
        | Expr::Foreign { span, .. }
        | Expr::TypeCheck { span, .. }
        | Expr::Handle { span, .. }
        | Expr::Error { span, .. }
        | Expr::TypeApp { span, .. }
        | Expr::FnType { span, .. }
        | Expr::OptionType { span, .. }
        | Expr::ProductType { span, .. }
        | Expr::SumType { span, .. }
        | Expr::ArrayType { span, .. }
        | Expr::PiType { span, .. }
        | Expr::Need { span, .. }
        | Expr::Resume { span, .. } => *span,
    };
    Some(span)
}

/// Find the Ident token at or after `start` whose interned symbol equals `name`.
pub fn find_name_token(tokens: &[Token], start: u32, name: Symbol) -> Option<Span> {
    let idx = tokens.partition_point(|t| t.span.start < start);
    for tok in tokens[idx..].iter().take(64) {
        if tok.span.start > start.saturating_add(200) {
            break;
        }
        if tok.kind == TokenKind::Ident && tok.symbol == Some(name) {
            return Some(tok.span);
        }
    }
    None
}

/// The span of a definition's name token (narrower than the full `def.span`).
pub fn def_name_span(def: &DefInfo, tokens: &[Token]) -> Span {
    find_name_token(tokens, def.span.start, def.name).unwrap_or(def.span)
}

/// Extract doc-comment text for the definition at `def_start` by scanning
/// source lines directly. Walks backward from the line containing `def_start`,
/// collecting consecutive `///` lines.
pub fn extract_doc_comments_from_source(def_start: u32, source: &str) -> String {
    let prefix = source.get(..def_start as usize).unwrap_or("");
    let def_line_start = prefix.rfind('\n').map_or(0, |i| i + 1);

    let mut doc_lines: Vec<&str> = vec![];
    let mut pos = def_line_start;

    loop {
        if pos == 0 {
            break;
        }
        let prev_end = pos - 1;
        let before = source.get(..prev_end).unwrap_or("");
        let prev_start = before.rfind('\n').map_or(0, |i| i + 1);
        let line = source.get(prev_start..prev_end).unwrap_or("");
        let trimmed = line.trim_start();

        if let Some(rest) = trimmed.strip_prefix("///") {
            let content = rest.strip_prefix(' ').unwrap_or(rest);
            doc_lines.push(content);
            pos = prev_start;
        } else if trimmed.starts_with("#[") || trimmed.is_empty() {
            // Skip attribute lines and blank lines between doc comments and the definition.
            pos = prev_start;
        } else {
            break;
        }
    }

    doc_lines.reverse();
    doc_lines.join("\n")
}
