use msc_ast::expr::FieldKey;
use msc_ast::{Expr, ExprIdx, NameRef, ParsedModule};
use msc_sema::{DefId, DefInfo, SemaResult};
use msc_shared::{Idx, Span, Symbol};

use crate::analysis::doc::AnalyzedDoc;
use crate::analysis::token_util::{expr_span, find_name_token};
use crate::types::SpanIndex;

/// Build a sorted span index from name_ref_defs, pat_defs, and definition name tokens.
pub fn build_span_index(sema: &SemaResult, module: &ParsedModule) -> SpanIndex {
    let mut entries: SpanIndex = vec![];

    // 1. NameRefs with resolved DefIds
    for (raw_idx, def_id) in sema.resolution.name_ref_defs.iter().enumerate() {
        if let Some(def_id) = def_id {
            if let Some(idx) = u32::try_from(raw_idx).ok().map(Idx::from_raw) {
                let nr: &NameRef = &module.arenas.name_refs[idx];
                if nr.span != Span::DUMMY {
                    entries.push((nr.span.start, nr.span.end(), *def_id));
                }
            }
        }
    }

    // 2. pat_defs (binding sites)
    for (span, &def_id) in &sema.resolution.pat_defs {
        if *span != Span::DUMMY {
            entries.push((span.start, span.end(), def_id));
        }
    }

    // 3. DefInfo name tokens (definition-site hover)
    for def in &sema.defs {
        if def.span != Span::DUMMY {
            entries.push((def.span.start, def.span.end(), def.id));
        }
    }

    entries.sort_by_key(|&(start, end, _)| (start, end));
    entries.dedup();
    entries
}

/// Find the definition at `offset` using the span index (O(log n) binary search).
pub fn def_at_offset(offset: u32, doc: &AnalyzedDoc) -> Option<&DefInfo> {
    let sema = doc.sema.as_ref()?;
    let idx = doc
        .span_index
        .partition_point(|&(start, _, _)| start <= offset);

    let mut best: Option<(DefId, u32)> = None;
    // Search backward from partition point for spans that contain the offset.
    for &(start, end, def_id) in doc.span_index[..idx].iter().rev() {
        if end < offset {
            break;
        }
        if start <= offset && offset <= end {
            let len = end - start;
            if best.is_none_or(|(_, best_len)| len < best_len) {
                best = Some((def_id, len));
            }
        }
    }

    let (def_id, _) = best?;
    sema.defs.get(def_id.0 as usize)
}

/// Find the definition whose reference or declaration site contains `offset`.
pub fn def_at_cursor(offset: u32, doc: &AnalyzedDoc) -> Option<&DefInfo> {
    let sema = doc.sema.as_ref()?;

    // 1. expr_defs (reference sites)
    if let Some(def_id) = sema
        .resolution
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            let span = expr_span(idx, &doc.module)?;
            if span.start <= offset && offset <= span.end() {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)
    {
        return sema.defs.get(def_id.0 as usize);
    }

    // 2. pat_defs (binding sites)
    if let Some(def_id) = sema
        .resolution
        .pat_defs
        .iter()
        .filter_map(|(span, &def_id)| {
            if span.start <= offset && offset <= span.end() {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)
    {
        return sema.defs.get(def_id.0 as usize);
    }

    // 3. Definition name tokens
    sema.defs.iter().find(|def| {
        if def.span == Span::DUMMY {
            return false;
        }
        let Some(name_span) = find_name_token(&doc.lexed.tokens, def.span.start, def.name) else {
            return false;
        };
        name_span.start <= offset && offset <= name_span.end()
    })
}

/// Find a `Expr::Field` node whose field-name span contains `offset`.
///
/// Returns `(expr_idx, field_name_symbol, field_name_span)` for the tightest
/// match, allowing hover to show the field's type even when `def_at_cursor`
/// finds nothing (plain record field access has no `DefId`).
pub fn field_at_cursor(offset: u32, doc: &AnalyzedDoc) -> Option<(ExprIdx, Symbol, Span)> {
    let arenas = &doc.module.arenas;
    let mut best: Option<(ExprIdx, Symbol, Span)> = None;
    for raw_idx in 0..arenas.exprs.len() {
        let Some(idx) = u32::try_from(raw_idx).ok().map(Idx::from_raw) else {
            continue;
        };
        if let Expr::Field {
            field:
                FieldKey::Name {
                    name,
                    span: field_span,
                },
            ..
        } = &arenas.exprs[idx]
        {
            if field_span.start <= offset
                && offset <= field_span.end()
                && best
                    .as_ref()
                    .is_none_or(|(_, _, s)| field_span.length < s.length)
            {
                best = Some((idx, *name, *field_span));
            }
        }
    }
    best
}
