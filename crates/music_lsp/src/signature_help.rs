//! Signature help: show function parameter types when inside a call.

use lsp_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureInformation,
};
use music_ast::{Expr, ExprIdx};
use music_sema::Type;
use music_shared::Idx;

use crate::analysis::{AnalyzedDoc, expr_span, position_to_offset};
use crate::hover::fmt_type_lsp;

pub fn signature_help(doc: &AnalyzedDoc, position: Position) -> Option<SignatureHelp> {
    let sema = doc.sema.as_ref()?;
    let offset = position_to_offset(&doc.source, position.line, position.character);

    // Find the innermost Call expression whose span contains the cursor.
    let mut best: Option<(ExprIdx, ExprIdx, u32)> = None;
    for idx in 0..doc.module.arenas.exprs.len() {
        let idx = Idx::from_raw(u32::try_from(idx).unwrap_or(0));
        if let Expr::Call { callee, span, .. } = &doc.module.arenas.exprs[idx]
            && span.start <= offset
            && offset <= span.end()
        {
            let better = best.is_none_or(|(_, _, len)| span.length < len);
            if better {
                best = Some((idx, *callee, span.length));
            }
        }
    }

    let (_call_idx, callee_idx, _) = best?;

    let callee_ty = sema.expr_types.get(&callee_idx).copied()?;

    let (param_types, ret_ty) = match &sema.types[callee_ty] {
        Type::Fn { params, ret, .. } => (params.clone(), *ret),
        _ => return None,
    };

    let callee_name = match &doc.module.arenas.exprs[callee_idx] {
        Expr::Name { name_ref, .. } => {
            let name = doc.module.arenas.name_refs[*name_ref].name;
            doc.interner.try_resolve(name).map(str::to_owned)
        }
        Expr::Field { field, .. } => {
            use music_ast::expr::FieldKey;
            match field {
                FieldKey::Name { name, .. } => doc.interner.try_resolve(*name).map(str::to_owned),
                FieldKey::Pos { .. } => None,
            }
        }
        _ => None,
    };

    let param_strs: Vec<String> = param_types
        .iter()
        .map(|&ty| fmt_type_lsp(ty, doc, sema))
        .collect();

    let ret_s = fmt_type_lsp(ret_ty, doc, sema);
    let label = if let Some(ref name) = callee_name {
        format!("let {name}({}): {ret_s}", param_strs.join(", "))
    } else {
        format!("({}) -> {ret_s}", param_strs.join(", "))
    };

    let params_start = label.find('(').unwrap_or(0) + 1;
    let mut param_infos = vec![];
    let mut cursor = params_start;
    for (i, ps) in param_strs.iter().enumerate() {
        let start = cursor;
        let end = start + ps.len();
        param_infos.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([
                u32::try_from(start).unwrap_or(0),
                u32::try_from(end).unwrap_or(0),
            ]),
            documentation: None,
        });
        cursor = end;
        if i < param_strs.len() - 1 {
            cursor += 2;
        }
    }

    let callee_span = expr_span(callee_idx, &doc.module)?;
    let paren_start = callee_span.end();
    let active_param = count_active_param(&doc.source, paren_start, offset);

    Some(SignatureHelp {
        signatures: vec![SignatureInformation {
            label,
            documentation: None,
            parameters: Some(param_infos),
            active_parameter: Some(active_param),
        }],
        active_signature: Some(0),
        active_parameter: Some(active_param),
    })
}

fn count_active_param(source: &str, start: u32, cursor: u32) -> u32 {
    let bytes = source.as_bytes();
    let from = start as usize;
    let to = (cursor as usize).min(bytes.len());
    let mut depth = 0i32;
    let mut commas = 0u32;
    for &b in bytes.get(from..to).unwrap_or(&[]) {
        match b {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth -= 1,
            b',' if depth <= 1 => commas += 1,
            _ => {}
        }
    }
    commas
}
