//! Signature help: show function parameter types when inside a call.

use musi_parse::ast::{Expr, PostfixOp};
use musi_sema::Type;
use musi_shared::Idx;
use tower_lsp_server::ls_types::{
    ParameterInformation, ParameterLabel, Position, SignatureHelp, SignatureInformation,
};

use crate::analysis::{AnalyzedDoc, expr_span, position_to_offset};
use crate::hover::fmt_type;

pub fn signature_help(doc: &AnalyzedDoc, position: Position) -> Option<SignatureHelp> {
    let sema = doc.sema.as_ref()?;
    let offset = position_to_offset(&doc.source, position.line, position.character);

    // Find the innermost Call expression whose span contains the cursor.
    let mut best: Option<(Idx<Expr>, Idx<Expr>, u32)> = None; // (call_idx, base_idx, span_len)
    for (idx, expr) in doc.module.ctx.exprs.iter_idx() {
        if let Expr::Postfix {
            base,
            op: PostfixOp::Call { .. },
            span,
        } = expr
            && span.start <= offset
            && offset <= span.start + span.length
        {
            let better = best.is_none_or(|(_, _, len)| span.length < len);
            if better {
                best = Some((idx, *base, span.length));
            }
        }
    }

    let (_call_idx, base_idx, _) = best?;

    // Get the callee's type from the base expression.
    let base_ty = sema.expr_types.get(&base_idx).cloned()?;
    let resolved = sema.unify_table.resolve(base_ty);

    let (param_types, ret_ty) = match &resolved {
        Type::Arrow(params, ret) => (params.clone(), *ret.clone()),
        _ => return None,
    };

    // Try to get a callee name for a nicer label.
    let callee_name = match doc.module.ctx.exprs.get(base_idx) {
        Expr::Ident { name, .. } => Some(doc.interner.resolve(*name).to_owned()),
        Expr::Postfix {
            op: PostfixOp::Field { name, .. },
            ..
        } => Some(doc.interner.resolve(*name).to_owned()),
        _ => None,
    };

    // Try to get parameter names from the function definition.
    let param_names: Vec<Option<String>> = if let Some(def_id) = sema.expr_defs.get(&base_idx) {
        let def = &sema.defs[def_id.0 as usize];
        // Walk module items to find the FnDef with matching name.
        let mut names = Vec::new();
        let mut found = false;
        for (_, expr) in doc.module.ctx.exprs.iter_idx() {
            if let Expr::FnDef { name, params, .. } = expr
                && *name == def.name
            {
                for p in params {
                    names.push(Some(doc.interner.resolve(p.name).to_owned()));
                }
                found = true;
                break;
            }
        }
        if found {
            names
        } else {
            vec![None; param_types.len()]
        }
    } else {
        vec![None; param_types.len()]
    };

    // Build signature label and parameter info.
    let param_strs: Vec<String> = param_types
        .iter()
        .enumerate()
        .map(|(i, ty)| {
            let ty_s = fmt_type(ty, doc, sema);
            if let Some(Some(name)) = param_names.get(i) {
                format!("{name}: {ty_s}")
            } else {
                ty_s
            }
        })
        .collect();

    let ret_s = fmt_type(&ret_ty, doc, sema);
    let label = if let Some(ref name) = callee_name {
        format!("fn {}({}): {}", name, param_strs.join(", "), ret_s)
    } else {
        format!("({}) -> {}", param_strs.join(", "), ret_s)
    };

    // Build ParameterInformation with byte offsets into the label.
    let params_start = label.find('(').unwrap_or(0) + 1;
    let mut param_infos = Vec::new();
    let mut cursor = params_start;
    for (i, ps) in param_strs.iter().enumerate() {
        let start = cursor;
        let end = start + ps.len();
        param_infos.push(ParameterInformation {
            label: ParameterLabel::LabelOffsets([start as u32, end as u32]),
            documentation: None,
        });
        cursor = end;
        if i < param_strs.len() - 1 {
            cursor += 2; // ", "
        }
    }

    // Determine the active parameter by counting commas in the source
    // between the call's opening `(` and the cursor.
    let base_span = expr_span(base_idx, &doc.module);
    // The opening `(` is right after the base expression.
    let paren_start = base_span.start + base_span.length;
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

/// Count the 0-based parameter index by scanning commas at depth 0
/// between `start` and `cursor` in `source`.
fn count_active_param(source: &str, start: u32, cursor: u32) -> u32 {
    let bytes = source.as_bytes();
    let from = start as usize;
    let to = (cursor as usize).min(bytes.len());
    let mut depth = 0i32;
    let mut commas = 0u32;
    for &b in &bytes[from..to] {
        match b {
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth -= 1,
            b',' if depth <= 1 => commas += 1,
            _ => {}
        }
    }
    commas
}
