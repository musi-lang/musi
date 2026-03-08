use musi_ast::{AttrArg, ClassMember, Expr, LitValue, Modifier, ParsedModule};
use musi_shared::{Arena, Idx, Interner};

use crate::error::CodegenError;
use crate::intrinsics::{self, Intrinsic};
use crate::module::MethodEntry;
use crate::{FunctionEntry, Module, SymbolEntry, SymbolFlags};

use super::state::{
    EmitState, TypeInfo, VariantInfo, payload_count, push_plain_fn, resolve_type_tag, ty_name_str,
};

pub(super) fn register_fn_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::FnDef {
        attrs,
        modifiers,
        name,
        params,
        ret_ty,
        body,
        ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };

    let fn_name = interner.resolve(*name).to_owned();
    let extrin_abi: Box<str> = modifiers
        .iter()
        .find_map(|m| {
            if let Modifier::Extrin(Some(sym)) = m {
                Some(Box::from(interner.resolve(*sym)))
            } else {
                None
            }
        })
        .unwrap_or_else(|| Box::from(""));
    let is_extrin = body.is_none() || modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_)));
    let is_export = modifiers.iter().any(|m| matches!(m, Modifier::Export));
    let flags_raw: u8 = if is_extrin { SymbolFlags::NATIVE } else { 0 }
        | if is_export { SymbolFlags::EXPORT } else { 0 };

    let intrinsic_id: u16 = if is_extrin {
        attrs
            .iter()
            .find_map(|attr| {
                if interner.resolve(attr.name) != "intrinsic" {
                    return None;
                }
                attr.args.first().and_then(|arg| match arg {
                    AttrArg::Named { name: arg_name, .. } => {
                        Intrinsic::from_name(interner.resolve(*arg_name)).map(Intrinsic::id)
                    }
                    AttrArg::Value {
                        value: LitValue::Str(sym),
                        ..
                    } => Intrinsic::from_name(interner.resolve(*sym)).map(Intrinsic::id),
                    AttrArg::Value { .. } => None,
                })
            })
            .or_else(|| Intrinsic::from_name(&fn_name).map(Intrinsic::id))
            .unwrap_or(intrinsics::NONE_ID)
    } else {
        intrinsics::NONE_ID
    };

    let (link_lib, link_name) = attrs.iter().fold((None, None), |(lib, nm), attr| {
        if interner.resolve(attr.name) != "link" {
            return (lib, nm);
        }
        let mut new_lib = lib;
        let mut new_nm = nm;
        for arg in &attr.args {
            match arg {
                AttrArg::Value {
                    value: LitValue::Str(sym),
                    ..
                } => {
                    new_lib = Some(Box::from(interner.resolve(*sym).trim_matches('"')));
                }
                AttrArg::Named {
                    name: arg_name,
                    value: Some(LitValue::Str(sym)),
                    ..
                } => {
                    if interner.resolve(*arg_name) == "name" {
                        new_nm = Some(Box::from(interner.resolve(*sym).trim_matches('"')));
                    }
                }
                _ => {}
            }
        }
        (new_lib, new_nm)
    });

    let sym_idx = module.push_symbol(SymbolEntry {
        name: fn_name.clone().into_boxed_str(),
        flags: SymbolFlags::new(flags_raw),
        intrinsic_id,
        abi: extrin_abi,
        link_lib,
        link_name,
    })?;

    let param_count =
        u8::try_from(params.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;
    let unit_return = ret_ty
        .as_ref()
        .and_then(|t| ty_name_str(t, interner))
        .as_deref()
        == Some("Unit");
    let fn_idx = module.push_function(FunctionEntry {
        symbol_idx: sym_idx,
        param_count,
        local_count: 0,
        code_offset: 0,
        code_length: 0,
        unit_return,
    })?;

    let _prev = state.fn_map.insert(fn_name.clone(), fn_idx);
    if let Some(ret_name) = ret_ty.as_ref().and_then(|t| ty_name_str(t, interner)) {
        let _prev = state.fn_return_types.insert(fn_name, ret_name);
    }
    Ok(())
}

fn ensure_type_tag(state: &mut EmitState, type_name: &str) {
    if !state.type_tag_map.contains_key(type_name) {
        let tag = state.next_type_tag;
        state.next_type_tag = state.next_type_tag.saturating_add(1);
        let _prev = state.type_tag_map.insert(type_name.to_owned(), tag);
    }
}

pub(super) fn register_record(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) {
    let Expr::Record {
        name: Some(name),
        fields,
        ..
    } = exprs.get(item_idx)
    else {
        return;
    };
    let type_name = interner.resolve(*name).to_owned();
    let field_names: Vec<String> = fields
        .iter()
        .map(|f| interner.resolve(f.name).to_owned())
        .collect();
    ensure_type_tag(state, &type_name);
    let _prev = state.type_map.insert(type_name, TypeInfo { field_names });
}

pub(super) fn register_choice(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let Expr::Choice {
        name: Some(name),
        variants,
        ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };
    let type_name = interner.resolve(*name).to_owned();
    ensure_type_tag(state, &type_name);

    let max_payload = variants.iter().map(payload_count).max().unwrap_or(0);
    let total_field_count = max_payload + 1;

    for (disc, variant) in variants.iter().enumerate() {
        let v_name = interner.resolve(variant.name).to_owned();
        let payload = payload_count(variant);
        let discriminant: i64 = if let Some(musi_ast::VariantPayload::Discriminant(
            LitValue::Int(v),
        )) = &variant.payload
        {
            *v
        } else {
            i64::try_from(disc).map_err(|_| CodegenError::UnsupportedExpr)?
        };
        let _prev = state.variant_map.insert(
            v_name,
            VariantInfo {
                type_name: type_name.clone(),
                discriminant,
                payload_count: payload,
                total_field_count,
            },
        );
    }
    Ok(())
}

pub(super) fn register_class_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    state: &mut EmitState,
) {
    let Expr::ClassDef { members, .. } = exprs.get(item_idx) else {
        return;
    };
    for member in members {
        if let ClassMember::Method(method_idx) = member
            && let Expr::FnDef { name, .. } = exprs.get(*method_idx)
        {
            let name_str = interner.resolve(*name).to_owned();
            let _inserted = state.class_method_names.insert(name_str);
        }
    }
}

pub(super) fn register_given_def(
    item_idx: Idx<Expr>,
    exprs: &Arena<Expr>,
    interner: &Interner,
    module: &mut Module,
    state: &EmitState,
) -> Result<(), CodegenError> {
    let Expr::GivenDef {
        class_app, members, ..
    } = exprs.get(item_idx)
    else {
        return Ok(());
    };

    let Some(type_tag) = resolve_type_tag(class_app, interner, state) else {
        return Ok(());
    };

    let members = members.clone();
    for member in &members {
        let ClassMember::Method(method_idx) = member else {
            continue;
        };
        let fn_expr = exprs.get(*method_idx);
        let Expr::FnDef {
            name,
            params,
            body: Some(_),
            modifiers,
            ..
        } = fn_expr
        else {
            continue;
        };
        if modifiers.iter().any(|m| matches!(m, Modifier::Extrin(_))) {
            continue;
        }
        let fn_name = interner.resolve(*name).to_owned();
        let param_count =
            u8::try_from(params.len()).map_err(|_| CodegenError::ParameterCountOverflow)?;
        let fn_idx = push_plain_fn(&fn_name, param_count, module)?;
        module.method_table.push(MethodEntry {
            name: fn_name.into_boxed_str(),
            type_tag,
            fn_idx,
        });
    }
    Ok(())
}

pub(super) fn register_module_decls(
    parsed: &ParsedModule,
    interner: &Interner,
    module: &mut Module,
    state: &mut EmitState,
) -> Result<(), CodegenError> {
    let exprs = &parsed.ctx.exprs;
    for &item_idx in parsed.ctx.expr_lists.get_slice(parsed.items) {
        register_fn_def(item_idx, exprs, interner, module, state)?;
        register_record(item_idx, exprs, interner, state);
        register_choice(item_idx, exprs, interner, state)?;
        register_class_def(item_idx, exprs, interner, state);
        register_given_def(item_idx, exprs, interner, module, state)?;
    }
    Ok(())
}
